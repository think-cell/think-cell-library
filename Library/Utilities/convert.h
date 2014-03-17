#pragma once

#include "Library/Utilities/Range/transform.h"

#include <boost/utility/enable_if.hpp>
#include <boost/mpl/or.hpp>
#include <boost/mpl/not.hpp>
#include <boost/mpl/and.hpp>

#include <type_traits>
#include <string>
#include <vector> 

//////////////////////////////////////////////////////////////////
// generic initialization and assignment between different types
//
// Copy/move/assignment cannot be overloaded for third party types
// or builtins.
//
// If Convert<T, S> is used to convert a builtin numeric type S into
// another builtin numeric type T, rounding and range checks are
// applied.
//
// For other types, the default behavior of Convert is
//   A a(Convert<A>(b))                       is equivalent to   A(a) ,
//   a=Convert<remove_reference<A>::type>(b)  is equivalent to   a=b ,
// i.e., the argument b is forwarded to the copy/move/assignment operators
// of A.
//
// Specialize SConversions if additional conversions for specific target
// types are required.

enum ETypeCategory {
	etypecatIntegral,
	etypecatFloatingPoint,
	etypecatEnum,
	etypecatReference,
	etypecatPointer,
	etypecatClass
};

template< typename T, typename Enable=void >
struct TypeCategory;

template< typename T >
struct TypeCategory<T, typename boost::enable_if<typename std::is_integral<T>::type>::type > : std::integral_constant< ETypeCategory, etypecatIntegral > {};

template< typename T >
struct TypeCategory<T, typename boost::enable_if<typename std::is_floating_point<T>::type>::type > : std::integral_constant< ETypeCategory, etypecatFloatingPoint > {};

template< typename T >
struct TypeCategory<T, typename boost::enable_if<typename std::is_enum<T>::type>::type > : std::integral_constant< ETypeCategory, etypecatEnum > {};

template< typename T >
struct TypeCategory<T, typename boost::enable_if<typename std::is_reference<T>::type>::type > : std::integral_constant< ETypeCategory, etypecatReference > {};

template< typename T >
struct TypeCategory<T, typename boost::enable_if<typename std::is_pointer<T>::type>::type > : std::integral_constant< ETypeCategory, etypecatPointer > {};

template< typename T >
struct TypeCategory<T, typename boost::enable_if<typename std::is_class<T>::type>::type > : std::integral_constant< ETypeCategory, etypecatClass > {};

template<typename TTarget,
	ETypeCategory=TypeCategory<TTarget>::value
> struct SDefaultConversions;

template<typename TTarget>
struct SConversions {};

struct SUseDefaultConversions {}; // to control InternalConvert overload selection
struct SUseConversions : SUseDefaultConversions {}; // to control InternalConvert overload selection

template<typename TTarget, typename TSource>
auto InternalConvert( TSource&& src, SUseDefaultConversions )
	return_decltype( SDefaultConversions<TTarget>() (std::forward<TSource>(src)) )

template<typename TTarget, typename TSource>
auto InternalConvert( TSource&& src, SUseConversions )
	return_decltype( SConversions<TTarget>() (std::forward<TSource>(src)) )

template<typename TTarget, typename TSource>
auto Convert(TSource&& src) return_decltype(
	InternalConvert<TTarget>(std::forward<TSource>(src), /*prefer SConversions<TTarget> if applicable*/ SUseConversions())
)

DEFINE_FN_TMPL( Convert, (typename) );

///////////////////////////////////////////////
// default conversions

template< typename T >
std::streamoff ConvertToUnderlying( std::fpos<T> const& pos ) {
	return boost::implicit_cast<std::streamoff>(pos);
}

template<typename TTarget>
struct SDefaultConversions<TTarget, etypecatClass> {
	template<typename TSource>
	TSource&& operator() (TSource&& src) const {
		return std::forward<TSource>(src);
	}
};

// if target is a reference, disallow initializations which involve nontrivial conversions
template<typename TTarget>
struct SDefaultConversions<TTarget, etypecatReference> {
	template<typename TSource>
	typename boost::enable_if<
		tc::creates_no_reference_to_temporary<TSource, TTarget>,
		TTarget
	>::type operator() (TSource&& src) const {
		return boost::implicit_cast<TTarget>(src); // static_cast if needed?
	}
};

// apply rounding and range checks when converting to a builtin arithmetic type
#include <boost/numeric/conversion/is_subranged.hpp>
#include <boost/numeric/conversion/converter.hpp>

template<typename TTarget, typename TSource>
struct range_checker {
	typedef typename boost::numeric::convdetail::GetRC<
		boost::numeric::conversion_traits<TTarget, TSource>,
		boost::numeric::silent_overflow_handler, // only use type::out_of_range(src) result for our asserts
		boost::numeric::Trunc< TTarget > // corresponds to builtin conversion from float to integer
	>::type type;
};

template<typename TTarget>
struct SDefaultConversions<TTarget, etypecatIntegral> {
	template<typename TSource>
	typename boost::enable_if<
		boost::mpl::and_<
			boost::mpl::or_<
				std::is_class< typename std::remove_reference< TSource >::type >,
				std::is_union< typename std::remove_reference< TSource >::type >
			>,
			std::is_convertible< TSource&&, TTarget >
		>,
		TTarget
	>::type operator() (TSource&& src) const {
		// use specific Convert function defined by source type
		return boost::implicit_cast<TTarget>(std::forward<TSource>(src));
	};

	template<typename TSource>
	typename boost::enable_if<
		boost::mpl::and_<
			boost::mpl::or_<
				std::is_class< typename std::remove_reference< TSource >::type >,
				std::is_union< typename std::remove_reference< TSource >::type >
			>,
			boost::mpl::not_< std::is_convertible< TSource&&, TTarget > >
		>,
		TTarget
	>::type operator() (TSource&& src) const {
		// use specific Convert function defined by source type
		return (*this)( ConvertToUnderlying(std::forward<TSource>(src)) );
	};

	template<typename TSource>
	typename boost::enable_if<
		std::is_integral< TSource >,
		TTarget
	>::type operator() (TSource src) const {
		_ASSERTPRINT( (!range_checker<TTarget, TSource>::type::out_of_range(src)), src );
		return static_cast<TTarget>(src);
	}

	template<typename TSource>
	typename boost::enable_if<
		std::is_floating_point< TSource >,
		TTarget
	>::type operator() (TSource src) const {
		double srcRounded=floor( static_cast<double>(src)+.5 );
		_ASSERTPRINT( (!range_checker<TTarget, double>::type::out_of_range(srcRounded)), src );
		return static_cast<TTarget>(srcRounded);
	}

/*	template<typename TSource>
	typename boost::enable_if<
		std::is_enum< TSource >,
		TTarget
	>::type operator() (TSource src) const {
		return (*this)( static_cast<std::underlying_type<TSource>::type(src) );
	}*/
};

template<typename TTarget>
struct SDefaultConversions<TTarget, etypecatFloatingPoint> {
	template<typename TSource>
	typename boost::enable_if<
		boost::mpl::or_<
			std::is_class< typename std::remove_reference< TSource >::type >,
			std::is_union< typename std::remove_reference< TSource >::type >
		>,
		TTarget
	>::type operator() (TSource&& src) const {
		// use specific Convert function defined by source type
		return (*this)( ConvertToUnderlying(std::forward<TSource>(src)) );
	};

	template<typename TSource>
	typename boost::enable_if<
		std::is_arithmetic< TSource >,
		TTarget
	>::type operator() (TSource src) const {
		_ASSERTPRINT( (!range_checker<TTarget, TSource>::type::out_of_range(src)), src );
		return static_cast<TTarget>(src);
	}
};

template<typename TTarget>
struct SDefaultConversions<TTarget, etypecatEnum> {
	template<typename TSource>
	typename boost::enable_if<
		boost::mpl::or_<
			std::is_class< typename std::remove_reference< TSource >::type >,
			std::is_union< typename std::remove_reference< TSource >::type >
		>,
		TTarget
	>::type operator() (TSource&& src) const {
		// use specific Convert function defined by source type
		return (*this)( ConvertToUnderlying(std::forward<TSource>(src)) );
	};

	template<typename TSource>
	typename boost::enable_if<
		std::is_enum< TSource >,
		TTarget
	>::type operator() (TSource src) const {
		_ASSERTPRINT( (!range_checker<TTarget, TSource>::type::out_of_range(src)), src );
		return static_cast<TTarget>(src);
	}

/*	template<typename TSource>
	typename boost::enable_if<
		std::is_integral< TSource >,
		TTarget
	>::type operator() (TSource src) const {
		_ASSERTPRINT( (!range_checker<TTarget, TSource>::type::out_of_range(src)), src );
		return static_cast<TTarget>(src);
	}*/
};

template<typename TTarget>
struct SDefaultConversions<TTarget, etypecatPointer> {
	template<typename TSource>
	typename boost::enable_if<
		boost::mpl::or_<
			std::is_class< typename std::remove_reference< TSource >::type >,
			std::is_union< typename std::remove_reference< TSource >::type >
		>,
		TTarget
	>::type operator() (TSource&& src) const {
		// use specific Convert function defined by source type
		return (*this)( ConvertToUnderlying(std::forward<TSource>(src)) );
	};

	template<typename TSource>
	TTarget operator() (TSource* src) const {
		return static_cast<TTarget>(src);
	}
};

///////////////////////////////////////////////
// special conversions


template<typename _Elem, typename RngValue>
struct SStringRangeConverter;

// if this fails to compile you are probably missing 
// Library/Utilities/Range/transform_adaptor.h or _Str.h 
// in your TU do not add them here, it would create a cycle in the includes

template<typename TContainer>
struct SContainerConversions {
	template<typename Rng>
	typename boost::enable_if<
		boost::mpl::and_<
			boost::mpl::not_< std::is_same< typename std::decay<Rng>::type, TContainer> >, // disable for trivial conversions to use move semantic / copy on write where possible
			boost::mpl::not_< tc::is_char< typename TContainer::value_type > >
		>,
	TContainer >::type operator()(Rng && rng) const {
		return boost::copy_range<TContainer>(tc::transform(std::forward<Rng>(rng), fn_Convert<typename TContainer::value_type>()));
	}

	template<typename Rng>
	typename boost::enable_if<
		boost::mpl::and_<
			boost::mpl::not_< std::is_same< typename std::decay<Rng>::type, TContainer> >, // disable for trivial conversions to use move semantic / copy on write where possible
			tc::is_char< typename TContainer::value_type >
		>,
	TContainer >::type operator()(Rng && rng) const {
		TContainer cont;
		SStringRangeConverter<typename TContainer::value_type, typename boost::range_value< typename std::remove_reference<Rng>::type >::type>::Append(cont, std::forward<Rng>(rng));
		return cont;
	}
};

// in particular useful for copy/assign vectors of different allocators
template<typename T, typename Alloc>
struct SConversions<std::vector<T, Alloc> > :
	SContainerConversions< std::vector<T, Alloc> >
{};

// features conversion from BSTR wrappers (CComBSTR, _bstr_t) to basic_string<OLECHAR>
template<typename CharT, typename Alloc>
struct SConversions<std::basic_string<CharT, std::char_traits<CharT>, Alloc> > :
	SContainerConversions<std::basic_string<CharT, std::char_traits<CharT>, Alloc> >
{};

// features conversion from basic_string to char const*
template<typename CharT>
struct SConversions<CharT const* >{
	template<typename Alloc>
	CharT const* operator()(std::basic_string<CharT, std::char_traits<CharT>, Alloc> const& str) const {
		return str.c_str();
	}
};
