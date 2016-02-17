//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "transform.h"
#include "is_static_castable.h"
#include "round.h"
#include "container.h" // tc::vector

#include <type_traits>
#include <string>

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

///////////////////////////////////////////////
// default conversions

template<typename TTarget, typename Enable=void>
struct SDefaultConversions;

template< typename T >
std::streamoff ConvertToUnderlying( std::fpos<T> const& pos ) noexcept {
	return pos;
}

// if target is a reference, disallow initializations which involve nontrivial conversions
template<typename TTarget>
struct SDefaultConversions<TTarget,  std::enable_if_t<std::is_reference<TTarget>::value>> {
	template<typename TSource>
	std::enable_if_t<
		tc::creates_no_reference_to_temporary<TSource, TTarget>::value
	, TTarget> operator() (TSource&& src) const {
		return src; // static_cast if needed?
	}
};

template<typename TTarget >
struct SClassConversions {
	static_assert( tc::is_decayed< TTarget >::value, "" );

	template<typename TSource>
	std::enable_if_t<
		(
			std::is_class<TTarget>::value ||
			std::is_class< std::remove_reference_t<TSource> >::value
		) && tc::is_static_castable< TSource&&, TTarget >::value
	, TTarget > operator() (TSource&& src) const noexcept {
		return static_cast<TTarget>(std::forward<TSource>(src));
	}
};

template<typename TTarget>
struct SDefaultConversions<TTarget, std::enable_if_t<std::is_class<TTarget>::value>> : SClassConversions<TTarget> {};

template<>
struct SDefaultConversions<bool> : SClassConversions<bool> {
	using SClassConversions<bool>::operator();

	bool operator() (bool b) const {
		return b;
	}
};

template<typename TTarget>
struct SDefaultConversions<TTarget,  std::enable_if_t<tc::is_char<TTarget>::value>> : SClassConversions<TTarget> {
	using SClassConversions<TTarget>::operator();

	template<typename TSource>
	std::enable_if_t<
		tc::is_char< TSource >::value
	, TTarget > operator() (TSource src) const {
		return tc::char_cast<TTarget>(src);
	}
};

template<typename TTarget>
struct SDefaultConversions<TTarget,  std::enable_if_t<tc::is_actual_integer<TTarget>::value>> : SClassConversions<TTarget> {
	using SClassConversions<TTarget>::operator();

	template<typename TSource>
	std::enable_if_t<
		tc::is_actual_integer< TSource >::value
	, TTarget > operator() (TSource src) const {
		return tc::numeric_cast<TTarget>(src);
	}

	template<typename TSource>
	std::enable_if_t<
		std::is_floating_point< TSource >::value
	,TTarget> operator() (TSource src) const {
		double srcRounded=std::floor( static_cast<double>(src)+.5 );
		return tc::numeric_cast<TTarget>(srcRounded);
	}
};

template<typename TTarget>
struct SDefaultConversions<TTarget,  std::enable_if_t<std::is_floating_point<TTarget>::value>> : SClassConversions<TTarget> {
	using SClassConversions<TTarget>::operator();

	template<typename TSource>
	std::enable_if_t<
		std::is_arithmetic< TSource >::value
	, TTarget> operator() (TSource src) const {
		return tc::numeric_cast<TTarget>(src);
	}
};

template<typename TTarget>
struct SDefaultConversions<TTarget, std::enable_if_t<std::is_enum<TTarget>::value || std::is_pointer<TTarget>::value>> : SClassConversions<TTarget> {
	using SClassConversions<TTarget>::operator();

	TTarget operator() (TTarget src) const {
		return src;
	}
};

template<typename TTarget>
struct SConversions final : SDefaultConversions<TTarget> {};

// control whether to ConvertToUnderlying first
struct SFirstConvertToUnderlying {};
struct SDirectConvert final : SFirstConvertToUnderlying {};

template<typename TTarget, typename TSource>
TTarget InternalConvert( TSource&& src, SFirstConvertToUnderlying ) noexcept {
	return Convert<TTarget>(ConvertToUnderlying(std::forward<TSource>(src)));
}

template<typename TTarget, typename TSource>
auto InternalConvert( TSource&& src, SDirectConvert ) noexcept
	return_decltype_rvalue_by_ref( SConversions<TTarget>() (std::forward<TSource>(src)) )

template<typename TTarget, typename TSource>
TTarget Convert(TSource&& src) noexcept {
	return InternalConvert<TTarget>(std::forward<TSource>(src), /*prefer no ConvertToUnderlying*/ SDirectConvert());
}

DEFINE_FN_TMPL( Convert, (typename) );

///////////////////////////////////////////////
// special conversions


template<typename _Elem, typename RngValue>
struct SStringRangeConverter;

// if this fails to compile you are probably missing 
// Library/Utilities/Public/transform_adaptor.h or _Str.h 
// in your TU do not add them here, it would create a cycle in the includes
template<typename TContainer>
struct SContainerConversionsHelper {
	template<typename Rng>
	std::enable_if_t<
		!tc::is_static_castable< Rng&&, TContainer >::value && // disable for trivial conversions to use move semantic / copy on write where possible
		!tc::is_char< typename tc::range_value<TContainer>::type >::value
	, TContainer > operator()(Rng&& rng) const noexcept {
		return tc::make_container<TContainer>(tc::transform(std::forward<Rng>(rng), fn_Convert<typename tc::range_value<TContainer>::type>()));
	}

	template<typename Rng>
	std::enable_if_t<
		!tc::is_static_castable< Rng&&, TContainer >::value && // disable for trivial conversions to use move semantic / copy on write where possible
		tc::is_char< typename tc::range_value<TContainer>::type >::value
	, TContainer > operator()(Rng&& rng) const noexcept {
		TContainer cont;
		SStringRangeConverter<typename tc::range_value<TContainer>::type, typename tc::range_value< std::remove_reference_t<Rng> >::type>::Append(cont, std::forward<Rng>(rng));
		return cont;
	}
};

// SContainerConversions cannot implement templated operator()(Rng&&) *and* use operator()(TSource&&)
// from SDefaultConversions. Apparently, despite the std::enable_if constructs, both are considered
// to have the same signatures and the using declaration is therefore ignored. Only Clang implements
// this standard rule, however:
// http://stackoverflow.com/questions/18861514/using-and-overloading-a-template-member-function-of-a-base-class
template<typename TContainer>
struct SContainerConversions : SDefaultConversions<TContainer>, SContainerConversionsHelper<TContainer> {
	using SDefaultConversions<TContainer>::operator();
	using SContainerConversionsHelper<TContainer>::operator();
};

// in particular useful for copy/assign vectors of different allocators
template<typename T, typename Alloc>
struct SConversions<tc::vector<T, Alloc> > final :
	SContainerConversions< tc::vector<T, Alloc> >
{};

// features conversion from BSTR wrappers (ATL::CComBSTR, _bstr_t) to basic_string<OLECHAR>
template<typename Char, typename Alloc>
struct SConversions<std::basic_string<Char, std::char_traits<Char>, Alloc> > final :
	SContainerConversions<std::basic_string<Char, std::char_traits<Char>, Alloc> >
{};

// features conversion from basic_string to char const*
template<typename Char>
struct SConversions<Char const* > final : SDefaultConversions<Char const*> {
	using SDefaultConversions<Char const*>::operator();

	template<typename Alloc>
	Char const* operator()(std::basic_string<Char, std::char_traits<Char>, Alloc> const& str) const {
		return tc::as_c_str(str);
	}

#ifdef _MSC_VER // compiler bug
	template<typename Alloc>
	Char const* operator()(std::basic_string<Char, std::char_traits<Char>, Alloc>&& str) const {
		return tc::as_c_str(str);
	}
#endif
};

template<typename TTargetFirst, typename TTargetSecond>
struct SConversions<std::pair<TTargetFirst, TTargetSecond>> final  : SDefaultConversions<std::pair<TTargetFirst, TTargetSecond>> {
	using SDefaultConversions<std::pair<TTargetFirst, TTargetSecond>>::operator();

	template<typename TSourceFirst, typename TSourceSecond>
	std::pair<TTargetFirst, TTargetSecond> operator()(std::pair<TSourceFirst, TSourceSecond> const& pair) const {
		// std::remove_cv affects only values and leaves const/volatile references untouched, which is what we want.
		return std::pair<TTargetFirst, TTargetSecond>(
			Convert<std::remove_cv_t<TTargetFirst>>(pair.first),
			Convert<std::remove_cv_t<TTargetSecond>>(pair.second)
		);
	}
};

