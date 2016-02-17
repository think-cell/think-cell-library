#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "remove_cvref.h"
#include "functors.h"

#include <boost/range/value_type.hpp>

#include <type_traits>
#include <string>
#include <ctype.h>

namespace RANGE_PROPOSAL_NAMESPACE {
	template<typename T>
	std::size_t strlen( T const* pt ) {
		return std::char_traits<T>::length(pt);
	}

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	bool isasciidigit( T ch ) {
		return tc::char_cast<T>('0')<=ch && ch<=tc::char_cast<T>('9');
	}

	template< typename T >
	bool isasciiupper( T ch ) {
		return tc::char_cast<T>('A')<=ch && ch<=tc::char_cast<T>('Z');
	}

	template< typename T >
	bool isasciilower( T ch ) {
		return tc::char_cast<T>('a')<=ch && ch<=tc::char_cast<T>('z');
	}

	template< typename T >
	bool isasciicntrl( T ch ) {
		return tc::char_cast<T>('\0')<=ch && ch<=tc::char_cast<T>('\x1f') || tc::char_cast<T>('\x7f')==ch;
	}
	
	template< typename T >
	T toasciiupper( T ch ) {
		if( isasciilower(ch) ) {
			return tc::sub( ch, 'a'-'A' );
		} else {
			return ch;
		}
	}
	template< typename T >
	T toasciilower( T ch ) {
		if( isasciiupper(ch) ) {
			return tc::add( ch, 'a'-'A' );
		} else {
			return ch;
		}
	}

	DEFINE_FN(toasciilower)
	DEFINE_FN(toasciiupper)

	template< class T >
	struct range_value : boost::range_value<T> {};
}

namespace boost {
	#pragma push_macro("CHAR_RANGE")
	#define CHAR_RANGE( xchar ) \
		template<> \
		struct range_iterator<xchar*> { \
			using type = xchar*; \
		}; \
		template<> \
		struct range_mutable_iterator<xchar*> { \
			using type = xchar*; \
		}; \
		template<> \
		struct range_const_iterator<xchar*> { \
			using type = std::add_const<xchar>::type*; \
		}; \
		template<> \
		struct range_iterator<xchar* const> { \
			using type = xchar*; \
		}; \
		template<> \
		struct range_mutable_iterator<xchar* const> { \
			using type = xchar*; \
		}; \
		template<> \
		struct range_const_iterator<xchar* const> { \
			using type = std::add_const<xchar>::type*; \
		}; \
		template<std::size_t N> \
		struct range_mutable_iterator<xchar[N]> { \
			using type = xchar*; \
		}; \
		/* support array-of-unknown-bounds incomplete type */ \
		template<> \
		struct range_mutable_iterator<xchar[]> { \
			using type = xchar*; \
		}; \
		template<std::size_t N> \
		struct range_const_iterator<xchar[N]> { \
			using type = std::add_const<xchar>::type*; \
		}; \
		/* support array-of-unknown-bounds incomplete type */ \
		template<> \
		struct range_const_iterator<xchar[]> { \
			using type = std::add_const<xchar>::type*; \
		}; \
		inline xchar* range_begin(xchar* pch) { \
			return pch; \
		} \
		template<std::size_t N> \
		inline xchar* range_begin(xchar (&ach)[N]) { \
			return ach; \
		} \
		inline xchar* range_end(xchar* pch) { \
			return pch+tc::strlen(pch); \
		} \
		template<std::size_t N> \
		inline xchar* range_end(xchar (&ach)[N]) { \
			std::size_t cch=tc::strlen(ach); \
			_ASSERT(cch<N); \
			return ach+cch; \
		}

	CHAR_RANGE(char)
	CHAR_RANGE(char const)
	CHAR_RANGE(wchar_t)
	CHAR_RANGE(wchar_t const)
#ifndef BOOST_NO_CXX11_CHAR16_T
		CHAR_RANGE(char16_t)
		CHAR_RANGE(char16_t const)
#endif
#ifndef BOOST_NO_CXX11_CHAR32_T
		CHAR_RANGE(char32_t)
		CHAR_RANGE(char32_t const)
#endif
	#pragma pop_macro("CHAR_RANGE")
}

// Include the following after overloading range_begin/range_end
// Clang already instantiates some calls to range_begin inside
// iterator_range.hpp and does not see the char* overloads otherwise
#ifdef TC_MAC
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#endif

#pragma warning(push)
#pragma warning( disable: 4267 )
// warning C4267 : 'argument' : conversion from 'size_t' to 'int', possible loss of data
// _Median(...) causes warning C4267 when difference_type is int and size_t is 64 bit. 
// Stephan T. Lavavej [stl@exchange.microsoft.com] agrees this is a bug and filed DevDiv#1213041 
// "<algorithm>: _Median() doesn't handle fancy difference types" to track the problem.
#include <boost/range/iterator_range.hpp>
#include <boost/range/sub_range.hpp>
#include <boost/range/any_range.hpp>
#pragma warning(pop)

#ifdef TC_MAC
#pragma clang diagnostic pop
#endif 

#include <boost/range/has_range_iterator.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {
	namespace is_char_detail {
		template< typename T >
		struct is_char: std::false_type {};
		template<>
		struct is_char<char>: std::true_type {};
		template<>
		struct is_char<wchar_t>: std::true_type {};
#ifndef BOOST_NO_CXX11_CHAR16_T
		template<>
		struct is_char<char16_t>: std::true_type {};
#endif
#ifndef BOOST_NO_CXX11_CHAR32_T
		template<>
		struct is_char<char32_t>: std::true_type {};
#endif
	}
	template< typename T >
	struct is_char: is_char_detail::is_char< typename std::remove_cv< T >::type > {};

	namespace is_range_with_iterators_detail {

		template< typename Rng >
		struct is_range_with_iterators : boost::mpl::or_<
				boost::has_range_const_iterator<Rng>,
				boost::has_range_iterator<Rng>
			>::type
		{};

		template<typename T>
		struct is_range_with_iterators<T*> : tc::is_char<T>::type {};

		template<typename T, std::size_t N>
		struct is_range_with_iterators<T[N]> : std::true_type {};

		/* support array-of-unknown-bounds incomplete type */
		template<typename T>
		struct is_range_with_iterators<T[]> : std::true_type {};
	}
	template<typename Rng>
	struct is_range_with_iterators : is_range_with_iterators_detail::is_range_with_iterators<tc::remove_cvref_t<Rng>> {};

	namespace is_range_of_impl {
		template<typename Rng, template<typename> class Pred, bool is_range_with_iterators>
		struct is_range_of2;
		
		template<typename Rng, template<typename> class Pred>
		struct is_range_of2<Rng, Pred, true> : Pred< typename tc::range_value<Rng>::type >::type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of2<Rng, Pred, false> : std::false_type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of1 : is_range_of2<Rng, Pred, is_range_with_iterators<Rng>::value >::type {};
	};
	template<typename Rng, template<typename> class Pred>
	struct is_range_of : is_range_of_impl::is_range_of1< tc::remove_cvref_t<Rng>, Pred > {};

	template<typename Rng>
	struct is_char_range : is_range_of<Rng, is_char> {};

	static_assert( is_char_range<wchar_t const* const>::value, "" );

	template< typename T >
	struct is_decayed : std::is_same< T, std::decay_t<T> >::type {};

	template<typename T>
	struct is_non_char_integral : std::integral_constant< bool, std::is_integral<T>::value && !tc::is_char<T>::value > {};

	//////////////////////////////
	// Traversables are either containers or ranges, which are references to consecutive elements in containers.
	//
	//	Ranges are regular types with pointer copy-semantics:
	//	- Modifying a copied range itself leaves the original unchanged.
	//	- Modifying the pointed-to elements modifies the same elements the original is pointing to.
	//	They are expected to be lightweight and passed by value.
	//	Their iterators must not be live-limited to the range,
	//	which outlaws iterators holding pointers to their range.
	//	Otherwise, the following would be illegal:
	//	auto lowerbound( Range, t ) { return std::begin(equal_range( Range, t )); }
	//
	//	Containers are expected to be heavy and passed by reference.
	//	Their iterator life may be limited to the life of the container.
	//	They may not be copyable.
	//	Containers have deep constness, so iterators of const containers are const_iterators.

	// const_range<T>::type is the equivalent of T* -> T const*
	// It makes the pointee const.
	template< typename T >
	struct const_range;

	// boost::iterator_range and derived are ranges
	template<typename T, bool is_range_with_iterators>
	struct is_range_impl2;
	template<typename T>
	struct is_range_impl2<T, true> : std::integral_constant< bool, std::is_base_of< boost::iterator_range< typename boost::range_iterator<T>::type >, T >::value > {};
	template<typename T>
	struct is_range_impl2<T, false> : std::false_type {};
	template<typename T>
	struct is_range_impl : is_range_impl2<T, tc::is_range_with_iterators<T>::value >::type {};
	template<typename It>
	struct const_range<boost::iterator_range<It>> {
		using type = boost::iterator_range<typename const_iterator_<It>::type>;
	};
	template< typename T >
	struct const_range<boost::sub_range<T>> {
		using type = boost::sub_range<T const>;
	};
	template<
		class Value
		, class Traversal
		, class Reference
		, class Difference
		, class Buffer
	>
	struct const_range<boost::any_range<Value,Traversal,Reference,Difference,Buffer>> {
		using type = boost::any_range<Value,Traversal,Reference,Difference,Buffer>;
	};
	template<
		class Value
		, class Traversal
		, class Reference
		, class Difference
		, class Buffer
	>
	struct const_range<boost::any_range<Value,Traversal,Reference&,Difference,Buffer>> {
		using type = boost::any_range<Value,Traversal,Reference const&,Difference,Buffer>;
	};

	// pointers to zero-terminated strings are ranges
	template< typename T >
	struct is_range_impl<T*> : is_char<T>::type {};
	template< typename T >
	struct const_range<T*> {
		using type = T const*;
	};
	// sub_ranges are ranges
	template< typename T >
	struct is_range_impl<sub_range<T>> : std::true_type {};
	template< typename T >
	struct const_range<sub_range<T>> {
		// sub_range of another range
		using type = sub_range<typename const_range<T>::type>;
	};
	template< typename T >
	struct const_range<sub_range<T&>> {
		// sub_range of a container, which is expected to have deep constness.
		using type = sub_range<T const&>;
	};
	// iterator_base is range
	template< typename It, typename ConstIt >
	struct is_range_impl<iterator_base<It,ConstIt>> : std::true_type {};
	template< typename It, typename ConstIt >
	struct const_range<iterator_base<It,ConstIt>> {
		using type = iterator_base<ConstIt,ConstIt>;
	};

	// singleton_range<T&> is range
	template< typename T >
	struct singleton_range;

	template< typename T >
	struct const_range<singleton_range<T&>> {
		// singleton_range of a container, which is expected to have deep constness.
		using type = singleton_range<T const&>;
	};
	template< typename T >
	struct is_range_impl<singleton_range<T&>> : std::true_type{};

	template< typename T >
	struct is_range : is_range_impl< std::decay_t<T> > {};

	namespace range_by_value_impl {
		template< typename T >
		struct range_by_value_for_range {
			using type = std::decay_t<T>;
		};
		template< typename T >
		struct range_by_value_for_range<T const&> {
			using type = typename tc::const_range< std::decay_t<T> >::type;
		};
	}
	template< typename T >
	struct range_by_value {
		static_assert( !std::is_rvalue_reference<T>::value, "" );
		using type = typename boost::mpl::eval_if_c< is_range<T>::value
			, range_by_value_impl::range_by_value_for_range<T>
			, boost::mpl::identity<T>
		>::type;
	};
	
	/////////////////////////////////////////////
	// verify_class

	template<typename C>
	struct verify_class_impl{
		static_assert( std::is_class<C>::value, "not a class!");
		using type=C;
	};

	template<typename C>
	using verify_class=typename verify_class_impl<C>::type;

	template<typename T> DEFINE_FN2( tc::verify_class<T>, fn_ctor );
}
