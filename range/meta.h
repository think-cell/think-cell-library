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

#include "range_defines.h"
#include "range_fwd.h"
#include "type_traits.h"
#include "functors.h"

#include <boost/range/value_type.hpp>

#include <type_traits>
#include <string>
#include <ctype.h>

namespace tc {
	template<typename T>
	std::size_t strlen( T const* pt ) noexcept {
		return std::char_traits<T>::length(pt);
	}

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	bool isasciidigit( T ch ) noexcept {
		return tc::char_cast<T>('0')<=ch && ch<=tc::char_cast<T>('9');
	}

	template< typename T >
	bool isasciiupper( T ch ) noexcept {
		return tc::char_cast<T>('A')<=ch && ch<=tc::char_cast<T>('Z');
	}

	template< typename T >
	bool isasciilower( T ch ) noexcept {
		return tc::char_cast<T>('a')<=ch && ch<=tc::char_cast<T>('z');
	}

	template< typename T >
	bool isasciicntrl( T ch ) noexcept {
		return tc::char_cast<T>('\0')<=ch && ch<=tc::char_cast<T>('\x1f') || tc::char_cast<T>('\x7f')==ch;
	}

	template< typename T >
	bool isasciiblank( T ch ) noexcept {
		return tc::char_cast<T>('\t')==ch || tc::char_cast<T>(' ')==ch;
	}

	template< typename T >
	bool isasciispace( T ch ) noexcept {
		return tc::isasciiblank(ch) || 
			tc::char_cast<T>('\xa')<=ch && ch<=tc::char_cast<T>('\xd'); // \n, \v, \f, \r
	}
	
	template< typename T >
	T toasciiupper( T ch ) noexcept {
		if( isasciilower(ch) ) {
			return static_cast<T>( ch-('a'-'A') );
		} else {
			return ch;
		}
	}
	template< typename T >
	T toasciilower( T ch ) noexcept {
		if( isasciiupper(ch) ) {
			return static_cast<T>( ch+('a'-'A') );
		} else {
			return ch;
		}
	}

	DEFINE_FN(toasciilower)
	DEFINE_FN(toasciiupper)
	DEFINE_FN(isasciispace)

	template< typename T >
	struct range_value : boost::range_value<T> {};
}

#pragma push_macro("CHAR_RANGE")
#define CHAR_RANGE( xchar ) \
namespace boost { \
	template<> \
	struct range_mutable_iterator<xchar*> { \
		using type = xchar*; \
	}; \
	template<> \
	struct range_const_iterator<xchar*> { \
		using type = xchar*; \
	}; \
	template<> \
	struct range_mutable_iterator<xchar const*> { \
		using type = xchar const*; \
	}; \
	template<> \
	struct range_const_iterator<xchar const*> { \
		using type = xchar const*; \
	}; \
	template<std::size_t N> \
	struct range_mutable_iterator<xchar[N]> { \
		using type = xchar*; \
	}; \
	template<std::size_t N> \
	struct range_const_iterator<xchar[N]> { \
		using type = xchar const*; \
	}; \
	/* support array-of-unknown-bounds incomplete type */ \
	template<> \
	struct range_mutable_iterator<xchar[]> { \
		using type = xchar*; \
	}; \
	template<> \
	struct range_const_iterator<xchar[]> { \
		using type = xchar const*; \
	}; \
	namespace range_detail { \
		inline xchar* range_begin(xchar* pch) { \
			return pch; \
		} \
		inline xchar const* range_begin(xchar const* pch) { \
			return pch; \
		} \
		template<std::size_t N> \
		inline xchar* range_begin(xchar (&ach)[N]) { \
			return ach; \
		} \
		template<std::size_t N> \
		inline xchar const* range_begin(xchar const (&ach)[N]) { \
			return ach; \
		} \
		inline xchar* range_end(xchar* pch) { \
			return pch+tc::strlen(pch); \
		} \
		inline xchar const* range_end(xchar const* pch) { \
			return pch+tc::strlen(pch); \
		} \
		template<std::size_t N> \
		inline xchar* range_end(xchar (&ach)[N]) { \
			std::size_t cch=tc::strlen(ach); \
			_ASSERT(cch<N); \
			return ach+cch; \
		} \
		template<std::size_t N> \
		inline xchar const* range_end(xchar const(&ach)[N]) { \
			std::size_t cch = tc::strlen(ach); \
			_ASSERT(cch<N); \
			return ach + cch; \
		} \
	} \
}

CHAR_RANGE(char)
CHAR_RANGE(wchar_t)
#ifndef BOOST_NO_CXX11_CHAR16_T
	CHAR_RANGE(char16_t)
#endif
#ifndef BOOST_NO_CXX11_CHAR32_T
	CHAR_RANGE(char32_t)
#endif
#pragma pop_macro("CHAR_RANGE")


// Include the following after overloading range_begin/range_end
// Clang already instantiates some calls to range_begin inside
// iterator_range.hpp and does not see the char* overloads otherwise
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#else
#pragma warning(push)
#pragma warning( disable: 4267 )
#endif
// warning C4267 : 'argument' : conversion from 'size_t' to 'int', possible loss of data
// _Median(...) causes warning C4267 when difference_type is int and size_t is 64 bit. 
// Stephan T. Lavavej [stl@exchange.microsoft.com] agrees this is a bug and filed DevDiv#1213041 
// "<algorithm>: _Median() doesn't handle fancy difference types" to track the problem.
#include <boost/range/iterator_range.hpp>
#include <boost/range/sub_range.hpp>
#include <boost/range/any_range.hpp>
#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma warning(pop)
#endif

#include <boost/range/has_range_iterator.hpp>

static_assert(boost::has_range_iterator<char*>::value, "");

namespace tc{
	namespace is_range_of_adl_barrier {
		template<typename Rng, template<typename> class Pred, bool is_range_with_iterators>
		struct is_range_of2;
		
		template<typename Rng, template<typename> class Pred>
		struct is_range_of2<Rng, Pred, true> : Pred< typename tc::range_value<Rng>::type >::type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of2<Rng, Pred, false> : std::false_type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of1 : is_range_of2<Rng, Pred, tc::is_range_with_iterators<Rng>::value >::type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of : is_range_of1< std::remove_reference_t<Rng>, Pred > {};
	};
	using is_range_of_adl_barrier::is_range_of;

	namespace is_char_range_adl_barrier {
		template<typename Rng>
		struct is_char_range final : is_range_of<Rng, is_char> {};
	}
	using is_char_range_adl_barrier::is_char_range;

	static_assert( is_char_range<wchar_t const* const>::value, "" );

	//////////////////////////////
	// Ranges are either containers or views, which are references to consecutive elements in containers.
	//
	//	Views are regular types with pointer copy-semantics:
	//	- Modifying a copied view itself leaves the original unchanged.
	//	- Modifying the pointed-to elements modifies the same elements the original is pointing to.
	//	They are expected to be lightweight and passed by value.
	//	Their iterators must not be live-limited to the view,
	//	which outlaws iterators holding pointers to their view.
	//	Otherwise, the following would be illegal:
	//	auto lowerbound( Range, t ) { return std::begin(equal_range( Range, t )); }
	//
	//	Containers are expected to be heavy and passed by reference.
	//	Their iterator life may be limited to the life of the container.
	//	They may not be copyable.
	//	Containers have deep constness, so iterators of const containers are const_iterators.

	// singleton_range<T&> is view
	template< typename T >
	struct singleton_range;

	// boost::iterator_range and derived are ranges
	namespace is_view_adl_barrier {
		template<typename T, bool is_range_with_iterators>
		struct is_view_impl2;
		template<typename T>
		struct is_view_impl2<T, true> : std::integral_constant< bool, std::is_base_of< boost::iterator_range< typename boost::range_iterator<T>::type >, T >::value > {};
		template<typename T>
		struct is_view_impl2<T, false> : std::false_type {};
		template<typename T>
		struct is_view_impl : is_view_impl2<T, tc::is_range_with_iterators<T>::value >::type {};

		// pointers to zero-terminated strings are views
		template< typename T >
		struct is_view_impl<T*> : is_char<T>::type {};

		// sub_ranges are views
		template< typename T >
		struct is_view_impl<sub_range<T>> : std::true_type {};

		// iterator_base is view
		template< typename It, typename ConstIt >
		struct is_view_impl<iterator_base<It,ConstIt>> : std::true_type {};

		template< typename T >
		struct is_view_impl<singleton_range<T&>> : std::true_type{};

		template< typename T >
		struct is_view final : is_view_impl< tc::decay_t<T> > {};

		template< typename T >
		struct view_by_value final {
			static_assert(!std::is_rvalue_reference<T>::value, "");
			using type = std::conditional_t< is_view<T>::value
				, tc::decay_t<T>
				, T
			>;
		};
	}
	using is_view_adl_barrier::is_view;
	using is_view_adl_barrier::view_by_value;

	template<typename T>
	using view_by_value_t = typename view_by_value<T>::type;
	
	/////////////////////////////////////////////
	// is_instance

	template<template<typename...> class X, typename T> struct is_instance : public std::false_type {};
	template<template<typename...> class X, typename T> struct is_instance<X, T const> : public is_instance<X, T> {};
	template<template<typename...> class X, typename T> struct is_instance<X, T volatile> : public is_instance<X, T> {};
	template<template<typename...> class X, typename T> struct is_instance<X, T const volatile> : public is_instance<X, T> {};
	template<template<typename...> class X, typename... Y> struct is_instance<X, X<Y...>> : public std::true_type {};

	/////////////////////////////////////////////
	// verify_class

	template<typename C>
	struct verify_class_impl final{
		static_assert( std::is_class<C>::value, "not a class!");
		using type=C;
	};

	template<typename C>
	using verify_class=typename verify_class_impl<C>::type;

	template<typename T> DEFINE_FN2( tc::verify_class<T>, fn_ctor );
}
