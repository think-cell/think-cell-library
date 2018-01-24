//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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
		_ASSERT(pt);
		return std::char_traits<T>::length(pt);
	}

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	bool isasciidigit( T ch ) noexcept {
		return tc::explicit_cast<T>('0')<=ch && ch<=tc::explicit_cast<T>('9');
	}

	template< typename T >
	bool isasciiupper( T ch ) noexcept {
		return tc::explicit_cast<T>('A')<=ch && ch<=tc::explicit_cast<T>('Z');
	}

	template< typename T >
	bool isasciilower( T ch ) noexcept {
		return tc::explicit_cast<T>('a')<=ch && ch<=tc::explicit_cast<T>('z');
	}

	template< typename T >
	bool isasciicntrl( T ch ) noexcept {
		return tc::explicit_cast<T>('\0')<=ch && ch<=tc::explicit_cast<T>('\x1f') || tc::explicit_cast<T>('\x7f')==ch;
	}
	DEFINE_FN(isasciicntrl)

	template< typename T >
	bool isasciiblank( T ch ) noexcept {
		return tc::explicit_cast<T>('\t')==ch || tc::explicit_cast<T>(' ')==ch;
	}

	template< typename T >
	bool isasciispace( T ch ) noexcept {
		return tc::isasciiblank(ch) || 
			tc::explicit_cast<T>('\xa')<=ch && ch<=tc::explicit_cast<T>('\xd'); // \n, \v, \f, \r
	}
	DEFINE_FN(isasciispace)
	
	template< typename T >
	T toasciiupper( T ch ) noexcept {
		if( isasciilower(ch) ) {
			return static_cast<T>( ch-('a'-'A') );
		} else {
			return ch;
		}
	}
	DEFINE_FN(toasciiupper)

	template< typename T >
	T toasciilower( T ch ) noexcept {
		if( isasciiupper(ch) ) {
			return static_cast<T>( ch+('a'-'A') );
		} else {
			return ch;
		}
	}
	DEFINE_FN(toasciilower)

	namespace range_reference_adl_barrier {
		template<typename Rng, bool>
		struct range_reference_base_with_const;

		template<typename Rng>
		struct range_reference_base_with_const<Rng, true> {
			using type = typename Rng::const_reference;
		};

		template<typename Rng>
		struct range_reference_base_with_const<Rng, false> {
			using type = typename Rng::reference;
		};

		// Rationale: range_reference is the type that should be deduced when range is plugged into for_each.
		template< typename Rng, typename Enable=void>
		struct range_reference : range_reference_base_with_const<Rng, std::is_const<Rng>::value> {};

		template< typename Rng >
		struct range_reference<Rng, std::enable_if_t<std::is_reference<Rng>::value>> : range_reference<std::remove_reference_t<Rng>> {};

		template< typename Rng >
		struct range_reference<Rng, std::enable_if_t<!std::is_reference<Rng>::value && is_range_with_iterators<Rng>::value> > {
			using type = typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::reference;
		};
	}
	using range_reference_adl_barrier::range_reference;

	template<typename T>
	using range_reference_t = typename range_reference<T>::type;
	
	namespace range_value_adl_barrier {
		template< typename Rng, typename Enable = void >
		struct range_value final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
		};

		template< typename Rng>
		struct range_value<Rng, std::enable_if_t<is_range_with_iterators<Rng>::value> > final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
			static_assert(std::is_same<
				type,
				typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::value_type
			>::value);
		};
	}
	using range_value_adl_barrier::range_value;

	template<typename T>
	using range_value_t = typename range_value<T>::type;

	namespace generator_range_reference_adl_barrier {
		template<typename ConstReferenceType, typename ReferenceType, typename Func>
		struct FuncWithReference : tc::decay_t<Func> {
			using reference = ReferenceType;
			using const_reference = ConstReferenceType;

			using tc::decay_t<Func>::operator();

			FuncWithReference(Func&& func) noexcept
				: tc::decay_t<Func>(std::forward<Func>(func))
			{}
		};
	}

	template<typename ConstReferenceType, typename ReferenceType = ConstReferenceType, typename Func>
	generator_range_reference_adl_barrier::FuncWithReference<ConstReferenceType, ReferenceType, Func> generator_range_reference(Func&& func) {
		return generator_range_reference_adl_barrier::FuncWithReference<ConstReferenceType, ReferenceType, Func>(std::forward<Func>(func));
	}
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

static_assert(boost::has_range_iterator<char*>::value, "Please apply think-cell boost patches, look at README.md for further information.");

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

	static_assert( is_char_range<wchar_t const* const>::value );

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
		struct is_view final : is_view_impl< std::remove_cv_t<T> > {};

		template< typename T >
		struct view_by_value final {
			static_assert(!std::is_rvalue_reference<T>::value);
			using type = std::conditional_t< is_view<std::remove_reference_t<T>>::value
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
