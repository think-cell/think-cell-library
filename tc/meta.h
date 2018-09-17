
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "type_traits.h"
#include "functors.h"
#include "tag_type.h"

#include <type_traits>
#include <string>

namespace tc {
	template<typename T>
	std::size_t strlen( T const* pt ) noexcept {
		_ASSERT(pt);
		return std::char_traits<T>::length(pt);
	}

	namespace no_adl {
		// Specialize for different types and const-nesses
		template<typename Rng, bool bConst, typename Enable=void>
		struct range_reference_base_with_const {};
		
		template<typename Rng>
		struct range_reference_base_with_const<Rng, /*bConst*/false, std::void_t<typename Rng::reference> > {
			using type = typename Rng::reference;
		};

		template<typename Rng>
		struct range_reference_base_with_const<Rng, /*bConst*/true, std::void_t<typename Rng::const_reference> > {
			using type = typename Rng::const_reference;
		};

		// Rationale: range_reference is the type that should be deduced when range is plugged into for_each.
		template< typename Rng, typename Enable=void>
		struct range_reference : range_reference_base_with_const<std::remove_const_t<Rng>, std::is_const<Rng>::value> {};

		template< typename Rng >
		struct range_reference<Rng, std::enable_if_t<std::is_reference<Rng>::value>> : range_reference<std::remove_reference_t<Rng>> {};

		template< typename Rng >
		struct range_reference<Rng, std::enable_if_t<!std::is_reference<Rng>::value && is_range_with_iterators<Rng>::value> > {
			using type = typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::reference;
		};
	}
	using no_adl::range_reference;

	template<typename T>
	using range_reference_t = typename range_reference<T>::type;
	
	namespace no_adl {
		template< typename Rng, typename Enable = void >
		struct range_value final {};

		template< typename Rng , typename Enable=void >
		struct has_value_type final: std::false_type {};

		template< typename Rng >
		struct has_value_type<Rng, tc::void_t<typename std::remove_reference_t<Rng>::value_type>>: std::true_type {};

		template< typename Rng >
		struct range_value<Rng, std::enable_if_t<is_range_with_iterators<Rng>::value && has_value_type<Rng>::value>> final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
			static_assert(std::is_same<type, typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::value_type>::value);
			static_assert(std::is_same<type, typename std::remove_reference_t<Rng>::value_type>::value);
		};

		template< typename Rng >
		struct range_value<Rng, std::enable_if_t<is_range_with_iterators<Rng>::value && !has_value_type<Rng>::value>> final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
			static_assert(std::is_same<type, typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::value_type>::value);
		};

		template< typename Rng >
		struct range_value<Rng, std::enable_if_t<!is_range_with_iterators<Rng>::value && has_value_type<Rng>::value>> final {
			using type = typename std::remove_reference_t<Rng>::value_type;
		};

		template< typename Rng >
		struct range_value<Rng, std::enable_if_t<!is_range_with_iterators<Rng>::value && !has_value_type<Rng>::value, tc::void_t<tc::range_reference_t<Rng>>>> final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
		};
	}
	using no_adl::range_value;

	template<typename T>
	using range_value_t = typename range_value<T>::type;

	namespace no_adl {
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
	auto generator_range_reference(Func&& func) noexcept {
		return no_adl::FuncWithReference<ConstReferenceType, ReferenceType, Func>(std::forward<Func>(func));
	}
}

namespace tc {
	namespace begin_end_adl {
		DEFINE_TAG_TYPE(begin_end_tag);

		template<typename T, std::size_t N>
		T* begin(begin_end_tag, T (&at)[N]) noexcept {
			return at+0;
		}
		template<typename T, std::size_t N>
		T* end(begin_end_tag, T (&at)[N]) noexcept {
			return at+N;
		}

		template<typename It>
		It begin(begin_end_tag, std::pair<It,It> const& pairit) noexcept {
			return pairit.first;
		}
		template<typename It>
		It end(begin_end_tag, std::pair<It,It> const& pairit) noexcept {
			return pairit.second;
		}
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
} \
namespace tc { \
	namespace begin_end_adl { \
		inline xchar* begin(begin_end_tag, xchar* pch) noexcept { \
			return pch; \
		} \
		inline xchar const* begin(begin_end_tag, xchar const* pch) noexcept { \
			return pch; \
		} \
		template<std::size_t N> \
		inline xchar* begin(begin_end_tag, xchar (&ach)[N]) noexcept { \
			return ach; \
		} \
		template<std::size_t N> \
		inline xchar const* begin(begin_end_tag, xchar const (&ach)[N]) noexcept { \
			return ach; \
		} \
		inline xchar* end(begin_end_tag, xchar* pch) noexcept { \
			return pch+tc::strlen(pch); \
		} \
		inline xchar const* end(begin_end_tag, xchar const* pch) noexcept { \
			return pch+tc::strlen(pch); \
		} \
		template<std::size_t N> \
		inline xchar* end(begin_end_tag, xchar (&ach)[N]) noexcept { \
			return ach+VERIFYEQUAL(tc::strlen(ach),N-1); \
		} \
		template<std::size_t N> \
		inline xchar const* end(begin_end_tag, xchar const(&ach)[N]) noexcept { \
			return ach+VERIFYEQUAL(tc::strlen(ach),N-1); \
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

namespace tc{
	namespace no_adl {
		template<typename Rng, template<typename> class Pred, bool is_range_with_iterators>
		struct is_range_of2;
		
		template<typename Rng, template<typename> class Pred>
		struct is_range_of2<Rng, Pred, true> : Pred< tc::range_value_t<Rng> >::type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of2<Rng, Pred, false> : std::false_type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of1 : is_range_of2<Rng, Pred, tc::is_range_with_iterators<Rng>::value >::type {};

		template<typename Rng, template<typename> class Pred>
		struct is_range_of : is_range_of1< std::remove_reference_t<Rng>, Pred > {};
	};
	using no_adl::is_range_of;

	namespace no_adl {
		template<typename Rng>
		struct is_char_range final : is_range_of<Rng, is_char> {};
	}
	using no_adl::is_char_range;

	static_assert( is_char_range<wchar_t const* const>::value );
}

namespace tc_begin_end_no_adl {
	template< typename Rng >
	auto adl_begin(Rng&& rng) MAYTHROW return_decltype(
		// Rng has free begin found by ADL
		begin( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto adl_end(Rng&& rng) MAYTHROW return_decltype(
		// Rng has free end found by ADL
		end( std::forward<Rng>(rng) )
	)
}

namespace tc {
	template< typename Rng >
	constexpr auto begin(Rng&& rng) MAYTHROW return_decltype(
		// Rng has member begin
		std::forward<Rng>(rng).begin()
	)

/*		template< typename Rng >
	auto begin(Rng&& rng) MAYTHROW return_decltype(
		// Rng has free begin found by ADL
		tc_begin_end_no_adl::adl_begin( std::forward<Rng>(rng) )
	)*/

	template< typename Rng >
	constexpr auto begin(Rng&& rng) MAYTHROW return_decltype(
		// Rng has free begin found by tag
		begin( begin_end_adl::begin_end_tag(), std::forward<Rng>(rng) )
	)

	template< typename Rng >
	constexpr auto end(Rng&& rng) MAYTHROW return_decltype(
		// Rng has member end
		std::forward<Rng>(rng).end()
	)

/*		template< typename Rng >
	auto end(Rng&& rng) MAYTHROW return_decltype(
		// Rng has free end found by ADL
		tc_begin_end_no_adl::adl_end( std::forward<Rng>(rng) )
	)*/

	template< typename Rng >
	constexpr auto end(Rng&& rng) MAYTHROW return_decltype(
		// Rng has free end found by tag
		end( begin_end_adl::begin_end_tag(), std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto cbegin(Rng&& rng) MAYTHROW return_decltype(
		// Rng has member begin
		tc::begin(static_cast<Rng const&&>(rng))
	)

	template< typename Rng >
	auto cend(Rng&& rng) MAYTHROW return_decltype(
		// Rng has member begin
		tc::end(static_cast<Rng const&&>(rng))
	)

	template< typename Rng >
	auto cbegin(Rng& rng) MAYTHROW return_decltype(
		// Rng has member begin
		tc::begin(static_cast<Rng const&>(rng))
	)

	template< typename Rng >
	auto cend(Rng& rng) MAYTHROW return_decltype(
		// Rng has member begin
		tc::end(static_cast<Rng const&>(rng))
	)
}

//////////////////////////////////////////////
// Boost.Range compatiblity

#ifdef BOOST_RANGE_BEGIN_HPP
#error
#endif
#ifdef BOOST_RANGE_END_HPP
#error
#endif
#define BOOST_RANGE_BEGIN_HPP
#define BOOST_RANGE_END_HPP

namespace boost {
	template< typename Rng >
	auto begin(Rng&& rng) MAYTHROW return_decltype(
		tc::begin( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto end(Rng&& rng) MAYTHROW return_decltype(
		tc::end( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto const_begin(Rng&& rng) MAYTHROW return_decltype(
		tc::begin( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto const_end(Rng&& rng) MAYTHROW return_decltype(
		tc::end( std::forward<Rng>(rng) )
	)
}
