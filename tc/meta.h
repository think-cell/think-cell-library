
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "range_fwd.h"
#include "type_traits.h"
#include "functors.h"
#include "tag_type.h"

#include <type_traits>
#include <string>

namespace tc {
	template<typename T>
	[[nodiscard]] constexpr std::size_t strlen( T const* pt ) noexcept {
		_ASSERTE(pt);
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
		struct range_reference<Rng, std::enable_if_t<!std::is_reference<Rng>::value && is_range_with_iterators<Rng>::value, tc::void_t<typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::reference>> > {
			using type = typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::reference;
		};
	}
	using no_adl::range_reference;

	template<typename T>
	using range_reference_t = typename range_reference<T>::type;
	
	namespace no_adl {
		template< typename Rng, typename RngNoCvref = tc::remove_cvref_t<Rng>, typename Enable = void >
		struct range_value final {};

		template< typename RngNoRef, typename Enable=void >
		struct has_value_type final : std::false_type {
			static_assert(!std::is_reference<RngNoRef>::value);
		};

		template< typename Rng >
		struct has_value_type<Rng, tc::void_t<typename Rng::value_type>> : std::true_type {};

		template< typename Rng, typename RngNoCvref >
		struct range_value<Rng, RngNoCvref, std::enable_if_t<
			is_range_with_iterators<Rng>::value && has_value_type<RngNoCvref>::value,
			tc::void_t<tc::decay_t<tc::range_reference_t<Rng>>>
		>> final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
			STATICASSERTSAME(type, typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::value_type);
			STATICASSERTSAME(type, typename RngNoCvref::value_type);
		};

		template< typename Rng, typename RngNoCvref >
		struct range_value<Rng, RngNoCvref, std::enable_if_t<
			is_range_with_iterators<Rng>::value && !has_value_type<RngNoCvref>::value,
			tc::void_t<tc::decay_t<tc::range_reference_t<Rng>>>
		>> final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
			STATICASSERTSAME(type, typename std::iterator_traits<typename boost::range_iterator<Rng>::type>::value_type);
		};

		template< typename Rng, typename RngNoCvref >
		struct range_value<Rng, RngNoCvref, std::enable_if_t<!is_range_with_iterators<Rng>::value && has_value_type<RngNoCvref>::value>> final {
			using type = typename RngNoCvref::value_type;
		};

		template< typename Rng, typename RngNoCvref >
		struct range_value<Rng, RngNoCvref, std::enable_if_t<
			!is_range_with_iterators<Rng>::value && !has_value_type<RngNoCvref>::value,
			tc::void_t<tc::decay_t<tc::range_reference_t<Rng>>>
		>> final {
			using type = tc::decay_t<tc::range_reference_t<Rng>>;
		};
	}
	using no_adl::range_value;

	template<typename T>
	using range_value_t = typename range_value<T>::type;

	namespace empty_range_adl {
		struct empty_range;
	}
	using empty_range_adl::empty_range;

	namespace no_adl {
		template<typename Rng, typename Enable=void>
		struct has_range_value /*final*/: std::false_type {};

		template<typename Rng>
		struct has_range_value<Rng, tc::void_t<tc::range_value_t<Rng>>> /*final*/: std::true_type {};
	}
	using no_adl::has_range_value;

	namespace no_adl {
		
		template<typename T, typename Enable=void>
		struct common_range_value_base {};

		template<typename... TValue>
		struct common_range_value_base<tc::type::list<TValue...>, tc::void_t<tc::common_type_decayed_t<TValue...>>> {
			using type = tc::common_type_decayed_t<TValue...>;
		};

		template<typename T, typename Enable=void>
		struct common_range_value_filtered {};

		template<typename... Rng>
		struct common_range_value_filtered<tc::type::list<Rng...>, std::enable_if_t<std::conjunction<tc::has_range_value<Rng>...>::value>>: common_range_value_base<tc::type::list<tc::range_value_t<Rng>...>> {};

		template<typename... Rng>
		struct common_range_value final: common_range_value_filtered<typename tc::type::partition<tc::type::list<tc::remove_cvref_t<Rng>...>, tc::type::rcurry<tc::is_safely_convertible, tc::empty_range>::template type>::false_part> {};
	};
	

	template<typename... Rng>
	using common_range_value_t = typename no_adl::common_range_value<Rng...>::type;

	namespace no_adl {
		template<typename Value, typename Func>
		struct [[nodiscard]] FuncWithValue : tc::decay_t<Func> {
			using value_type = Value;
		};
	}

	template<typename Value, typename Func>
	constexpr auto generator_range_value(Func&& func) noexcept {
		return no_adl::FuncWithValue<Value, Func>{std::forward<Func>(func)};
	}

	namespace no_adl {
		template<typename Rng, typename>
		struct constexpr_size_base {};

		template<typename T, std::size_t N>
		struct constexpr_size_base<T[N], void> : std::integral_constant<std::size_t, N - tc::is_char<T>::value> {};
	}
	template<typename Rng>
	using constexpr_size = no_adl::constexpr_size_base<tc::remove_cvref_t<Rng>>;

	namespace no_adl {
		template<typename Rng, typename = void>
		struct has_constexpr_size : std::false_type {};

		template<typename Rng>
		struct has_constexpr_size<Rng, std::void_t<typename tc::constexpr_size<Rng>::type>> : std::true_type {};
	}
	using no_adl::has_constexpr_size;

	namespace begin_end_adl {
		DEFINE_ADL_TAG_TYPE(begin_end_tag);

		template<typename T, std::size_t N>
		constexpr T* begin(begin_end_tag_t, T (&at)[N]) noexcept {
			return at;
		}
		template<typename T, std::size_t N>
		constexpr T const* begin(begin_end_tag_t, T const (&at)[N]) noexcept {
			return at;
		}
		template<typename T, std::size_t N>
		constexpr T* end(begin_end_tag_t, T (&at)[N]) noexcept {
			if constexpr( tc::is_char<T>::value ) {
				auto const nSize = tc::strlen(at);
				_ASSERTE( tc::constexpr_size<T[N]>::value == nSize );
				return at+nSize;
			} else {
				return at+tc::constexpr_size<T[N]>::value;
			}
		}
		template<typename T, std::size_t N>
		constexpr T const* end(begin_end_tag_t, T const (&at)[N]) noexcept {
			if constexpr( tc::is_char<T>::value ) {
				auto const nSize = tc::strlen(at);
				_ASSERTE( tc::constexpr_size<T[N]>::value == nSize );
				return at+nSize;
			} else {
				return at+tc::constexpr_size<T[N]>::value;
			}
		}

		template<typename It>
		constexpr It begin(begin_end_tag_t, std::pair<It,It> const& pairit) noexcept {
			return pairit.first;
		}
		template<typename It>
		constexpr It end(begin_end_tag_t, std::pair<It,It> const& pairit) noexcept {
			return pairit.second;
		}
	}
}

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
namespace tc::begin_end_adl { \
	/* Note: We cannot use overloading to differentiate xchar* and xchar(&)[N]. */ \
	template<typename CharPtrConvertible, std::enable_if_t< \
		tc::is_safely_convertible<CharPtrConvertible, xchar*>::value \
			&& !tc::has_constexpr_size<CharPtrConvertible>::value \
	>* = nullptr> \
	constexpr xchar* begin(begin_end_tag_t, CharPtrConvertible&& pchc) noexcept { \
		return pchc; \
	} \
	template<typename CharPtrConvertible, std::enable_if_t< \
		tc::is_safely_convertible<CharPtrConvertible, xchar*>::value \
			&& !tc::has_constexpr_size<CharPtrConvertible>::value \
	>* = nullptr> \
	constexpr xchar* end(begin_end_tag_t, CharPtrConvertible&& pchc) noexcept { \
		xchar* pch = pchc; \
		return pch+tc::strlen(pch); \
	} \
	template<typename CharPtrConvertible, std::enable_if_t< \
		tc::is_safely_convertible<CharPtrConvertible, xchar const*>::value \
			&& !tc::is_safely_convertible<CharPtrConvertible, xchar*>::value \
			&& !tc::has_constexpr_size<CharPtrConvertible>::value \
	>* = nullptr> \
	constexpr xchar const* begin(begin_end_tag_t, CharPtrConvertible&& pchc) noexcept { \
		return pchc; \
	} \
	template<typename CharPtrConvertible, std::enable_if_t< \
		tc::is_safely_convertible<CharPtrConvertible, xchar const*>::value \
			&& !tc::is_safely_convertible<CharPtrConvertible, xchar*>::value \
			&& !tc::has_constexpr_size<CharPtrConvertible>::value \
	>* = nullptr> \
	constexpr xchar const* end(begin_end_tag_t, CharPtrConvertible&& pchc) noexcept { \
		xchar const* pch = pchc; \
		return pch+tc::strlen(pch); \
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

namespace tc{
	namespace no_adl {
		template<template<typename...> typename TTrait, typename Rng, typename=void>
		struct is_range_of final: std::false_type {};

		template<template<typename...> typename TTrait, typename Rng>
		struct is_range_of<TTrait, Rng, std::enable_if_t<TTrait<tc::range_value_t<Rng>>::value>> final: std::true_type {};
	}
	using no_adl::is_range_of;
}

namespace tc_begin_end_no_adl {
	template< typename Rng >
	auto adl_begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free begin found by ADL
		begin( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto adl_end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free end found by ADL
		end( std::forward<Rng>(rng) )
	)
}

namespace tc {
	template< typename Rng >
	[[nodiscard]] constexpr auto begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member begin
		std::forward<Rng>(rng).begin()
	)

/*		template< typename Rng >
	[[nodiscard]] auto begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free begin found by ADL
		tc_begin_end_no_adl::adl_begin( std::forward<Rng>(rng) )
	)*/

	template< typename Rng >
	[[nodiscard]] constexpr auto begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free begin found by tag
		begin( begin_end_adl::begin_end_tag, std::forward<Rng>(rng) )
	)

	template< typename Rng >
	[[nodiscard]] constexpr auto end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member end
		std::forward<Rng>(rng).end()
	)

/*		template< typename Rng >
	[[nodiscard]] auto end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free end found by ADL
		tc_begin_end_no_adl::adl_end( std::forward<Rng>(rng) )
	)*/

	template< typename Rng >
	[[nodiscard]] constexpr auto end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free end found by tag
		end( begin_end_adl::begin_end_tag, std::forward<Rng>(rng) )
	)

	template< typename Rng >
	[[nodiscard]] auto cbegin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member begin
		tc::begin(static_cast<Rng const&&>(rng))
	)

	template< typename Rng >
	[[nodiscard]] auto cend(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member begin
		tc::end(static_cast<Rng const&&>(rng))
	)

	template< typename Rng >
	[[nodiscard]] auto cbegin(Rng& rng) return_decltype_MAYTHROW(
		// Rng has member begin
		tc::begin(static_cast<Rng const&>(rng))
	)

	template< typename Rng >
	[[nodiscard]] auto cend(Rng& rng) return_decltype_MAYTHROW(
		// Rng has member begin
		tc::end(static_cast<Rng const&>(rng))
	)
	
	template <typename Rng>
	auto cyclic_next(typename boost::range_iterator<std::remove_reference_t<Rng>>::type it, Rng&& rng) noexcept {
		++it;
		return tc::end(rng)==it ? tc::begin(std::forward<Rng>(rng)) : it;
	}
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
	auto begin(Rng&& rng) return_decltype_MAYTHROW(
		tc::begin( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto end(Rng&& rng) return_decltype_MAYTHROW(
		tc::end( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto const_begin(Rng&& rng) return_decltype_MAYTHROW(
		tc::begin( std::forward<Rng>(rng) )
	)

	template< typename Rng >
	auto const_end(Rng&& rng) return_decltype_MAYTHROW(
		tc::end( std::forward<Rng>(rng) )
	)
}
