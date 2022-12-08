
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/type_traits_fwd.h"
#include "../base/functors.h"
#include "../base/tag_type.h"
#include "../container/string.h"
#include "range_fwd.h"

#include <type_traits>

namespace tc {
	namespace begin_end_adl {
		DEFINE_ADL_TAG_TYPE(begin_end_tag);
	}

	template<typename Rng>
	[[nodiscard]] constexpr auto begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member begin
		std::forward<Rng>(rng).begin()
	)

	template<typename Rng>
	[[nodiscard]] constexpr auto begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free begin found by tag
		begin(begin_end_adl::begin_end_tag, std::forward<Rng>(rng))
	)

	template<typename Rng>
	[[nodiscard]] constexpr auto end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member end
		std::forward<Rng>(rng).end()
	)

	template<typename Rng>
	[[nodiscard]] constexpr auto end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free end found by tag
		end(begin_end_adl::begin_end_tag, std::forward<Rng>(rng))
	)

	template<typename Rng>
	using iterator_t = decltype(tc::begin(std::declval<Rng&>()));
	template<typename Rng>
	using sentinel_t = decltype(tc::end(std::declval<Rng&>()));
	template<typename Rng>
	concept common_range = std::same_as<tc::iterator_t<Rng>, tc::sentinel_t<Rng>>;


	template<typename It> // TODO C++20 Use std::iter_reference_t
	using iter_reference_t = decltype(*std::declval<It&>());

	namespace range_output_t_adl {
		DEFINE_ADL_TAG_TYPE(adl_tag);

		template<typename T, T... t>
		auto range_output_t_impl(adl_tag_t, std::integer_sequence<T, t...> const&) -> tc::type::list<T>; // declaration only
	}

	namespace range_output_no_adl {
		template<typename Rng>
		struct from_iter_reference {};

		template<typename Rng> requires (!std::is_void<tc::iter_reference_t<tc::iterator_t<Rng>>>::value)
		struct from_iter_reference<Rng> {
			using type = tc::type::list<tc::remove_rvalue_reference_t<tc::iter_reference_t<tc::iterator_t<Rng>>>>;
		};

		template<typename Rng>
		struct from_adl_with_tag : from_iter_reference<Rng> {};

		template<typename Rng> requires requires { /*adl*/range_output_t_impl(tc::range_output_t_adl::adl_tag, std::declval<Rng>()); }
		struct from_adl_with_tag<Rng> {
			using type = decltype(/*adl*/range_output_t_impl(tc::range_output_t_adl::adl_tag, std::declval<Rng>()));
			static_assert(tc::decayed<type> && tc::is_instance<tc::type::list, type>::value);
			static_assert(!tc::type::any_of<type, std::is_rvalue_reference>::value);
		};

		template<typename Rng>
		struct from_adl final : from_adl_with_tag<Rng> {};

		template<typename Rng> requires requires { /*adl*/range_output_t_impl(std::declval<Rng>()); }
		struct from_adl<Rng> {
			using type = decltype(/*adl*/range_output_t_impl(std::declval<Rng>()));
			static_assert(tc::decayed<type> && tc::is_instance<tc::type::list, type>::value);
			static_assert(!tc::type::any_of<type, std::is_rvalue_reference>::value);
		};
	}
	template<typename Rng>
	using range_output_t = typename range_output_no_adl::from_adl<Rng>::type;

	namespace FuncWithOutput_adl {
		template<typename Func, typename... T>
		struct [[nodiscard]] FuncWithOutput : tc::decay_t<Func> {
			friend auto range_output_t_impl(FuncWithOutput const&) -> tc::type::list<T...>; // declaration only
		};

		template<typename Func, typename... T>
		struct [[nodiscard]] FuncWithOutput<Func, tc::type::list<T...>> : FuncWithOutput<Func, T...> {};
	}

	template<typename... T, typename Func>
	constexpr auto generator_range_output(Func&& func) noexcept {
		return FuncWithOutput_adl::FuncWithOutput<Func, T...>{std::forward<Func>(func)};
	}

	namespace no_adl {
		template<typename T>
		struct value_type_impl {
			using type = T;
		};
	}
	template<typename T>
	using value_t = typename tc::no_adl::value_type_impl<tc::decay_t<T>>::type;

	namespace range_value_no_adl {
		template<typename Rng, typename = void>
		struct from_range_output final { using type = void; };

		template<typename Rng>
		struct from_range_output<Rng, std::void_t<tc::type::apply_t<tc::common_type_decayed_t, tc::type::transform_t<tc::range_output_t<Rng>, tc::value_t>>>> final {
			using type = tc::type::apply_t<tc::common_type_decayed_t, tc::type::transform_t<tc::range_output_t<Rng>, tc::value_t>>;
		};

		template<typename Rng, typename = void>
		struct from_cont final { using type = void; };

		template<typename Cont>
		struct from_cont<Cont, std::void_t<typename std::remove_reference_t<Cont>::value_type>> final {
			using type = typename std::remove_reference_t<Cont>::value_type;
		};

		template<typename Rng, typename ValueTypeFromOutput = typename from_range_output<Rng>::type, typename ValueTypeFromCont = typename from_cont<Rng>::type>
		struct range_value final {
			// STATICASSERTSAME(ValueTypeFromOutput, ValueTypeFromCont); fires for Rng=std::array<tc::tuple<int&>, 1> (tuple<int> or tuple<int&>) or Rng=boost::filesystem::path (path or wchar_t?).
			// In both cases, not defining range_value_t is sensible.
		};

		template<typename Rng, typename ValueType>
		struct range_value<Rng, ValueType, ValueType> final {
			using type = ValueType;
		};

		template<typename Rng, typename ValueTypeFromOutput>
		struct range_value<Rng, ValueTypeFromOutput, void> final {
			using type = ValueTypeFromOutput;
		};

		template<typename Rng, typename ValueTypeFromCont>
		struct range_value<Rng, void, ValueTypeFromCont> final {
			using type = ValueTypeFromCont;
		};

		template<typename Rng>
		struct range_value<Rng, void, void> final {}; // range_value_t is not void
	}
	template<typename Rng>
	using range_value_t = typename tc::range_value_no_adl::range_value<Rng>::type;

	namespace no_adl {
		template<typename Rng, typename Enable=void>
		struct has_range_value /*final*/: tc::constant<false> {};

		template<typename Rng>
		struct has_range_value<Rng, tc::void_t<tc::range_value_t<Rng>>> /*final*/: tc::constant<true> {};
	}
	using no_adl::has_range_value;

	namespace no_adl {
		template<typename Rng, typename>
		struct constexpr_size_base {};

		template<typename T, std::size_t N>
		struct constexpr_size_base<T[N], void> : tc::constant<N - (tc::is_char<T>::value ? 1 : 0)> {};

		template<typename T, T... t>
		struct constexpr_size_base<std::integer_sequence<T, t...>, void> : tc::constant<sizeof...(t)> {};
	}
	template<typename Rng>
	using constexpr_size = no_adl::constexpr_size_base<std::remove_cvref_t<Rng>>;

	namespace no_adl {
		template<typename Rng, typename = void>
		struct has_constexpr_size : tc::constant<false> {};

		template<typename Rng>
		struct has_constexpr_size<Rng, std::void_t<typename tc::constexpr_size<Rng>::type>> : tc::constant<true> {};
	}
	using no_adl::has_constexpr_size;

	template<typename T>
	[[nodiscard]] constexpr std::size_t strlen( T const* pt ) noexcept {
		_ASSERTNORETURN(pt);
		return std::char_traits<T>::length(pt);
	}

	namespace zero_termination_sentinel_adl {
		struct zero_termination_sentinel final {};

		template<typename Char> requires tc::is_char<Char>::value
		constexpr bool operator==(zero_termination_sentinel, Char const* psz) noexcept {
			return static_cast<Char>(0) == *psz;
		}
	}
	using zero_termination_sentinel_adl::zero_termination_sentinel;

	namespace nullptr_or_zero_termination_sentinel_adl {
		struct nullptr_or_zero_termination_sentinel final {};

		template<typename Char> requires tc::is_char<Char>::value
		constexpr bool operator==(nullptr_or_zero_termination_sentinel, Char const* psz) noexcept {
			return !psz || static_cast<Char>(0) == *psz;
		}
	}
	using nullptr_or_zero_termination_sentinel_adl::nullptr_or_zero_termination_sentinel;

	namespace begin_end_adl {
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

		template<typename It, typename Enable = decltype(*++std::declval<It&>())>
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
namespace tc { \
	namespace begin_end_adl { \
		/* Note: We cannot use overloading to differentiate xchar* and xchar(&)[N]. */ \
		template<typename CharPtrConvertible> requires \
			tc::is_safely_convertible<CharPtrConvertible, xchar*>::value \
				&& (!tc::has_constexpr_size<CharPtrConvertible>::value) \
		constexpr xchar* begin(begin_end_tag_t, CharPtrConvertible&& pchc) noexcept { \
			return pchc; \
		} \
		template<typename CharPtrConvertible> requires \
			tc::is_safely_convertible<CharPtrConvertible, xchar const*>::value \
				&& (!tc::is_safely_convertible<CharPtrConvertible, xchar*>::value) \
				&& (!tc::has_constexpr_size<CharPtrConvertible>::value) \
		constexpr xchar const* begin(begin_end_tag_t, CharPtrConvertible&& pchc) noexcept { \
			return pchc; \
		} \
		template<typename CharPtrConvertible> requires \
			tc::is_safely_convertible<CharPtrConvertible, xchar const*>::value \
				&& (!tc::has_constexpr_size<CharPtrConvertible>::value) \
		constexpr tc::zero_termination_sentinel end(begin_end_tag_t, CharPtrConvertible&& pchc) noexcept { \
			_ASSERT( pchc ); \
			return {}; \
		} \
	} \
}

CHAR_RANGE(char)
CHAR_RANGE(wchar_t)
CHAR_RANGE(char16_t)
CHAR_RANGE(char32_t)

namespace tc{
	namespace no_adl {
		template<template<typename...> typename TTrait, typename Rng>
		struct is_range_of final: tc::constant<false> {};

		template<template<typename...> typename TTrait, typename Rng> requires TTrait<tc::range_value_t<Rng>>::value
		struct is_range_of<TTrait, Rng> final: tc::constant<true> {};
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
	template <typename Rng>
	auto cyclic_next(tc::iterator_t<Rng> it, Rng&& rng) noexcept {
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
