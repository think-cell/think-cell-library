
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "meta.h"
#include "../base/casts.h"

namespace tc {
	namespace literal_range_detail {
		template <typename T, auto ... Ts>
		inline constexpr T const array[] = {T(Ts)...};

		template <tc::char_like T, auto ... Ts>
#if defined(__clang__) && !defined(_DEBUG)
		// We make it static, so it matches a string literal.
		// That way, compilers are able to de-duplicate it: https://godbolt.org/z/9hjjxrTnx
		// This de-duplicate only works with clang release build
		static
#else
		inline
#endif
		constexpr T const str[] = {T(Ts)..., T()};
	}

	namespace literal_range_adl {
		// Note that Ts... does not have the type of T but can be some other type, that can be used to construct T in the array initializer.
		// This keeps the instantiated name of the type small - for example, a value of tc::char_ascii is something like `tc::char_ascii<template-params>{tc::additive<>{}, value}`,
		// but if passed as `char` it is just `value`.
		template <typename T, auto... Ts>
		struct literal_range final {
			// The return type should be an array reference so we don't lose type information.
			// However, there is a bug in the current MSVC version, that doesn't allow decltype(auto): https://developercommunity.visualstudio.com/t/decltypeauto-strips-reference-of-array-types/218527
			static constexpr auto as_array() noexcept -> T const (&)[sizeof...(Ts) + (tc::char_like<T> ? 1 : 0)]{
				if constexpr (tc::char_like<T>) {
					return literal_range_detail::str<T, Ts...>;
				} else {
					return literal_range_detail::array<T, Ts...>;
				}
			}

			// Implicit conversion to C string, if it's a C string.
			template <std::same_as<T const*> Target>
			constexpr operator Target() const& noexcept
				requires tc::char_like<T> && (tc::strlen(literal_range::as_array()) == sizeof...(Ts))
			{
				return as_array(); // decays to pointer
			}

			constexpr T const* begin() const& noexcept {
				return as_array();
			}
			constexpr T const* end() const& noexcept {
				return as_array() + sizeof...(Ts);
			}

			static constexpr auto size() noexcept {
				return tc::least_uint_constant<sizeof...(Ts)>{};
			}
		};

		template <tc::char_like T, auto ... Ts>
		[[nodiscard]] constexpr auto as_c_str_impl(literal_range<T, Ts...> rng) return_decltype_noexcept(
			tc::implicit_cast<T const*>(rng)
		)
	}
	using literal_range_adl::literal_range;

	template <typename T, auto ... Ts>
	inline constexpr auto enable_borrowed_range<tc::literal_range<T, Ts...>> = true;

	template <auto ... Ts>
	constexpr literal_range<tc::common_type_t<decltype(Ts)...>, Ts...> literal_range_of = {};

	namespace literal_range_detail {
		template <typename T>
		constexpr auto is_literal_range = false;
		template <typename T, auto ... Ts>
		constexpr auto is_literal_range<literal_range<T, Ts...>> = true;
	}
	template <typename T>
	concept is_literal_range = literal_range_detail::is_literal_range<tc::decay_t<T>>;
}
