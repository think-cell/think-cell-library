
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/generic_macros.h"
#include "../algorithm/append.h"

namespace tc {
	namespace no_adl {
		template< typename Char, typename Rng, typename Enable = void >
		struct has_convertible_as_c_str final
		: tc::constant<false> {};

		template< typename Char, typename Rng >
		struct has_convertible_as_c_str< Char, Rng, std::enable_if_t<tc::safely_convertible_to<decltype(tc::as_c_str(std::declval<Rng>())), Char*>> > final
		: tc::constant<true> {};

		template< typename Char, typename Str >
		struct [[nodiscard]] make_c_str_impl final {
			explicit make_c_str_impl(Str&& str) noexcept: m_str(std::forward<Str>(str)) {}

			// We don't want make_c_str_impl to be able to implicitly cast to bool. Deleting operator bool() won't work because
			// a deleted function is still considered in overload resolution and will cause ambiguity between foo(bool) and foo(char const*).
			template<typename T> requires std::is_same<T, Char const*>::value
			operator T() const /*no &*/ noexcept {
				return tc::as_c_str(m_str);
			}

			template<typename T> requires std::is_same<T, Char*>::value
			operator T() /*no &*/ noexcept {
				return tc::as_c_str(m_str);
			}
			
			operator tc::span<Char const>() const /*no &*/ noexcept {
				return m_str;
			}
		private:
			Str m_str;
		};
	}

	/////////////////////////////////////////////////////////////////////////////////////////////////
	//  make_c_str(<Char>): create a value or reference holder which is castable to c string

	//   1. One input range, tc::as_c_str(rng) is valid and convertible to Char const*: hold the value or reference of the rng, castable to tc::as_c_str(rng)
	//   2. Otherwise: create and hold a tc::string<Char>, castable to Char const*
	//   3. Explicitly specified <Char> could be omitted if the char pointer type deduced from the first rng is convertible to destination c string
	//   4. make_mutable_c_str(<Char>): create a value or reference holder which is castable to mutable c string

	template< typename Char, typename Rng0 > requires tc::no_adl::has_convertible_as_c_str<Char const, Rng0>::value
	auto make_c_str(Rng0&& rng0) noexcept {
		static_assert(tc::decayed<Char>);
		return tc::no_adl::make_c_str_impl<Char const, Rng0>(std::forward<Rng0>(rng0));
	}

	template< typename Rng0 >
	auto make_c_str(Rng0&& rng0) return_decltype_noexcept(
		tc::make_c_str<std::remove_cv_t<typename std::pointer_traits<decltype(tc::as_c_str(std::declval<Rng0>()))>::element_type>>(std::forward<Rng0>(rng0))
	)

	template< typename Char, typename Rng0, typename... Rng >
	auto make_c_str(Rng0&& rng0, Rng&& ... rng) MAYTHROW {
		static_assert(tc::decayed<Char>);
		return tc::no_adl::make_c_str_impl<Char const, tc::string<Char>>(tc::make_str<Char>(std::forward<Rng0>(rng0), std::forward<Rng>(rng)...));
	}

	template< typename Rng0, typename... Rng >
	auto make_c_str(Rng0&& rng0, Rng&& ... rng) MAYTHROW {
		return tc::make_c_str<tc::range_value_t<decltype(tc::concat(std::declval<Rng0>(), std::declval<Rng>()...))>>(std::forward<Rng0>(rng0), std::forward<Rng>(rng)...);
	}

	template< typename Char, typename Rng0 > requires tc::no_adl::has_convertible_as_c_str<Char, Rng0>::value
	auto make_mutable_c_str(Rng0&& rng0) noexcept {
		static_assert(tc::decayed<Char>);
		return tc::no_adl::make_c_str_impl<Char, Rng0>(std::forward<Rng0>(rng0));
	}

	template< typename Rng0 >
	auto make_mutable_c_str(Rng0&& rng0) return_decltype_noexcept(
		tc::make_mutable_c_str<std::remove_cv_t<typename std::pointer_traits<decltype(tc::as_c_str(std::declval<Rng0>()))>::element_type>>(std::forward<Rng0>(rng0))
	)

	template< typename Char, typename Rng0, typename... Rng >
	auto make_mutable_c_str(Rng0&& rng0, Rng&& ... rng) MAYTHROW {
		static_assert(tc::decayed<Char>);
		return tc::no_adl::make_c_str_impl<Char, tc::string<Char>>(tc::make_str<Char>(std::forward<Rng0>(rng0), std::forward<Rng>(rng)...));
	}

	template< typename Rng0, typename... Rng >
	auto make_mutable_c_str(Rng0&& rng0, Rng&& ... rng) MAYTHROW {
		return tc::make_mutable_c_str<tc::range_value_t<decltype(tc::concat(std::declval<Rng0>(), std::declval<Rng>()...))>>(std::forward<Rng0>(rng0), std::forward<Rng>(rng)...);
	}
}

