
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits_fwd.h"

namespace tc {
	namespace no_adl {
		template<typename Char, std::size_t N> requires tc::char_like<Char> && (!std::is_volatile<Char>::value)
		struct string_template_param final {
			consteval string_template_param(Char const (&str)[N]) noexcept {
				for(int i=0; i<N; ++i) m_str[i]=str[i]; // avoid using library functions to prevent circular dependency on _ASSERT
			}

			constexpr auto begin() const& noexcept {
				return &m_str[0];
			}

			constexpr auto end() const& noexcept {
				return &m_str[N-1];
			}

			constexpr operator auto const&() const& noexcept {
				return m_str;
			}

			using iterator = Char const*;
			using const_iterator = iterator;
			using value_type = Char;
			using difference_type = std::ptrdiff_t;
			using size_type = std::size_t;

			Char m_str[N];
		};

		template<typename Rng> struct constexpr_size_impl; // prevent circular dependency

		template<typename Char, std::size_t N>
		struct constexpr_size_impl<string_template_param<Char, N>> : tc::constant<N-1> {};
	}
	using no_adl::string_template_param;
}
