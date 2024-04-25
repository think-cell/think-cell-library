
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits_fwd.h"

namespace tc {
	namespace literal_range_adl {
		template <typename T, auto... Ts>
		struct literal_range;
	}
	using literal_range_adl::literal_range;

	namespace no_adl {
		template <typename Char, std::size_t N> requires tc::char_like<Char> && (!std::is_volatile<Char>::value)
		struct string_template_param final {
			consteval string_template_param(Char const (&str)[N]) noexcept {
				for(int i=0; i<N; ++i) m_str[i]=str[i]; // avoid using library functions to prevent circular dependency on _ASSERT
			}
			template <auto ... Cs>
			consteval string_template_param(literal_range<Char, Cs...>) noexcept {
				static_assert(sizeof...(Cs) + 1 == N);
				std::size_t idx = 0;
				((m_str[idx++] = Cs), ...); // avoid using library functions to prevent circular dependency on _ASSERT
				m_str[idx] = Char();
			}

			constexpr auto begin() const& noexcept {
				return &m_str[0];
			}

			constexpr auto end() const& noexcept {
				return &m_str[N-1];
			}

			static constexpr auto size() noexcept {
				return tc::least_uint_constant<N-1>{};
			}

			constexpr operator auto const&() const& noexcept {
				return m_str;
			}

			constexpr Char operator[](std::size_t idx) const& noexcept {
				return m_str[idx];
			}

			Char m_str[N];
		};

		template <typename Char, auto ... Cs>
		string_template_param(literal_range<Char, Cs...>) -> string_template_param<Char, sizeof...(Cs) + 1>;
	}
	using no_adl::string_template_param;
}
