
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits.h"
#include "casts.h"
#include "implements_compare.h"
#include "binary_operators.h"
#include "utility.h"
#include "meta.h"
#include "explicit_cast.h"

namespace tc {
	namespace char_restrictive_adl {
		template<typename Char, typename Pred>
		struct char_restrictive /* not final */: tc::implements_compare<char_restrictive<Char, Pred>> {
			using char_type = Char;
			static_assert(tc::is_decayed<char_type>::value);
			static_assert(tc::is_char<char_type>::value);
		protected:
			char_type m_ch;
		public:
			constexpr char_restrictive() noexcept = default;

			template<typename T, typename=decltype(Pred::check(std::declval<T>()))>
			explicit constexpr char_restrictive(T ch) noexcept: MEMBER_INIT_CAST( m_ch, ch ) {
				_ASSERTE(Pred::check(ch));
			}

			template<typename T, std::enable_if_t<std::is_same<T, char_type>::value>* = nullptr>
			constexpr operator T() const& noexcept { return m_ch; }

			friend constexpr tc::order compare_impl(char_restrictive const& lhs, char_restrictive const& rhs) noexcept {
				return tc::compare(lhs.m_ch, rhs.m_ch);
			}			
		};

		template<typename Char, typename Pred>
		constexpr auto underlying_cast_impl(char_restrictive<Char, Pred> const& ch) return_decltype_noexcept(
			tc::underlying_cast(tc::implicit_cast<Char>(ch))
		)
	}
	using char_restrictive_adl::char_restrictive;

	namespace no_adl {
		template<typename T, typename Enable=void>
		struct is_char_like_impl: tc::is_char<T> {};

		template<typename T>
		struct is_char_like_impl<T, std::enable_if_t<tc::is_instance_or_derived<tc::char_restrictive, T>::value>>: std::true_type {};

		template<typename T>
		using is_char_like = is_char_like_impl<std::remove_cv_t<T>>;
	}
	using no_adl::is_char_like;

	namespace char_ascii_adl {
		struct pred_char_ascii final {
			template<typename Char, std::enable_if_t<tc::is_char<Char>::value>* = nullptr>
			static constexpr bool check(Char ch) {
				return 0<=tc::underlying_cast(ch) && tc::underlying_cast(ch)<=127;
			}
		};

		struct char_ascii final: tc::char_restrictive<char, pred_char_ascii>, tc::additive<> {
			using base_ = tc::char_restrictive<char, pred_char_ascii>;
			using base_::base_;

			using base_::operator char;
			template<typename T, std::enable_if_t<tc::is_char<T>::value && !std::is_same<T, char>::value>* = nullptr>
			constexpr operator T() const& {
				return tc::explicit_cast<T>(m_ch);
			}

			template<typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* = nullptr>
			constexpr char_ascii& operator+=(T n) noexcept {
				_ASSERTE(tc::cmp_less_equal(0-static_cast<int>(m_ch), n) && tc::cmp_less_equal(n, 127-static_cast<int>(m_ch)));
				m_ch = static_cast<char>(m_ch+n);
				return *this;
			}

			template<typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* = nullptr>
			constexpr char_ascii& operator-=(T n) noexcept {
				_ASSERTE(tc::cmp_less_equal(static_cast<int>(m_ch)-127, n) && tc::cmp_less_equal(n, static_cast<int>(m_ch)));
				m_ch = static_cast<char>(m_ch-n);
				return *this;
			}
		};

		template<typename T, std::enable_if_t<tc::is_char<T>::value>* = nullptr>
		constexpr auto operator==(char_ascii const& lhs, T const rhs) return_decltype_noexcept(
			tc::underlying_cast(lhs)==tc::underlying_cast(rhs)
		)

		template<typename T, std::enable_if_t<tc::is_char<T>::value>* = nullptr>
		constexpr auto operator==(T const lhs, char_ascii const& rhs) return_decltype_noexcept(
			tc::underlying_cast(lhs)==tc::underlying_cast(rhs)
		)
	}
	using char_ascii_adl::char_ascii;

	namespace no_adl {
		template<>
		struct char_limits<tc::char_ascii> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 1;
			static constexpr bool in_range(unsigned int n) noexcept {
				return n <= 0x7f;
			}
		};
	}

	namespace no_adl {
#pragma push_macro("CUSTOMIZE_COMMON_TYPE_CHAR_ASCII")
#define CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char_type) \
		template<> \
		struct common_type_decayed<tc::char_ascii, char_type> { \
			using type = char_type; \
		}; \
		template<> \
		struct common_type_decayed<char_type, tc::char_ascii> { \
			using type = char_type; \
		};

		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char)
		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(wchar_t)
		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char16_t)
		CUSTOMIZE_COMMON_TYPE_CHAR_ASCII(char32_t)
#pragma pop_macro("CUSTOMIZE_COMMON_TYPE_CHAR_ASCII")
	}

	namespace no_adl {
		template<typename TCharRestrictive>
		struct char_restrictive_traits /* not final */ {
			static_assert(tc::is_instance_or_derived<tc::char_restrictive, TCharRestrictive>::value);
			using char_type = TCharRestrictive;
			using int_type = typename std::char_traits<typename TCharRestrictive::char_type>::int_type;
			using off_type = typename std::char_traits<typename TCharRestrictive::char_type>::off_type;
			using pos_type = typename std::char_traits<typename TCharRestrictive::char_type>::pos_type;
			using state_type = typename std::char_traits<typename TCharRestrictive::char_type>::state_type;

			static constexpr void assign(char_type& r, const char_type& a) noexcept {
				r = a;
			}

			static char_type* assign(char_type* p, std::size_t count, char_type a) {
				std::fill(p, p+count, a);
				return p;
			}

			static constexpr bool eq(char_type a, char_type b) noexcept {
				return a == b;
			}

			static constexpr bool lt(char_type a, char_type b) noexcept {
				return a < b;
			}

			static constexpr std::size_t length(const char_type* s) {
				std::size_t n = 0;
				if(s) {
					while(char_type() != *s++)
						++n;
				}
				return n;
			}

			static constexpr const char_type* find(const char_type* p, std::size_t count, const char_type& ch) {
				while(0<count) {
					if(*p == ch) {
						return p;
					} else {
						++p;
						--count;
					}
				}
				return nullptr;
			}

			static constexpr int compare(const char_type* s1, const char_type* s2, std::size_t count) {
				for(std::size_t i=0; i<count; ++i) {
					if (*s1<*s2) return -1;
					else if (*s2<*s1) return 1;
				}
				return 0;
			}

			static char_type* move(char_type* dest, const char_type* src, std::size_t count) {
				if (src <= dest && dest < src+count) {
					std::copy_backward(src, src+count, dest+count);
				} else {
					std::copy(src, src+count, dest);
				}
				return dest;
			}

			static char_type* copy(char_type* dest, const char_type* src, std::size_t count) {
				return move(dest, src, count); // copy works correctly just like move in clang and gcc's implementation
			}
		};
	}
}

namespace std {
	template<typename Char, typename Pred>
	struct char_traits<tc::char_restrictive<Char, Pred>>: tc::no_adl::char_restrictive_traits<tc::char_restrictive<Char, Pred>> {};

	template<>
	struct char_traits<tc::char_ascii>: tc::no_adl::char_restrictive_traits<tc::char_ascii> {};
}

CHAR_RANGE(tc::char_ascii)

#define ASCIISTRVAL(str) tc::make_array<tc::char_ascii>(str)  // avoid copy
#define ASCIISTR(str) as_constexpr(ASCIISTRVAL(str))
