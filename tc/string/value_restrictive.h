
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/type_traits.h"
#include "../base/utility.h"
#include "../base/casts.h"
#include "../base/explicit_cast.h"
#include "../array.h"
#include "../algorithm/binary_operators.h"
#include "../range/meta.h"

namespace tc {
	// Code unit predicates
	//
	// These names are based on the terminology used in the Unicode Standard: https://www.unicode.org/versions/Unicode13.0.0/UnicodeStandard-13.0.pdf
	// "(...) reserved for the first, or leading, element of a UTF-8 code unit sequences, (...) reserved for the subsequent, or trailing, elements of such sequences;"
	// "(...) for the lead, trail, or single code units in any of those encoding forms overlap."
	[[nodiscard]] constexpr bool is_leading_codeunit(char const ch) noexcept {
		return 0xc0 == (ch & 0xc0); // matches 11xxxxxx
	}

	[[nodiscard]] constexpr bool is_leading_codeunit(tc::char16 const ch) noexcept {
		return 0xd800u == (ch & 0xfc00u); // high surrogate
	}

	[[nodiscard]] constexpr bool is_trailing_codeunit(char const ch) noexcept {
		return 0x80 == (ch & 0xc0); // matches 10xxxxxx
	}

	[[nodiscard]] constexpr bool is_trailing_codeunit(tc::char16 const ch) noexcept {
		return 0xdc00u == (ch & 0xfc00u); // low surrogate
	}

	namespace value_restrictive_adl {
		template<typename TTarget, typename TSource, TSource tFirst, TSource tLast>
		struct is_char_and_interval_is_in_range {
			static_assert(tFirst <= tLast);

		private:
			struct interval_is_in_range { // Code units can be directly compared when ALL allowed code points can be represented by one of the smallest code unit type.
				static constexpr bool value = tc::char_limits<TTarget>::interval_in_range(tFirst, tLast); // interval_in_range guarantees that t is a code point for all t in [tFirst, tLast]
			};

		public:
			static constexpr bool value = std::conjunction<tc::is_char<TTarget>, interval_is_in_range>::value;
		};

		template<typename T, T tFirst, T tLast>
		struct value_restrictive final : tc::additive<> {
		// private: TODO: inhibits usage as template parameter
			static_assert(tc::decayed<T>);
			static_assert(std::is_enum<T>::value || is_char_and_interval_is_in_range<T, T, tFirst, tLast>::value);
			static_assert(tFirst <= tLast);

			template<typename U, U uFirst, U uLast>
			friend struct value_restrictive;

			T m_t;

		public:
			static constexpr T c_tFirst = tFirst;
			static constexpr T c_tLast = tLast;

			constexpr value_restrictive() noexcept = default;

			template<typename U, U uFirst, U uLast> requires (c_tFirst <= uFirst) && (uLast <= c_tLast)
			constexpr value_restrictive(value_restrictive<U, uFirst, uLast> const ch) noexcept : m_t(ch.m_t) {} // Implicit ctor from a more restrictive type

			template<typename U, U uFirst, U uLast>
			explicit constexpr value_restrictive(value_restrictive<U, uFirst, uLast> const ch) noexcept : value_restrictive(ch.m_t) {} // Delegate to explicit ctor from data type

			template<typename U> requires tc::is_explicit_castable<T, U const&>::value
			explicit constexpr value_restrictive(U const u) noexcept : MEMBER_INIT_CAST(m_t, u) {
				_ASSERTE(tc::underlying_cast(c_tFirst) <= tc::underlying_cast(u) && tc::underlying_cast(u) <= tc::underlying_cast(c_tLast));
			}

			template<typename Rhs>
			constexpr value_restrictive& operator=(Rhs const& rhs) & noexcept { // Needed for spirit: it needs to assign to a value_restrictive attribute
				tc::renew(*this, rhs); // Go through the ctors
				return *this;
			}

			template<typename U> requires std::is_same<T, U>::value || is_char_and_interval_is_in_range<U, T, c_tFirst, c_tLast>::value
			constexpr operator U() const& noexcept {
				return m_t;
			}

			template<typename Integral> requires tc::is_actual_integer<Integral>::value
			constexpr value_restrictive& operator+=(Integral const n) noexcept {
				_ASSERTE(tc::cmp_less_equal(c_tFirst - m_t, n) && tc::cmp_less_equal(n, c_tLast - m_t));
				m_t = static_cast<T>(m_t + n);
				return *this;
			}

			template<typename Integral> requires tc::is_actual_integer<Integral>::value
			constexpr value_restrictive& operator-=(Integral const n) noexcept {
				_ASSERTE(tc::cmp_less_equal(m_t - c_tLast, n) && tc::cmp_less_equal(n, m_t - c_tFirst));
				m_t = static_cast<T>(m_t - n);
				return *this;
			}
		};

		template<typename T, T tFirst, T tLast>
		constexpr auto underlying_cast_impl(value_restrictive<T, tFirst, tLast> const& t) return_decltype_noexcept(
			tc::underlying_cast(tc::implicit_cast<T>(t))
		)

#pragma push_macro("DEFINE_COMPARISON_OP")
#define DEFINE_COMPARISON_OP(op) \
		template<typename T, T tFirst, T tLast, typename U, std::enable_if_t<std::is_same<T, U>::value || is_char_and_interval_is_in_range<U, T, tFirst, tLast>::value>* = nullptr> \
		constexpr auto operator op(value_restrictive<T, tFirst, tLast> const& lhs, U const& rhs) return_decltype_noexcept( \
			static_cast<U>(tc::implicit_cast<T>(lhs)) op rhs \
		) \
		template<typename T, T tFirst, T tLast, typename U, U uFirst, U uLast, std::enable_if_t<std::is_same<T, U>::value>* = nullptr> \
		constexpr auto operator op(value_restrictive<T, tFirst, tLast> const& lhs, value_restrictive<U, uFirst, uLast> const& rhs) return_decltype_noexcept( \
			tc::implicit_cast<T>(lhs) op tc::implicit_cast<T>(rhs) \
		) \
		template<typename T, T tFirst, T tLast, typename U, U uFirst, U uLast, std::enable_if_t<!std::is_same<T, U>::value && tc::is_char<T>::value && tc::is_char<U>::value>* = nullptr> \
		constexpr auto operator op(value_restrictive<T, tFirst, tLast> const& lhs, value_restrictive<U, uFirst, uLast> const& rhs) return_decltype_noexcept( \
			static_cast<char32_t>(tc::implicit_cast<T>(lhs)) op static_cast<char32_t>(tc::implicit_cast<T>(rhs)) \
		) \

		DEFINE_COMPARISON_OP(==)
		DEFINE_COMPARISON_OP(<=>)
#pragma pop_macro("DEFINE_COMPARISON_OP")
	}
	using value_restrictive_adl::value_restrictive;

	template <typename Char, Char chFirst, Char chLast>
	[[nodiscard]] constexpr bool is_trailing_codeunit(tc::value_restrictive<Char, chFirst, chLast> const ch) noexcept {
		return false;
	}

	namespace no_adl {
		template<typename T, T tFirst, T tLast>
		struct is_char_like_impl<tc::value_restrictive<T, tFirst, tLast>> : tc::is_char_like<T> {};
	}

	using char_ascii = tc::value_restrictive<char, '\0', '\x7f'>;
	using char_asciidigit = tc::value_restrictive<char, '0', '9'>;
	using char_asciilower = tc::value_restrictive<char, 'a', 'z'>;
	using char_asciiupper = tc::value_restrictive<char, 'A', 'Z'>;

	namespace no_adl {
		template<typename Char, Char chFirst, Char chLast>
		struct char_limits<tc::value_restrictive<Char, chFirst, chLast>> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 1; // because we static_assert(is_single_codeunit())

			[[nodiscard]] static constexpr bool in_range(unsigned int n) noexcept {
				return chFirst <= n && n <= chLast;
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
		template<typename Char, Char chFirst, Char chLast>
		struct char_traits_selector<tc::value_restrictive<Char, chFirst, chLast>, void> final {
			struct type final {
				using char_type = tc::value_restrictive<Char, chFirst, chLast>;
				using int_type = typename tc::char_traits<Char>::int_type;
				using off_type = typename tc::char_traits<Char>::off_type;
				using pos_type = typename tc::char_traits<Char>::pos_type;
				using state_type = typename tc::char_traits<Char>::state_type;

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
		};
	}
}

#define ASCIISTRVAL(str) tc::make_array<tc::char_ascii>(str)  // avoid copy
#define ASCIISTR(str) as_constexpr(ASCIISTRVAL(str))
