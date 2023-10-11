
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/type_traits.h"
#include "base/safe_comparison.h"
#include "base/casts.h"
#include "base/explicit_cast.h"
#include "algorithm/binary_operators.h"

// restricted_enum restricts an enum or char type to a subinterval.
// It provides the following operations:
// * implicit (checked) conversion to/from base type
// * forwarding of to/from_underlying
// * full comparison
// Additionally, for types other than enum class, it also has affine arithmetic:
// * value += integer, value + integer
// * value -= integer, value - integer
// * value - value resulting in integer

namespace tc {
	namespace restricted_enum_adl {
		namespace detail {
			template <tc::char_type T>
			constexpr bool is_only_ascii(T, T tLast) noexcept
			{
				// No need to check tFirst, it's underlying is <= tLast and the underlying type is unsigned.
				return tc::to_underlying(tLast) <= 0x7F;
			}
			
			// Workaround clang by providing an overload for non-char types.
			// This will never actually be called as it is after a `tc::char_type<T>` check, but has to exist.
			template <typename T>
			constexpr std::false_type is_only_ascii(T, T) noexcept { return {}; }
		}

		template<typename T, T tFirst, T tLast>
		struct restricted_enum final : tc::additive<> {
		// private: TODO: inhibits usage as template parameter
			static_assert(tc::decayed<T>);
			static_assert(std::is_enum<T>::value || tc::char_type<T>);
			static_assert(tc::to_underlying(tFirst) <= tc::to_underlying(tLast));

			T m_t;

		public:
			static constexpr T c_tFirst = tFirst;
			static constexpr T c_tLast = tLast;

			constexpr restricted_enum() noexcept = default;

			template<typename U>
				requires tc::explicit_castable_from<T, U const&>
			static bool constructable_from(U const& u) noexcept {
				return restricted_enum(c_tFirst) <= u && u <= restricted_enum(c_tLast); // We reuse the SFINAE from the comparison operators of restricted_enum.
			}

			template<typename U>
				requires tc::explicit_castable_from<T, U const&>
			explicit constexpr restricted_enum(U const& u) noexcept : tc_member_init_cast(m_t, u) {
				_ASSERTE(tc::to_underlying(c_tFirst) <= tc::to_underlying(m_t) && tc::to_underlying(m_t) <= tc::to_underlying(c_tLast));
			}

			// Implicit ctor from a more restrictive type.
			template<typename U, U uFirst, U uLast>
				requires std::convertible_to<U, T> && (tc::cmp_less_equal(tc::to_underlying(c_tFirst), tc::to_underlying(uFirst)) && tc::cmp_less_equal(tc::to_underlying(uLast), tc::to_underlying(c_tLast)))
			constexpr restricted_enum(restricted_enum<U, uFirst, uLast> const rhs) noexcept : m_t(tc::implicit_cast<U>(rhs)) {}

			// Explicit ctor otherwise.
			template<typename U, U uFirst, U uLast>
				requires std::convertible_to<U, T>
			explicit constexpr restricted_enum(restricted_enum<U, uFirst, uLast> const rhs) noexcept : restricted_enum(tc::implicit_cast<U>(rhs)) {}

			// Assignment allows explicit constructors as well.
			// This is needed for spirit: it needs to assign to a restricted_enum attribute.
			template<typename U>
				requires std::is_constructible<restricted_enum<T, tFirst, tLast>, U const&>::value
			constexpr restricted_enum& operator=(U const& rhs) & noexcept {
				tc::renew(*this, rhs);
				return *this;
			}

			template <typename U>
				requires std::same_as<U, T>
					|| (tc::char_type<T> && tc::char_type<U> && detail::is_only_ascii(tFirst, tLast))
			constexpr operator U() const& noexcept {
				return tc::explicit_cast<U>(m_t);
			}

			template<tc::actual_integer Integral>
				requires requires (T& value, Integral n) { value += n; }
			constexpr restricted_enum& operator+=(Integral const n) noexcept {
				_ASSERTE(tc::cmp_less_equal(c_tFirst - m_t, n) && tc::cmp_less_equal(n, c_tLast - m_t));
				m_t = static_cast<T>(m_t + n);
				return *this;
			}

			template<tc::actual_integer Integral>
				requires requires (T& value, Integral n) { value -= n; }
			constexpr restricted_enum& operator-=(Integral const n) noexcept {
				_ASSERTE(tc::cmp_less_equal(m_t - c_tLast, n) && tc::cmp_less_equal(n, m_t - c_tFirst));
				m_t = static_cast<T>(m_t - n);
				return *this;
			}
		};

		namespace detail
		{
			template <typename T>
			constexpr bool is_restricted_enum = false;
			template <typename T, T tFirst, T tLast>
			constexpr bool is_restricted_enum<restricted_enum<T, tFirst, tLast>> = true;
		}

		template<typename T, T tFirst, T tLast>
		constexpr auto to_underlying_impl(restricted_enum<T, tFirst, tLast> const t) return_decltype_noexcept(
			tc::to_underlying(tc::implicit_cast<T>(t))
		)

		template<typename T, T tFirst, T tLast>
		constexpr auto from_underlying_impl(tc::type::identity<restricted_enum<T, tFirst, tLast>>, tc::underlying_type_t<restricted_enum<T, tFirst, tLast>> const t) return_decltype_noexcept(
			// Range check for T done in its from_underlying implementation.
			// Range check for [tFirst, tLast] done in constructor.
			restricted_enum<T, tFirst, tLast>(tc::from_underlying<T>(t))
		)

#pragma push_macro("DEFINE_COMPARISON_OP")
#define DEFINE_COMPARISON_OP(op, type, trait) \
		template<typename T, T tFirst, T tLast, typename U> \
			requires (!detail::is_restricted_enum<U> && trait<T, U>) \
		constexpr type operator op(restricted_enum<T, tFirst, tLast> const lhs, U const& rhs) noexcept { \
			return tc::implicit_cast<T>(lhs) op rhs; \
		} \
		template<tc::char_type T, T tFirst, T tLast, tc::char_type U> \
			requires (!detail::is_restricted_enum<U> && !trait<T, U> && detail::is_only_ascii(tFirst, tLast)) \
		constexpr type operator op(restricted_enum<T, tFirst, tLast> const lhs, U const& rhs) noexcept { \
			return static_cast<U>(tc::implicit_cast<T>(lhs)) op rhs; \
		} \
		template<typename T, T tFirst, T tLast, typename U, U uFirst, U uLast> \
			requires trait<T, U> \
		constexpr type operator op(restricted_enum<T, tFirst, tLast> const lhs, restricted_enum<U, uFirst, uLast> const rhs) noexcept { \
			return tc::implicit_cast<T>(lhs) op tc::implicit_cast<U>(rhs); \
		} \
		template<tc::char_type T, T tFirst, T tLast, tc::char_type U, U uFirst, U uLast> \
			requires (!trait<T, U> && detail::is_only_ascii(tFirst, tLast) && detail::is_only_ascii(uFirst, uLast)) \
		constexpr type operator op(restricted_enum<T, tFirst, tLast> const lhs, restricted_enum<U, uFirst, uLast> const rhs) noexcept { \
			return char32_t(tc::implicit_cast<T>(lhs)) op char32_t(tc::implicit_cast<U>(rhs)); \
		}

		DEFINE_COMPARISON_OP(==, bool, tc::safely_equality_comparable_with)
		DEFINE_COMPARISON_OP(<=>, std::weak_ordering, tc::safely_totally_ordered_with)
#pragma pop_macro("DEFINE_COMPARISON_OP")

		template <typename T, T tFirst, T tLast, typename U>
			requires requires (T const& lhs, U const& rhs) { lhs - rhs; }
		constexpr auto operator-(restricted_enum<T, tFirst, tLast> const lhs, U const& rhs) noexcept {
			return tc::implicit_cast<T>(lhs) - rhs;
		}
		template <typename T, typename U, U uFirst, U uLast>
			requires requires (T const& lhs, U const& rhs) { lhs - rhs; }
		constexpr auto operator-(T const& lhs, restricted_enum<U, uFirst, uLast> const rhs) noexcept {
			return lhs - tc::implicit_cast<U>(rhs);
		}
		template <typename T, T tFirst, T tLast, typename U, U uFirst, U uLast>
			requires requires (T const& lhs, U const& rhs) { lhs - rhs; }
		constexpr auto operator-(restricted_enum<T, tFirst, tLast> const lhs, restricted_enum<U, uFirst, uLast> const rhs) noexcept {
			return tc::implicit_cast<T>(lhs) - tc::implicit_cast<U>(rhs);
		}
	}
	using restricted_enum_adl::restricted_enum;

	namespace char_like_detail {
		template<typename T, T tFirst, T tLast>
		inline constexpr bool char_like_impl<tc::restricted_enum<T, tFirst, tLast>> = tc::char_like<T>;
	}
	namespace no_adl {
		template<tc::char_type Char, Char chFirst, Char chLast>
			requires (restricted_enum_adl::detail::is_only_ascii(chFirst, chLast))
		struct char_limits<tc::restricted_enum<Char, chFirst, chLast>> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 1;

			[[nodiscard]] static constexpr bool interval_in_range(unsigned int nFirst, unsigned int nLast) noexcept {
				return static_cast<unsigned>(chFirst) <= nFirst && nLast <= static_cast<unsigned>(chLast);
			}
		};
	}

}
