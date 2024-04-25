
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits_fwd.h"
#include "explicit_cast.h"

namespace tc {
	namespace no_adl {
		template<int nBits>
		struct int_least;

		template<int nBits> requires (nBits<=std::numeric_limits<std::intmax_t>::digits)
		struct int_least<nBits> {
			using type=typename boost::int_t<nBits+1>::least; // boost::int_t<Bits>::least uses bit-width including sign bit
		};

		template<int nBits>
		struct uint_least;

		template<int nBits> requires (nBits<=std::numeric_limits<std::uintmax_t>::digits)
		struct uint_least<nBits> {
			using type=typename boost::uint_t<nBits>::least;
		};
	}

	// nBits is the number of radix-2 digits the type can represent without change. It does not include sign bit. nBits<=std::numeric_limits<tc::int_least_t<nBits>>::digits.
	template<int nBits>
	using int_least_t = typename no_adl::int_least<nBits>::type;

	template<int nBits>
	using uint_least_t = typename no_adl::uint_least<nBits>::type;
	
	namespace actual_integer_like_detail {
		template<typename T>
		inline constexpr bool actual_integer_like_impl = tc::actual_integer<T>;
	}
	template<typename T>
	concept actual_integer_like = actual_integer_like_detail::actual_integer_like_impl<T>;

	namespace floating_point_like_detail {
		template<typename T>
		inline constexpr bool floating_point_like_impl = std::floating_point<T>;
	}
	template<typename T>
	concept floating_point_like = floating_point_like_detail::floating_point_like_impl<T>;

	// checks whether arithmetic operations Lhs op Rhs preserve values of Lhs and Rhs (vs. silently converting one to unsigned)
	template< typename Lhs, typename Rhs >
	struct arithmetic_operation_preserves_sign : tc::constant<
		// std::is_signed is false for non-arithmetic (e.g., user-defined) types. In this case, we check that the result of addition is signed. This presumably ensures that there is no silent conversion to unsigned.
		(!std::is_signed<Lhs>::value && !std::is_signed<Rhs>::value) || std::is_signed<decltype(std::declval<Lhs>()+std::declval<Rhs>())>::value
	> {
		static_assert(tc::actual_integer<Lhs>);
		static_assert(tc::actual_integer<Rhs>);
	};

	static_assert( !tc::arithmetic_operation_preserves_sign<unsigned int, int>::value );
	static_assert( tc::arithmetic_operation_preserves_sign<unsigned char, int>::value );

	template<typename Lhs, typename Rhs, typename T>
	constexpr auto prepare_argument(T t) noexcept {
		if constexpr( tc::arithmetic_operation_preserves_sign<Lhs,Rhs>::value ) {
			return t;
		} else {
			return tc::as_unsigned(t);
		}
	}

	template< typename Lhs, tc::actual_integer Rhs> requires tc::actual_integer<Lhs> || tc::char_type<Lhs>
	[[nodiscard]] constexpr Lhs add( Lhs lhs, Rhs rhs ) noexcept {
		if constexpr( std::is_signed<Lhs>::value ) {
			// does not rely on implementation-defined truncation of integers
			tc_return_cast( lhs+tc::explicit_cast< std::make_signed_t<decltype(lhs+rhs)>>(rhs) );
		} else {
			// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
			return static_cast<Lhs>(lhs+rhs);
		}
	}

	template< typename Lhs, tc::actual_integer Rhs> requires tc::actual_integer<Lhs> || tc::char_type<Lhs>
	[[nodiscard]] constexpr Lhs sub( Lhs lhs, Rhs rhs ) noexcept {
		if constexpr( std::is_signed<Lhs>::value ) {
			// does not rely on implementation-defined truncation of integers
			tc_return_cast( lhs-tc::explicit_cast< std::make_signed_t<decltype(lhs-rhs)>>(rhs) );
		} else {
			// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
			return static_cast<Lhs>(lhs-rhs);
		}
	}

	template<typename T>
	constexpr T& assign_add(T& lhs, T rhs) noexcept {
		static_assert( tc::actual_integer<T> );
		_ASSERTE( lhs < 0
			? std::numeric_limits<T>::lowest() - lhs <= rhs
			: rhs <= std::numeric_limits<T>::max() - lhs
		);
		return lhs += rhs;
	}

	template<typename T>
	constexpr T& assign_mul(T& lhs, T rhs) noexcept {
		static_assert( tc::actual_integer<T> );
		_ASSERTE(
			lhs <= 0
				? rhs <= 0
					? 0 == lhs || std::numeric_limits<T>::max() / lhs <= rhs
					: 0 == rhs || std::numeric_limits<T>::lowest() / rhs <= lhs
				: rhs <= 0
					? std::numeric_limits<T>::lowest() / lhs <= rhs
					: rhs <= std::numeric_limits<T>::max() / lhs
		);
		return lhs *= rhs;
	}

	template<typename B, typename E>
	[[nodiscard]] constexpr auto pow(B const base, E exp) noexcept -> decltype(base * base) {
		static_assert( tc::actual_integer<B> );
		static_assert( tc::actual_integer<E> );
		using R = decltype(base * base);
		R result = 1;
		if( 0 != exp ) {
			_ASSERTE( 0 < exp );
			R rbase = base;
			for (;;) {
				if( 1 == exp % 2 ) {
					tc::assign_mul(result, rbase);
				}
				exp /= 2;
				if( 0 == exp ) {
					break;
				}
				tc::assign_mul(rbase, rbase);
			}
		} else {
			_ASSERTE( 0 != base );
		}
		return result;
	}
}
