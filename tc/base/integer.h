
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits_fwd.h"
#include "explicit_cast.h"

namespace tc {
	// integer<nBits> must be able to represent -2^(nBits-1) and 2^(nBits-1)-1
	template< int nBits, bool bBuiltIn=nBits<=std::numeric_limits<std::uintmax_t>::digits >
	struct integer;

	template< int nBits >
	struct integer<nBits,true> final {
		using signed_=typename boost::int_t<nBits>::least;
		using unsigned_=typename boost::uint_t<nBits>::least;
	};

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
