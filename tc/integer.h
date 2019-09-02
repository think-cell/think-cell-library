
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits.h"
#include "explicit_cast.h"

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wcomma"
#else
#pragma warning(push)
#pragma warning( disable: 4459 ) // declaration hides global declaration
#endif
#include <boost/multiprecision/cpp_int.hpp>
#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma warning(pop)
#endif

namespace tc {
	// integer<nBits> must be able to represent -2^(nBits-1) and 2^(nBits-1)-1
	template< int nBits, bool bBuiltIn=nBits<=std::numeric_limits<std::uintmax_t>::digits >
	struct integer;

	template< int nBits >
	struct integer<nBits,true> final {
		using signed_=typename boost::int_t<nBits>::least;
		using unsigned_=typename boost::uint_t<nBits>::least;
	};

	template< int nBits >
	struct integer<nBits,false> final {
		using signed_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits/*not -1 due to signed magnitude representation*/, nBits/*not -1 due to signed magnitude representation*/, boost::multiprecision::signed_magnitude, boost::multiprecision::unchecked, void> >;
		using unsigned_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits, nBits, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void> >;
	};

	template<typename T>
	struct is_actual_integer_like final : tc::is_actual_integer<T> {};
	template<unsigned MinBits,unsigned MaxBits,boost::multiprecision::cpp_integer_type SignType,boost::multiprecision::cpp_int_check_type Checked,typename Alloc>
	struct is_actual_integer_like<boost::multiprecision::number<
		boost::multiprecision::cpp_int_backend<
			MinBits,
			MaxBits,
			SignType,
			Checked,
			Alloc
		>
	>> final : std::true_type {};
	template<typename T>
	struct is_floating_point_like final : std::is_floating_point<T> {};

	// checks whether arithmetic operations Lhs op Rhs preserve values of Lhs and Rhs (vs. silently converting one to unsigned)
	template< typename Lhs, typename Rhs >
	struct arithmetic_operation_preserves_sign : std::integral_constant<bool,
		// std::is_signed is false for non-arithmetic (e.g., user-defined) types. In this case, we check that the result of addition is signed. This presumably ensures that there is no silent conversion to unsigned.
		(!std::is_signed<Lhs>::value && !std::is_signed<Rhs>::value) || std::is_signed<decltype(std::declval<Lhs>()+std::declval<Rhs>())>::value
	> {
		static_assert(tc::is_actual_integer<Lhs>::value);
		static_assert(tc::is_actual_integer<Rhs>::value);
	};

	static_assert( !tc::arithmetic_operation_preserves_sign<unsigned int, int>::value );
	static_assert( tc::arithmetic_operation_preserves_sign<unsigned char, int>::value );

	template< typename Lhs, typename Rhs, typename Enable=void >
	struct prepare_argument;

	template<typename Lhs, typename Rhs >
	struct prepare_argument <Lhs,Rhs,std::enable_if_t< tc::arithmetic_operation_preserves_sign<Lhs,Rhs>::value > > final {
		template< typename T > static T prepare(T t) noexcept { return t; }
	};

	template<typename Lhs, typename Rhs >
	struct prepare_argument <Lhs,Rhs,std::enable_if_t< !tc::arithmetic_operation_preserves_sign<Lhs,Rhs>::value > > final {
		template< typename T > static std::make_unsigned_t<T> prepare(T t) noexcept { return tc::unsigned_cast(t); }
	};

	template< typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_actual_integer<Lhs>::value &&
		tc::is_actual_integer<Rhs>::value &&
		!std::is_signed<Lhs>::value
	>* = nullptr>
	constexpr Lhs add( Lhs lhs, Rhs rhs ) noexcept {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs+rhs);
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_actual_integer<Lhs>::value &&
		tc::is_actual_integer<Rhs>::value &&
		!std::is_signed<Lhs>::value
	>* = nullptr>
	constexpr Lhs sub( Lhs lhs, Rhs rhs ) noexcept {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs-rhs);
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_actual_integer<Lhs>::value &&
		tc::is_actual_integer<Rhs>::value &&
		std::is_signed<Lhs>::value
	>* = nullptr>
	constexpr Lhs add( Lhs lhs, Rhs rhs ) noexcept {
		// does not rely on implementation-defined truncation of integers
		return tc::explicit_cast<Lhs>( lhs+tc::explicit_cast< std::make_signed_t<decltype(lhs+rhs)>>(rhs) );
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_actual_integer<Lhs>::value &&
		tc::is_actual_integer<Rhs>::value &&
		std::is_signed<Lhs>::value
	>* = nullptr>
	constexpr Lhs sub( Lhs lhs, Rhs rhs ) noexcept {
		// does not rely on implementation-defined truncation of integers
		return tc::explicit_cast<Lhs>( lhs-tc::explicit_cast< std::make_signed_t<decltype(lhs-rhs)>>(rhs) );
	}
}
