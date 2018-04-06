//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "explicit_cast.h"
#include "return_decltype.h"
#include "inplace.h"
#include "type_traits.h"
#include <type_traits>
#include <chrono>

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
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
	template<typename T>
	struct is_actual_integer_like final : tc::is_actual_integer<T> {};
	template<unsigned MinBits,unsigned MaxBits,boost::multiprecision::cpp_integer_type SignType,boost::multiprecision::cpp_int_check_type Checked,typename Allocator>
	struct is_actual_integer_like<boost::multiprecision::number<
		boost::multiprecision::cpp_int_backend<
			MinBits,
			MaxBits,
			SignType,
			Checked,
			Allocator
		>
	>> final : std::true_type {};
	template<typename T>
	struct is_floating_point_like final : std::is_floating_point<T> {};

	// checks whether arithmetic operations Lhs op Rhs preserve values of Lhs and Rhs (vs. silently converting one to unsigned)
	template< typename Lhs, typename Rhs >
	struct arithmetic_operation_preserves_sign : std::integral_constant<bool,
		// std::is_signed is false for non-arithmetic (e.g., user-defined) types. In this case, we check that the result of addition is signed. This presumably ensures that there is no silent conversion to unsigned.
		!std::is_signed<Lhs>::value && !std::is_signed<Rhs>::value || std::is_signed<decltype(std::declval<Lhs>()+std::declval<Rhs>())>::value
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

	template<typename TTarget, typename TSource, std::enable_if_t<tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TSource&& reluctant_numeric_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<!tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TTarget reluctant_numeric_cast(TSource&& src) noexcept {
		return tc::explicit_cast<TTarget>(std::forward<TSource>(src));
	}

	template<typename Lhs, typename Rhs>
	void assign_numeric_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::explicit_cast<Lhs>(std::forward<Rhs>(rhs));
	}

	//////////////////////////////
	// round

	template< typename T >
	T round( T t ) noexcept {
		static_assert( std::is_floating_point<T>::value );
		return std::floor(t+static_cast<T>(.5));
	}

	//////////////////////////////
	// type-safe multiplication and division

	// TInt<nBits> must be able to represent -2^(nBits-1) and 2^(nBits-1)-1
	template< int nBits, bool bBuiltIn=nBits<=std::numeric_limits<std::uintmax_t>::digits >
	struct TInt;

	template< int nBits >
	struct TInt<nBits,true> final {
		using signed_=typename boost::int_t<nBits>::least;
		using unsigned_=typename boost::uint_t<nBits>::least;
	};

	template< int nBits >
	struct TInt<nBits,false> final {
		using signed_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits/*not -1 due to signed magnitude representation*/, nBits/*not -1 due to signed magnitude representation*/, boost::multiprecision::signed_magnitude, boost::multiprecision::unchecked, void> >;
		using unsigned_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits, nBits, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void> >;
	};

	template< typename T1, typename T2, typename Enable=void >
	struct TMultiply final {};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, std::enable_if_t< 
		tc::is_actual_integer_like< T1 >::value && 
		tc::is_actual_integer_like< T2 >::value &&
		( std::numeric_limits<T1>::is_signed || std::numeric_limits<T2>::is_signed )
	>> final {
		using type=typename TInt< std::numeric_limits<T1>::digits+std::numeric_limits<T2>::digits 
			// For 2s complement representation,
			// in case of both operands signed, -(2^31) * -(2^31) = -2^62, which needs 63 bits to be represented.
			// digits is length-1 for signed numbers, so 31+31+1=63, OK!
			// In case of one unsigned and one signed, (2^32-1) * -2^31 = -2^63 + 2^31, which needs 63 bits to be represented.
			// digits is length for unsigned and length-1 for signed numbers, so 32+31=63, OK!
			+( std::numeric_limits<T1>::is_signed && std::numeric_limits<T2>::is_signed ) >::signed_;
	};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, std::enable_if_t< 
		tc::is_actual_integer_like< T1 >::value && 
		tc::is_actual_integer_like< T2 >::value &&
		!std::numeric_limits<T1>::is_signed && !std::numeric_limits<T2>::is_signed
	>> final {
		using type=typename TInt< std::numeric_limits<T1>::digits+std::numeric_limits<T2>::digits >::unsigned_;
	};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, std::enable_if_t< std::is_floating_point< T1 >::value || std::is_floating_point< T2 >::value >> final {
		using type = decltype(std::declval<T1>()*std::declval<T2>());
	};

	template< typename Lhs, typename Rhs >
	typename TMultiply<Lhs,Rhs>::type
	mul( Lhs lhs, Rhs rhs ) noexcept {
		return tc::explicit_cast<typename TMultiply<Lhs,Rhs>::type>(lhs)*tc::explicit_cast<typename TMultiply<Lhs,Rhs>::type>(rhs);
	}

	template<typename T>
	auto sqr(T const& x) noexcept
		return_decltype( mul(x,x) )

	template< typename Num, typename Den >
	constexpr double fdiv( Num num, Den den) noexcept {
		return static_cast<double>(num)/static_cast<double>(den);
	}

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

	struct SRoundFloor final {
		template< typename T, std::enable_if_t<std::is_integral<T>::value>* = nullptr>
		T operator()( T t ) const& noexcept {
			return t;
		}
		template< typename T, std::enable_if_t<std::is_floating_point<T>::value>* = nullptr>
		T operator()( T t ) const& noexcept {
			return std::floor(t);
		}
		template< typename T >
		static T PositiveOffset( T t ) noexcept {
			return 0;
		}
		template< typename T >
		static T NegativeOffset( T t ) noexcept {
			return t-1;
		}
	} const roundFLOOR{};

	struct SRoundNearest final {
		template< typename T, std::enable_if_t<std::is_integral<T>::value>* = nullptr>
		T operator()( T t ) const& noexcept {
			return t;
		}
		template< typename T, std::enable_if_t<std::is_floating_point<T>::value>* = nullptr>
		T operator()( T t ) const& noexcept {
			return tc::round(t);
		}
		template< typename T >
		static T PositiveOffset( T t ) noexcept {
			return t/2;
		}
		template< typename T >
		static T NegativeOffset( T t ) noexcept {
			return (t-1)/2;
		}
	} const roundNEAREST{};

	struct SRoundCeil final {
		template< typename T, std::enable_if_t<std::is_integral<T>::value>* = nullptr>
		T operator()( T t ) const& noexcept {
			return t;
		}
		template< typename T, std::enable_if_t<std::is_floating_point<T>::value>* = nullptr>
		T operator()( T t ) const& noexcept {
			return std::ceil(t);
		}
		template< typename T >
		static T PositiveOffset( T t ) noexcept {
			return t-1;
		}
		template< typename T >
		static T NegativeOffset( T t ) noexcept {
			return 0;
		}
	} const roundCEIL{};

	struct SRoundBanker final {
	} const roundBANKER{};

	namespace idiv_impl {
		// The standard guarantees that integer division rounds to zero.
		// [expr.mul]/4 (order 5.6/4) 
		template< typename Num, typename Denom, typename Round >
		Num idiv( Num num, Denom denom, Round round ) noexcept {
			static_assert( tc::is_actual_integer_like<Num>::value );
			static_assert( tc::is_actual_integer_like< Denom >::value );
			static_assert( std::numeric_limits<Num>::is_signed==std::numeric_limits<Denom>::is_signed );
			_ASSERT( 0<denom );
			return tc::explicit_cast<Num>( ( num<0 ? num-Round::NegativeOffset(denom) : num+Round::PositiveOffset(denom) )/denom );
		}

		template< typename Num, typename Denom >
		Num idiv( Num num, Denom denom, SRoundBanker ) noexcept {
			static_assert( tc::is_actual_integer_like<Num>::value );
			static_assert( tc::is_actual_integer_like< Denom >::value );
			static_assert( std::numeric_limits<Num>::is_signed==std::numeric_limits<Denom>::is_signed );
			_ASSERT( 0<denom );
			auto result = num / denom;
			auto remainder = num - result * denom;
			if(remainder<0) -tc::inplace(remainder);
			switch_no_default(tc::compare(denom,remainder * 2)) {
				case tc::order::equal:
					if(result % 2 == 0) break;
				case tc::order::less:
					result += num < 0 ? -1 : 1;
					break;
				case tc::order::greater:
					break;
			}
			return tc::explicit_cast<Num>(result);
		}
	}

	template< typename Num, typename Denom, typename Round, std::enable_if_t<
		tc::is_actual_integer_like<Num>::value && tc::is_actual_integer_like< Denom >::value && std::numeric_limits<Num>::is_signed && std::numeric_limits<Denom>::is_signed
	>* = nullptr>
		Num idiv(Num num, Denom denom, Round round) noexcept {
		if (denom < 0) {
			-tc::inplace(num);
			-tc::inplace(denom);
		}
		return idiv_impl::idiv(num, denom, round);
	}

	template< typename Num, typename Denom, typename Round, std::enable_if_t<
		tc::is_actual_integer_like<Num>::value && tc::is_actual_integer_like< Denom >::value && !std::numeric_limits<Num>::is_signed && !std::numeric_limits<Denom>::is_signed
	>* = nullptr>
	Num idiv( Num num, Denom denom, Round round ) noexcept {
		return idiv_impl::idiv( num, denom, round );
	}

	template< typename Num, typename Denom, typename Round, std::enable_if_t<
		tc::is_actual_integer_like<Num>::value && tc::is_actual_integer_like< Denom >::value && std::numeric_limits<Num>::is_signed && !std::numeric_limits<Denom>::is_signed
	>* = nullptr>
	Num idiv( Num num, Denom denom, Round round ) noexcept {
		return idiv/*not idiv_impl::idiv*/( num, tc::signed_cast(denom), round );
	}

	template< typename Num, typename Denom, typename Round, std::enable_if_t<
		tc::is_actual_integer_like<Num>::value && tc::is_actual_integer_like< Denom >::value && !std::numeric_limits<Num>::is_signed && std::numeric_limits<Denom>::is_signed
	>* = nullptr>
	Num idiv( Num num, Denom denom, Round round ) noexcept {
		return idiv_impl::idiv( num, tc::unsigned_cast(denom), round );
	}

	template< typename Num, typename Denom, typename Round, std::enable_if_t<
		tc::is_actual_integer_like<Num>::value && std::is_floating_point< Denom >::value
	>* = nullptr>
	Num idiv( Num num, Denom denom, Round round ) noexcept {
		return tc::explicit_cast<Num>(round(num/denom));
	}

	template< typename Rep, typename Period, typename Den, typename Round >
	auto idiv(std::chrono::duration<Rep, Period> const& dur, Den den, Round round) noexcept return_decltype(
		std::chrono::duration<Rep, Period>(idiv(dur.count(), tc_move(den), round))
	)

	/////////////////////////////////
	// scale_div

	template< typename Num, typename Denom, typename Round, std::enable_if_t<tc::is_actual_integer< Num >::value>* = nullptr >
	Num scale_div( Num num, Denom denom, Round round ) noexcept {
		return idiv(num,denom,round);
	}

	template< typename Num, typename Denom, std::enable_if_t<std::is_floating_point< Num >::value>* = nullptr >
	Num scale_div( Num num, Denom denom, SRoundNearest ) noexcept {
		return tc::explicit_cast<Num>(num/denom);
	}

	template< typename Num, typename Denom >
	Num scale_div( Num num, Denom denom ) noexcept {
		return scale_div(num,denom,tc::roundNEAREST);
	}

	template< bool bGeneralized, typename T, std::enable_if_t<tc::is_actual_integer< T >::value>* = nullptr >
	T internal_lower_half(T t) noexcept {
		return scale_div(t, 2, tc::roundFLOOR);
	}

	template< bool bGeneralized, typename T, std::enable_if_t<std::is_floating_point< T >::value>* = nullptr >
	T internal_lower_half(T t) noexcept {
		return t / 2;
	}

	template< bool bGeneralized, typename Rep, typename Period, std::enable_if_t<bGeneralized>* = nullptr >
	std::chrono::duration<Rep, Period> internal_lower_half(std::chrono::duration<Rep, Period> const& dur) noexcept {
		return idiv(dur, 2, tc::roundFLOOR);
	}

	template< typename T >
	auto lower_half(T&& t) noexcept return_decltype( internal_lower_half</*bGeneralized*/ false>(std::forward<T>(t)) )

	template< bool bGeneralized, typename T >
	auto internal_center(T const& begin, T const& end) noexcept {
		return begin + internal_lower_half<bGeneralized>(end - begin);
	}

	template< typename T >
	auto center(T const& begin, T const& end) noexcept return_decltype( internal_center</*bGeneralized*/false>(begin, end) )

	/////////////////////////////////
	// scale_mul

	template<typename T, typename Factor, std::enable_if_t<
		tc::is_actual_integer< T >::value && std::is_integral< Factor >::value
	>* = nullptr>
	T scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		return tc::explicit_cast<T>(tc::prepare_argument<T,Factor>::prepare(t)*tc::prepare_argument<T,Factor>::prepare(factor));
	}

	template<typename T, typename Factor, typename TRound, std::enable_if_t<
		tc::is_actual_integer< T >::value && std::is_floating_point< Factor >::value
	>* = nullptr>
	T scale_mul(T t, Factor factor, TRound round) noexcept {
		return tc::explicit_cast<T>(round(t*factor));
	}

	template<typename T, typename Factor, std::enable_if_t<
		std::is_floating_point< T >::value
	>* = nullptr>
	T scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		return tc::explicit_cast<T>(t*factor);
	}

	template<typename T, typename Factor>
	T scale_mul(T t, Factor factor) noexcept {
		return scale_mul(t,factor,tc::roundNEAREST);
	}

	/////////////////////////////////
	// scale_muldiv

	template<typename T, typename Num, typename Den, typename TRound, std::enable_if_t<
		tc::is_actual_integer< T >::value && tc::is_actual_integer< Num >::value && tc::is_actual_integer< Den >::value
	>* = nullptr>
	T scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		return tc::reluctant_numeric_cast<T>(idiv(mul(t, num), den, round));
	}

	template<typename T, typename Num, typename Den, typename TRound, std::enable_if_t<
		tc::is_actual_integer< T >::value && tc::is_actual_integer< Num >::value && tc::is_floating_point_like< Den >::value
		>* = nullptr>
	T scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		return tc::reluctant_numeric_cast<T>(round(mul(t, num) / den));
	}

	template<typename T, typename Num, typename Den, typename TRound, std::enable_if_t<
		tc::is_actual_integer< T >::value && tc::is_floating_point_like< Num >::value
		>* = nullptr>
	T scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		return tc::reluctant_numeric_cast<T>(round(t * tc::fdiv(num, den)));
	}

	template<typename T, typename Num, typename Den, std::enable_if_t<
		tc::is_floating_point_like< T >::value
	>* = nullptr>
	T scale_muldiv(T t, Num num, Den den, SRoundNearest) noexcept {
		return tc::reluctant_numeric_cast<T>(t * tc::fdiv(num, den));
	}

	template<typename T, typename Num, typename Den>
	T scale_muldiv(T const& x, Num const& num, Den const& den) noexcept {
		return scale_muldiv(x, num, den, tc::roundNEAREST);
	}

	struct fraction {
		int m_nNum;
		int m_nDen;
	};

	template<typename T>
	T scale_muldiv(T const& x, tc::fraction const& fracn) noexcept {
		return scale_muldiv(x, fracn.m_nNum, fracn.m_nDen);
	}

	/////////////////////////////////
	// rounding_cast
	template<typename Dst, typename Src, typename TRound,
		std::enable_if_t<
			tc::is_actual_integer<tc::decay_t<Src>>::value && (tc::is_actual_integer<Dst>::value || tc::is_floating_point_like<Dst>::value)
			|| (tc::is_floating_point_like<tc::decay_t<Src>>::value && tc::is_floating_point_like<Dst>::value)
		>* = nullptr
	>
	decltype(auto) rounding_cast(Src&& x, TRound) noexcept {
		return tc::reluctant_numeric_cast<Dst>(std::forward<Src>(x));
	}

	template<typename Dst, typename Src, typename TRound,
		std::enable_if_t<tc::is_floating_point_like<tc::decay_t<Src>>::value && tc::is_actual_integer<Dst>::value>* = nullptr
	>
	decltype(auto) rounding_cast(Src&& x, TRound round) noexcept {
		return tc::reluctant_numeric_cast<Dst>(round(std::forward<Src>(x)));
	}

	template<typename Dst, typename Src>
	decltype(auto) rounding_cast(Src&& x) noexcept {
		return rounding_cast<Dst>(std::forward<Src>(x), roundNEAREST);
	}

	DEFINE_FN_TMPL(rounding_cast,(typename))

	template<typename Lhs, typename Rhs, typename TRound>
	void assign_rounding_cast(Lhs& lhs, Rhs&& rhs, TRound round) noexcept {
		lhs=tc::rounding_cast<Lhs>(std::forward<Rhs>(rhs), round);
	}

	template<typename Lhs, typename Rhs>
	void assign_rounding_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::rounding_cast<Lhs>(std::forward<Rhs>(rhs));
	}
}
