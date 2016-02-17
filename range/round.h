//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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

#include "return_decltype.h"
#include "in_place.h"
#include "type_traits.h"
#include <boost/integer.hpp>
#include <boost/numeric/conversion/is_subranged.hpp>
#include <boost/numeric/conversion/converter.hpp>
#include <type_traits>
#include <chrono>

#ifdef TC_MAC
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#else
#pragma warning(push)
#pragma warning( disable: 4459 ) // declaration hides global declaration
#endif
#include <boost/multiprecision/cpp_int.hpp>
#ifdef TC_MAC
#pragma clang diagnostic pop
#else
#pragma warning(pop)
#endif

namespace std {
	template<unsigned MinBits,unsigned MaxBits,boost::multiprecision::cpp_integer_type SignType,boost::multiprecision::cpp_int_check_type Checked,typename Allocator>
	struct is_integral<boost::multiprecision::number<
		boost::multiprecision::cpp_int_backend<
			MinBits,
			MaxBits,
			SignType,
			Checked,
			Allocator
		>
	>> : std::true_type {};
	template<unsigned MinBits,unsigned MaxBits,boost::multiprecision::cpp_integer_type SignType,boost::multiprecision::cpp_int_check_type Checked,typename Allocator>
	struct is_signed<boost::multiprecision::number<
		boost::multiprecision::cpp_int_backend<
			MinBits,
			MaxBits,
			SignType,
			Checked,
			Allocator
		>
	>> : std::integral_constant<bool,SignType==boost::multiprecision::signed_magnitude> {};
}

namespace tc {
	template<typename T>
	struct is_floating_point_or_similar final : std::integral_constant<
		bool,
		std::is_floating_point<T>::value
	> {};

	// checks whether arithmetic operations Lhs op Rhs preserve values of Lhs and Rhs (vs. silently converting one to unsigned)
	template< typename Lhs, typename Rhs >
	struct arithmetic_operation_preserves_sign : std::integral_constant<bool,
		// std::is_signed is false for non-arithmetic (e.g., user-defined) types. In this case, we check that the result of addition is signed. This presumably ensures that there is no silent conversion to unsigned.
		!std::is_signed<Lhs>::value && !std::is_signed<Rhs>::value || std::is_signed<decltype(std::declval<Lhs>()+std::declval<Rhs>())>::value
	> {
		static_assert(tc::is_actual_integer<Lhs>::value, "");
		static_assert(tc::is_actual_integer<Rhs>::value, "");
	};

	static_assert( !tc::arithmetic_operation_preserves_sign<unsigned int, int>::value, "" );
	static_assert( tc::arithmetic_operation_preserves_sign<unsigned char, int>::value, "" );

	
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

	//////////////////////////////
	// char_cast

	template< typename T>
	struct char_limits;

	template<>
	struct char_limits<char> {
		static bool in_range( unsigned int n ) noexcept {
			return n<=0x7f;
		}
	};

	template<>
	struct char_limits<char16_t> {
		static bool in_range( unsigned int n ) noexcept {
			return n<=0xd7ff || 0xe000<=n && n<=0xffff;
		}
	};

#ifdef WIN32
	static_assert(sizeof(char16_t)==sizeof(wchar_t),"");
	template<>
	struct char_limits<wchar_t> : char_limits<char16_t> {};
#endif

	template<>
	struct char_limits<char32_t> {
		static bool in_range( unsigned int n ) noexcept {
			return n<=0xd7ff || 0xe000<=n && n<=0x10ffff;
		}
	};

	template< typename Dst, typename Src >
	Dst char_cast(Src src) noexcept {
		static_assert( tc::is_decayed< Dst >::value, "" );
		_ASSERT( char_limits<Src>::in_range(tc::unsigned_modulo_cast(src)) );
		_ASSERT( char_limits<Dst>::in_range(tc::unsigned_modulo_cast(src)) );
		return static_cast<Dst>(src);
	}

	//////////////////////////////
	// numeric_cast

	template<typename TTarget, typename TSource>
	void range_check( TSource const& src ) noexcept {
		_ASSERTPRINT( (boost::numeric::cInRange==boost::numeric::converter<TTarget, TSource,
			boost::numeric::conversion_traits<TTarget, TSource>,
			boost::numeric::silent_overflow_handler, // only use type::out_of_range(src) result for our asserts
			boost::numeric::Trunc< TTarget > // corresponds to builtin conversion from float to integer
		>::out_of_range(src)), src );
	}

	template<typename TTarget, unsigned MinBits,unsigned MaxBits,boost::multiprecision::cpp_integer_type SignType,boost::multiprecision::cpp_int_check_type Checked,typename Allocator>
	void range_check(boost::multiprecision::number<
		boost::multiprecision::cpp_int_backend<
			MinBits,
			MaxBits,
			SignType,
			Checked,
			Allocator
		>
	> const& ) noexcept {}

	template<typename TTarget, typename TSource>
	std::enable_if_t<
		tc::is_actual_integer<TTarget>::value && std::is_floating_point<TSource>::value,
	TTarget > numeric_cast(TSource src) noexcept {
		range_check<TTarget>(src);
		TTarget target=static_cast<TTarget>(src);
		_ASSERTEQUAL( target,src ); // default round-to-zero from floating point to integer is wrong most of the time, so we force rounding first
		return target;
	}

	template<typename TTarget, typename TSource>
	std::enable_if_t<
		tc::is_actual_integer<TTarget>::value && tc::is_actual_integer<TSource>::value ||
		std::is_floating_point<TTarget>::value && tc::is_actual_integer<TSource>::value ||
		std::is_floating_point<TTarget>::value && std::is_floating_point<TSource>::value,
	TTarget > numeric_cast(TSource src) noexcept {
		range_check<TTarget>(src);
		return static_cast<TTarget>(src);
	}

	template<typename TTarget, typename State>
	TTarget numeric_cast(std::fpos<State> const& src) noexcept {
		return numeric_cast<TTarget>(boost::implicit_cast<std::streamoff>(src));
	}

	template<typename TTarget, typename TSource>
	std::enable_if_t<
		tc::is_base_of_decayed<TTarget, TSource>::value,
		TSource&&
	> reluctant_numeric_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource>
	std::enable_if_t<
		!tc::is_base_of_decayed<TTarget, TSource>::value,
		TTarget
	> reluctant_numeric_cast(TSource&& src) noexcept {
		return numeric_cast<TTarget>(std::forward<TSource>(src));
	}

	//////////////////////////////
	// round

	template< typename T >
	T round( T t ) noexcept {
		static_assert( std::is_floating_point<T>::value, "" );
		return std::floor(t+static_cast<T>(.5));
	}

	//////////////////////////////
	// type-safe multiplication and division

	template< int nBits, bool bBuiltIn=nBits<=sizeof(std::uintmax_t)*CHAR_BIT >
	struct TInt;

	template< int nBits >
	struct TInt<nBits,true> final {
		using signed_=typename boost::int_t<nBits>::least;
		using unsigned_=typename boost::uint_t<nBits>::least;
	};

	template< int nBits >
	struct TInt<nBits,false> final {
		using signed_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits, nBits, boost::multiprecision::signed_magnitude, boost::multiprecision::unchecked, void> >;
		using unsigned_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits, nBits, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void> >;
	};

	template< typename T1, typename T2, typename Enable=void >
	struct TMultiply final {};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, std::enable_if_t< 
		tc::is_actual_integer< T1 >::value && 
		tc::is_actual_integer< T2 >::value &&
		( std::is_signed<T1>::value || std::is_signed<T2>::value )
	>> final {
		using type=typename TInt< ( sizeof(T1)+sizeof(T2) )*CHAR_BIT >::signed_;
	};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, std::enable_if_t< 
		tc::is_actual_integer< T1 >::value && 
		tc::is_actual_integer< T2 >::value &&
		std::is_unsigned<T1>::value && std::is_unsigned<T2>::value
	>> final {
		using type=typename TInt< ( sizeof(T1)+sizeof(T2) )*CHAR_BIT >::unsigned_;
	};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, std::enable_if_t< std::is_floating_point< T1 >::value || std::is_floating_point< T2 >::value >> final {
		using type = decltype(std::declval<T1>()*std::declval<T2>());
	};

	template< typename Lhs, typename Rhs >
	typename TMultiply<Lhs,Rhs>::type
	mul( Lhs lhs, Rhs rhs ) noexcept {
		return tc::numeric_cast<typename TMultiply<Lhs,Rhs>::type>(lhs)*tc::numeric_cast<typename TMultiply<Lhs,Rhs>::type>(rhs);
	}

	template<typename T>
	auto sqr(T const& x) noexcept
		return_decltype( mul(x,x) )

	template< typename Num, typename Den >
	constexpr double fdiv( Num num, Den den) noexcept {
		return static_cast<double>(num)/static_cast<double>(den);
	}

	template< typename Lhs, typename Rhs>
	std::enable_if_t<
		( tc::is_actual_integer<Lhs>::value || tc::is_char<Lhs>::value ) &&
		tc::is_actual_integer<Rhs>::value &&
		!std::is_signed<Lhs>::value
	, Lhs > add( Lhs lhs, Rhs rhs ) noexcept {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs+rhs);
	}

	template< typename Lhs, typename Rhs>
	std::enable_if_t<
		( tc::is_actual_integer<Lhs>::value || tc::is_char<Lhs>::value ) &&
		tc::is_actual_integer<Rhs>::value &&
		!std::is_signed<Lhs>::value
	, Lhs > sub( Lhs lhs, Rhs rhs ) noexcept {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs-rhs);
	}

	template< typename Lhs, typename Rhs>
	std::enable_if_t<
		( tc::is_actual_integer<Lhs>::value || tc::is_char<Lhs>::value ) &&
		tc::is_actual_integer<Rhs>::value &&
		std::is_signed<Lhs>::value
	, Lhs > add( Lhs lhs, Rhs rhs ) noexcept {
		// does not rely on implementation-defined truncation of integers
		auto result=lhs+tc::numeric_cast< std::make_signed_t<decltype(lhs+rhs)>>(rhs);
		range_check<Lhs>(result);
		return static_cast<Lhs>(result);
	}

	template< typename Lhs, typename Rhs>
	std::enable_if_t<
		( tc::is_actual_integer<Lhs>::value || tc::is_char<Lhs>::value ) &&
		tc::is_actual_integer<Rhs>::value &&
		std::is_signed<Lhs>::value
	, Lhs > sub( Lhs lhs, Rhs rhs ) noexcept {
		// does not rely on implementation-defined truncation of integers
		auto result=lhs-tc::numeric_cast< std::make_signed_t<decltype(lhs-rhs)>>(rhs);
		range_check<Lhs>(result);
		return static_cast<Lhs>(result);
	}
}

struct SRoundFloor final {
	template< typename T >
	std::enable_if_t< std::is_integral<T>::value, T > operator()( T t ) const noexcept {
		return t;
	}
	template< typename T >
	std::enable_if_t< std::is_floating_point<T>::value, T > operator()( T t ) const noexcept {
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
	template< typename T >
	std::enable_if_t< std::is_integral<T>::value, T > operator()( T t ) const noexcept {
		return t;
	}
	template< typename T >
	std::enable_if_t< std::is_floating_point<T>::value, T > operator()( T t ) const noexcept {
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
	template< typename T >
	std::enable_if_t< std::is_integral<T>::value, T > operator()( T t ) const noexcept {
		return t;
	}
	template< typename T >
	std::enable_if_t< std::is_floating_point<T>::value, T > operator()( T t ) const noexcept {
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

namespace tc {
	namespace idiv_impl {
		// The standard guarantees that integer division rounds to zero.
		// [expr.mul]/4 (oder 5.6/4) 
		template< typename Num, typename Denom, typename Round >
		Num idiv( Num num, Denom denom, Round round ) noexcept {
			static_assert( tc::is_actual_integer<Num>::value, "" );
			static_assert( tc::is_actual_integer< Denom >::value, "" );
			static_assert( std::is_signed<Num>::value==std::is_signed<Denom>::value, "" );
			_ASSERT( 0<denom );
			return tc::numeric_cast<Num>( ( num<0 ? num-Round::NegativeOffset(denom) : num+Round::PositiveOffset(denom) )/denom );
		}

		template< typename Num, typename Denom >
		Num idiv( Num num, Denom denom, SRoundBanker ) noexcept {
			static_assert( tc::is_actual_integer<Num>::value, "" );
			static_assert( tc::is_actual_integer< Denom >::value, "" );
			static_assert( std::is_signed<Num>::value==std::is_signed<Denom>::value, "" );
			_ASSERT( 0<denom );
			auto halfdenom=std::div(denom,2);
			if( 0==halfdenom.rem ) {
				auto result2=std::div(num,halfdenom.quot);
				if( 0==result2.rem ) {
					switch_no_default( result2.quot%4 ) {
					case -2:
					case 0:
					case 2:
						break;
					case 1:
					case -3:
						--num;
						break;
					case 3:
					case -1:
						++num;
						break;
					}
				}
			}
			return tc::numeric_cast<Num>( ( num<0 ? num-(denom-1)/2 : num+halfdenom.quot )/denom );
		}
	}

	template< typename Num, typename Denom, typename Round >
	std::enable_if_t<
		tc::is_actual_integer< Denom >::value && std::is_signed<Num>::value==std::is_signed<Denom>::value
	,Num> idiv( Num num, Denom denom, Round round ) noexcept {
		static_assert( tc::is_actual_integer<Num>::value, "" );
		_ASSERT( 0!=denom );
		if( denom<0 ) { // optimized away for unsigned denom
			-tc::in_place(num);
			-tc::in_place(denom);
		}
		return idiv_impl::idiv( num, denom, round );
	}

	template< typename Num, typename Denom, typename Round >
	std::enable_if_t<
		tc::is_actual_integer< Denom >::value && std::is_signed<Num>::value && !std::is_signed<Denom>::value
	,Num> idiv( Num num, Denom denom, Round round ) noexcept {
		static_assert( tc::is_actual_integer<Num>::value, "" );
		return idiv_impl::idiv( num, tc::signed_cast(denom), round );
	}

	template< typename Num, typename Denom, typename Round >
	std::enable_if_t<
		tc::is_actual_integer< Denom >::value && !std::is_signed<Num>::value && std::is_signed<Denom>::value
	,Num> idiv( Num num, Denom denom, Round round ) noexcept {
		static_assert( tc::is_actual_integer<Num>::value, "" );
		return idiv_impl::idiv( num, tc::unsigned_cast(denom), round );
	}

	template< typename Num, typename Denom, typename Round >
	std::enable_if_t<
		std::is_floating_point< Denom >::value
	,Num>	idiv( Num num, Denom denom, Round round ) noexcept {
		static_assert( tc::is_actual_integer<Num>::value, "" );
		return tc::numeric_cast<Num>(round(num/denom));
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
		return tc::numeric_cast<Num>(num/denom);
	}

	template< typename Num, typename Denom >
	Num scale_div( Num num, Denom denom ) noexcept {
		return scale_div(num,denom,roundNEAREST);
	}

	template< bool bGeneralized, typename T, std::enable_if_t<tc::is_actual_integer< T >::value>* = nullptr >
	T internal_lower_half(T t) noexcept {
		return scale_div(t, 2, roundFLOOR);
	}

	template< bool bGeneralized, typename T, std::enable_if_t<std::is_floating_point< T >::value>* = nullptr >
	T internal_lower_half(T t) noexcept {
		return t / 2;
	}

	template< bool bGeneralized, typename Rep, typename Period, std::enable_if_t<bGeneralized>* = nullptr >
	std::chrono::duration<Rep, Period> internal_lower_half(std::chrono::duration<Rep, Period> const& dur) noexcept {
		return idiv(dur, 2, roundFLOOR);
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

	template<typename T, typename Factor>
	std::enable_if_t<
		tc::is_actual_integer< T >::value && std::is_integral< Factor >::value
	,T> scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		return tc::numeric_cast<T>(tc::prepare_argument<T,Factor>::prepare(t)*tc::prepare_argument<T,Factor>::prepare(factor));
	}

	template<typename T, typename Factor, typename TRound>
	std::enable_if_t<
		tc::is_actual_integer< T >::value && std::is_floating_point< Factor >::value
	,T> scale_mul(T t, Factor factor, TRound round) noexcept {
		return tc::numeric_cast<T>(round(t*factor));
	}

	template<typename T, typename Factor>
	std::enable_if_t<
		std::is_floating_point< T >::value
	,T> scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		return tc::numeric_cast<T>(t*factor);
	}

	template<typename T, typename Factor>
	T scale_mul(T t, Factor factor) noexcept {
		return scale_mul(t,factor,roundNEAREST);
	}

	/////////////////////////////////
	// scale_muldiv

	template<typename T, typename Num, typename Den, typename TRound>
	std::enable_if_t<
		tc::is_actual_integer< T >::value && tc::is_actual_integer< Num >::value && tc::is_actual_integer< Den >::value
	,T> scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		return tc::reluctant_numeric_cast<T>(idiv(mul(t, num), den, round));
	}

	template<typename T, typename Num, typename Den, typename TRound>
	std::enable_if_t<
		tc::is_actual_integer< T >::value && tc::is_actual_integer< Num >::value && tc::is_floating_point_or_similar< Den >::value
		, T> scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		return tc::reluctant_numeric_cast<T>(round(mul(t, num) / den));
	}

	template<typename T, typename Num, typename Den, typename TRound>
	std::enable_if_t<
		tc::is_actual_integer< T >::value && tc::is_floating_point_or_similar< Num >::value
		, T> scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		return tc::reluctant_numeric_cast<T>(round(t * tc::fdiv(num, den)));
	}

	template<typename T, typename Num, typename Den>
	std::enable_if_t<
		tc::is_floating_point_or_similar< T >::value
	,T> scale_muldiv(T t, Num num, Den den, SRoundNearest) noexcept {
		return tc::reluctant_numeric_cast<T>(t * tc::fdiv(num, den));
	}

	template<typename T, typename Num, typename Den>
	T scale_muldiv(T const& x, Num const& num, Den const& den) noexcept {
		return scale_muldiv(x, num, den, roundNEAREST);
	}
}

