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
		_ASSERT( char_limits<Src>::in_range(tc::unsigned_char_cast(src)) );
		_ASSERT( char_limits<Dst>::in_range(tc::unsigned_char_cast(src)) );
		return static_cast<Dst>(src);
	}

	//////////////////////////////
	// numeric_cast
	// - various cases are similar to the ones in is_safely_convertible_between_arithmetic_values

	template<typename TTarget, typename TSource, std::enable_if_t<
		std::is_floating_point<TTarget>::value && tc::is_actual_arithmetic<TSource>::value>* = nullptr>
	TTarget numeric_cast(TSource src) noexcept {
		return static_cast<TTarget>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<
		tc::is_actual_integer<TTarget>::value && std::is_floating_point<TSource>::value>* = nullptr>
	TTarget numeric_cast(TSource src) noexcept {
		TTarget target=static_cast<TTarget>(src);
		_ASSERTEQUAL( target,src ); // default round-to-zero from floating point to integer is wrong most of the time, so we force rounding first
		return target;
	}

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
#else
#pragma warning(push)
#pragma warning(disable:4018) // signed/unsigned mismatch
#endif
	template<typename TTarget, typename TSource, std::enable_if_t<
		tc::is_actual_integer<TTarget>::value && tc::is_actual_integer<TSource>::value>* = nullptr>
	TTarget numeric_cast(TSource src) noexcept {
		_ASSERTPRINT(
			(
				!std::is_signed<TSource>::value ||
				( std::is_signed<TTarget>::value 
					? std::numeric_limits<TTarget>::lowest() <= src
					: /*must be signed 0 to avoid conversion of src to unsigned*/0 <= src
				)
			) &&
			// conversion to unsigned (Warning 4018) is ok here:
			src <= std::numeric_limits<TTarget>::max()
		, src);
		return static_cast<TTarget>(src);
	}
#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma warning( pop )
#endif

	template<typename T>
	struct is_boost_multiprecision_number : std::false_type {};

	template<typename BackEnd, boost::multiprecision::expression_template_option e>
	struct is_boost_multiprecision_number< boost::multiprecision::number<BackEnd,e> > : std::true_type {};

	template<typename TTarget, typename TSource, std::enable_if_t< is_boost_multiprecision_number< TTarget >::value || is_boost_multiprecision_number< TSource >::value>* =nullptr >
	TTarget numeric_cast( TSource const& src ) {
		return NOEXCEPT( static_cast<TTarget>(src) );
	}

	template<typename TTarget, typename State>
	TTarget numeric_cast(std::fpos<State> const& src) noexcept {
		return numeric_cast<TTarget>(boost::implicit_cast<std::streamoff>(src));
	}

	template<typename TTarget, typename TSource, std::enable_if_t<tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TSource&& reluctant_numeric_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<!tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TTarget reluctant_numeric_cast(TSource&& src) noexcept {
		return numeric_cast<TTarget>(std::forward<TSource>(src));
	}

	template<typename Lhs, typename Rhs>
	void assign_numeric_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::numeric_cast<Lhs>(std::forward<Rhs>(rhs));
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
		return tc::numeric_cast<typename TMultiply<Lhs,Rhs>::type>(lhs)*tc::numeric_cast<typename TMultiply<Lhs,Rhs>::type>(rhs);
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
	Lhs add( Lhs lhs, Rhs rhs ) noexcept {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs+rhs);
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_actual_integer<Lhs>::value &&
		tc::is_actual_integer<Rhs>::value &&
		!std::is_signed<Lhs>::value
	>* = nullptr>
	Lhs sub( Lhs lhs, Rhs rhs ) noexcept {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs-rhs);
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_actual_integer<Lhs>::value &&
		tc::is_actual_integer<Rhs>::value &&
		std::is_signed<Lhs>::value
	>* = nullptr>
	Lhs add( Lhs lhs, Rhs rhs ) noexcept {
		// does not rely on implementation-defined truncation of integers
		return tc::numeric_cast<Lhs>( lhs+tc::numeric_cast< std::make_signed_t<decltype(lhs+rhs)>>(rhs) );
	}

	template< typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_actual_integer<Lhs>::value &&
		tc::is_actual_integer<Rhs>::value &&
		std::is_signed<Lhs>::value
	>* = nullptr>
	Lhs sub( Lhs lhs, Rhs rhs ) noexcept {
		// does not rely on implementation-defined truncation of integers
		return tc::numeric_cast<Lhs>( lhs-tc::numeric_cast< std::make_signed_t<decltype(lhs-rhs)>>(rhs) );
	}
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

namespace tc {
	namespace idiv_impl {
		// The standard guarantees that integer division rounds to zero.
		// [expr.mul]/4 (oder 5.6/4) 
		template< typename Num, typename Denom, typename Round >
		Num idiv( Num num, Denom denom, Round round ) noexcept {
			static_assert( tc::is_actual_integer_like<Num>::value, "" );
			static_assert( tc::is_actual_integer_like< Denom >::value, "" );
			static_assert( std::numeric_limits<Num>::is_signed==std::numeric_limits<Denom>::is_signed, "" );
			_ASSERT( 0<denom );
			return tc::numeric_cast<Num>( ( num<0 ? num-Round::NegativeOffset(denom) : num+Round::PositiveOffset(denom) )/denom );
		}

		template< typename Num, typename Denom >
		Num idiv( Num num, Denom denom, SRoundBanker ) noexcept {
			static_assert( tc::is_actual_integer<Num>::value, "" );
			static_assert( tc::is_actual_integer< Denom >::value, "" );
			static_assert( std::numeric_limits<Num>::is_signed==std::numeric_limits<Denom>::is_signed, "" );
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

	template< typename Num, typename Denom, typename Round, std::enable_if_t<
		tc::is_actual_integer_like<Num>::value && tc::is_actual_integer_like< Denom >::value && std::numeric_limits<Num>::is_signed==std::numeric_limits<Denom>::is_signed
	>* = nullptr>
	Num idiv( Num num, Denom denom, Round round ) noexcept {
		_ASSERT( 0!=denom );
		if( denom<0 ) { // optimized away for unsigned denom
			-tc::inplace(num);
			-tc::inplace(denom);
		}
		return idiv_impl::idiv( num, denom, round );
	}

	template< typename Num, typename Denom, typename Round, std::enable_if_t<
		tc::is_actual_integer_like<Num>::value && tc::is_actual_integer_like< Denom >::value && std::numeric_limits<Num>::is_signed && !std::numeric_limits<Denom>::is_signed
	>* = nullptr>
	Num idiv( Num num, Denom denom, Round round ) noexcept {
		return idiv_impl::idiv( num, tc::signed_cast(denom), round );
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

	template<typename T, typename Factor, std::enable_if_t<
		tc::is_actual_integer< T >::value && std::is_integral< Factor >::value
	>* = nullptr>
	T scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		return tc::numeric_cast<T>(tc::prepare_argument<T,Factor>::prepare(t)*tc::prepare_argument<T,Factor>::prepare(factor));
	}

	template<typename T, typename Factor, typename TRound, std::enable_if_t<
		tc::is_actual_integer< T >::value && std::is_floating_point< Factor >::value
	>* = nullptr>
	T scale_mul(T t, Factor factor, TRound round) noexcept {
		return tc::numeric_cast<T>(round(t*factor));
	}

	template<typename T, typename Factor, std::enable_if_t<
		std::is_floating_point< T >::value
	>* = nullptr>
	T scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		return tc::numeric_cast<T>(t*factor);
	}

	template<typename T, typename Factor>
	T scale_mul(T t, Factor factor) noexcept {
		return scale_mul(t,factor,roundNEAREST);
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
		return scale_muldiv(x, num, den, roundNEAREST);
	}
}

