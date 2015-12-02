#pragma once

#include "return_decltype.h"
#include "in_place.h"
#include "Range/meta.h"
#include <boost/integer.hpp>
#include <boost/numeric/conversion/is_subranged.hpp>
#include <boost/numeric/conversion/converter.hpp>
#include <type_traits>

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
	struct is_floating_point_or_similar : std::integral_constant<
		bool,
		std::is_floating_point<T>::value
	>{};

	// checks whether arithmetic operations Lhs op Rhs preserve values of Lhs and Rhs (vs. silently converting one to unsigned)
	template< typename Lhs, typename Rhs, typename Enable=typename std::enable_if< std::is_arithmetic<Lhs>::value || std::is_arithmetic<Rhs>::value >::type >
	struct arithmetic_operation_preserves_value : std::integral_constant<bool,
		!std::is_signed<Lhs>::value && !std::is_signed<Rhs>::value || std::is_signed<decltype(std::declval<Lhs>()+std::declval<Rhs>())>::value
	> {};

	static_assert( !tc::arithmetic_operation_preserves_value<unsigned int, int>::value, "" );
	static_assert( tc::arithmetic_operation_preserves_value<unsigned char, int>::value, "" );

	
	template< typename Lhs, typename Rhs, typename Enable=void >
	struct prepare_argument;

	template<typename Lhs, typename Rhs >
	struct prepare_argument <Lhs,Rhs,typename std::enable_if< tc::arithmetic_operation_preserves_value<Lhs,Rhs>::value >::type > {
		template< typename T > static T prepare(T t){ return t; }
	};

	template<typename Lhs, typename Rhs >
	struct prepare_argument <Lhs,Rhs,typename std::enable_if< !tc::arithmetic_operation_preserves_value<Lhs,Rhs>::value >::type > {
		template< typename T > static std::make_unsigned_t<T> prepare(T t){ return tc::unsigned_cast(t); }
	};

	//////////////////////////////
	// char_cast

	template< typename T>
	struct char_limits;

	template<>
	struct char_limits<char> {
		static bool in_range( unsigned int n ) {
			return n<=0x7f;
		}
	};

	template<>
	struct char_limits<char16_t> {
		static bool in_range( unsigned int n ) {
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
		static bool in_range( unsigned int n ) {
			return n<=0xd7ff || 0xe000<=n && n<=0x10ffff;
		}
	};

	template< typename Dst, typename Src >
	Dst char_cast(Src src) {
		static_assert( tc::is_decayed< Dst >::value, "" );
		_ASSERT( char_limits<Src>::in_range(tc::unsigned_modulo_cast(src)) );
		_ASSERT( char_limits<Dst>::in_range(tc::unsigned_modulo_cast(src)) );
		return static_cast<Dst>(src);
	}

	//////////////////////////////
	// numeric_cast

	template<typename TTarget, typename TSource>
	void range_check( TSource const& src ) {
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
	> const& ) {};

	template<typename TTarget, typename TSource>
	typename std::enable_if<
		tc::is_non_char_integral<TTarget>::value && std::is_floating_point<TSource>::value,
	TTarget >::type numeric_cast(TSource src) {
		range_check<TTarget>(src);
		TTarget target=static_cast<TTarget>(src);
		_ASSERTEQUAL( target,src ); // default round-to-zero from floating point to integer is wrong most of the time, so we force rounding first
		return target;
	}

	template<typename TTarget, typename TSource>
	typename std::enable_if<
		tc::is_non_char_integral<TTarget>::value && tc::is_non_char_integral<TSource>::value ||
		std::is_floating_point<TTarget>::value && tc::is_non_char_integral<TSource>::value ||
		std::is_floating_point<TTarget>::value && std::is_floating_point<TSource>::value,
	TTarget >::type numeric_cast(TSource src) {
		range_check<TTarget>(src);
		return static_cast<TTarget>(src);
	}

	template<typename TTarget, typename State>
	TTarget numeric_cast(std::fpos<State> const& src) {
		return numeric_cast<TTarget>(boost::implicit_cast<std::streamoff>(src));
	}

	template<typename TTarget, typename TSource>
	typename std::enable_if<
		std::is_same<TTarget, std::decay_t<TSource>>::value,
		TSource&&
	>::type numeric_cast_or_identity(TSource&& src) {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource>
	typename std::enable_if<
		!std::is_same<TTarget, std::decay_t<TSource>>::value,
		TTarget
	>::type numeric_cast_or_identity(TSource&& src) {
		return numeric_cast<TTarget>(std::forward<TSource>(src));
	}

	//////////////////////////////
	// round

	template< typename T >
	T round( T t ) {
		static_assert( std::is_floating_point<T>::value, "" );
		return std::floor(t+static_cast<T>(.5));
	}

	//////////////////////////////
	// type-safe multiplication and division

	template< int nBits, bool bBuiltIn=nBits<=sizeof(std::uintmax_t)*CHAR_BIT >
	struct TInt;

	template< int nBits >
	struct TInt<nBits,true> {
		using signed_=typename boost::int_t<nBits>::least;
		using unsigned_=typename boost::uint_t<nBits>::least;
	};

	template< int nBits >
	struct TInt<nBits,false> {
		using signed_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits, nBits, boost::multiprecision::signed_magnitude, boost::multiprecision::unchecked, void> >;
		using unsigned_=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits, nBits, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void> >;
	};

	template< typename T1, typename T2, typename Enable=void >
	struct TMultiply {};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, typename std::enable_if< 
		std::is_integral< T1 >::value && 
		std::is_integral< T2 >::value &&
		( std::is_signed<T1>::value || std::is_signed<T2>::value )
	>::type> {
		using type=typename TInt< ( sizeof(T1)+sizeof(T2) )*CHAR_BIT >::signed_;
	};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, typename std::enable_if< 
		std::is_integral< T1 >::value && 
		std::is_integral< T2 >::value &&
		std::is_unsigned<T1>::value && std::is_unsigned<T2>::value
	>::type> {
		using type=typename TInt< ( sizeof(T1)+sizeof(T2) )*CHAR_BIT >::unsigned_;
	};

	template< typename T1, typename T2 >
	struct TMultiply<T1, T2, typename std::enable_if< std::is_floating_point< T1 >::value || std::is_floating_point< T2 >::value >::type> {
		using type = decltype(std::declval<T1>()*std::declval<T2>());
	};

	template< typename Lhs, typename Rhs >
	typename TMultiply<Lhs,Rhs>::type
	mul( Lhs lhs, Rhs rhs ) {
		return tc::numeric_cast<typename TMultiply<Lhs,Rhs>::type>(lhs)*tc::numeric_cast<typename TMultiply<Lhs,Rhs>::type>(rhs);
	}

	template<typename T>
	auto sqr(T const& x)
		return_decltype( mul(x,x) )

	template< typename Num, typename Den >
	inline double fdiv( Num num, Den den) {
		return static_cast<double>(num)/static_cast<double>(den);
	}

	template< typename Lhs, typename Rhs>
	typename std::enable_if<
		std::is_integral<Lhs>::value &&
		tc::is_non_char_integral<Rhs>::value &&
		!std::is_signed<Lhs>::value
	, Lhs >::type add( Lhs lhs, Rhs rhs ) {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs+rhs);
	}

	template< typename Lhs, typename Rhs>
	typename std::enable_if<
		std::is_integral<Lhs>::value &&
		tc::is_non_char_integral<Rhs>::value &&
		!std::is_signed<Lhs>::value
	, Lhs >::type sub( Lhs lhs, Rhs rhs ) {
		// modulo semantics, both arguments are zero- or sign-extended to unsigned and the result truncated
		return static_cast<Lhs>(lhs-rhs);
	}

	template< typename Lhs, typename Rhs>
	typename std::enable_if<
		std::is_integral<Lhs>::value &&
		tc::is_non_char_integral<Rhs>::value &&
		std::is_signed<Lhs>::value
	, Lhs >::type add( Lhs lhs, Rhs rhs ) {
		// does not rely on implementation-defined truncation of integers
		auto result=lhs+tc::numeric_cast< std::make_signed_t<decltype(lhs+rhs)>>(rhs);
		range_check<Lhs>(result);
		return static_cast<Lhs>(result);
	}

	template< typename Lhs, typename Rhs>
	typename std::enable_if<
		std::is_integral<Lhs>::value &&
		tc::is_non_char_integral<Rhs>::value &&
		std::is_signed<Lhs>::value
	, Lhs >::type sub( Lhs lhs, Rhs rhs ) {
		// does not rely on implementation-defined truncation of integers
		auto result=lhs-tc::numeric_cast< std::make_signed_t<decltype(lhs-rhs)>>(rhs);
		range_check<Lhs>(result);
		return static_cast<Lhs>(result);
	}
}

struct SRoundFloor {
	template< typename T >
	typename std::enable_if< std::is_integral<T>::value, T >::type operator()( T t ) const {
		return t;
	}
	template< typename T >
	typename std::enable_if< std::is_floating_point<T>::value, T >::type operator()( T t ) const {
		return std::floor(t);
	}
	template< typename T >
	static T PositiveOffset( T t ) {
		return 0;
	}
	template< typename T >
	static T NegativeOffset( T t ) {
		return t-1;
	}
} const roundFLOOR{};

struct SRoundNearest {
	template< typename T >
	typename std::enable_if< std::is_integral<T>::value, T >::type operator()( T t ) const {
		return t;
	}
	template< typename T >
	typename std::enable_if< std::is_floating_point<T>::value, T >::type operator()( T t ) const {
		return tc::round(t);
	}
	template< typename T >
	static T PositiveOffset( T t ) {
		return t/2;
	}
	template< typename T >
	static T NegativeOffset( T t ) {
		return (t-1)/2;
	}
} const roundNEAREST{};

struct SRoundCeil {
	template< typename T >
	typename std::enable_if< std::is_integral<T>::value, T >::type operator()( T t ) const {
		return t;
	}
	template< typename T >
	typename std::enable_if< std::is_floating_point<T>::value, T >::type operator()( T t ) const {
		return std::ceil(t);
	}
	template< typename T >
	static T PositiveOffset( T t ) {
		return t-1;
	}
	template< typename T >
	static T NegativeOffset( T t ) {
		return 0;
	}
} const roundCEIL{};

struct SRoundBanker {
} const roundBANKER{};

namespace tc {
	namespace idiv_impl {
		// The standard guarantees that integer division rounds to zero.
		// [expr.mul]/4 (oder 5.6/4) 
		template< typename Num, typename Denom, typename Round >
		Num idiv( Num num, Denom denom, Round round ) {
			static_assert( tc::is_non_char_integral<Num>::value, "" );
			static_assert( tc::is_non_char_integral< Denom >::value, "" );
			static_assert( std::is_signed<Num>::value==std::is_signed<Denom>::value, "" );
			_ASSERT( 0<denom );
			return tc::numeric_cast<Num>( ( num<0 ? num-Round::NegativeOffset(denom) : num+Round::PositiveOffset(denom) )/denom );
		}

		template< typename Num, typename Denom >
		Num idiv( Num num, Denom denom, SRoundBanker ) {
			static_assert( tc::is_non_char_integral<Num>::value, "" );
			static_assert( tc::is_non_char_integral< Denom >::value, "" );
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
	typename std::enable_if<
		tc::is_non_char_integral< Denom >::value && std::is_signed<Num>::value==std::is_signed<Denom>::value
	,Num>::type idiv( Num num, Denom denom, Round round ) {
		static_assert( tc::is_non_char_integral<Num>::value, "" );
		_ASSERT( 0!=denom );
		if( denom<0 ) { // optimized away for unsigned denom
			-tc::in_place(num);
			-tc::in_place(denom);
		}
		return idiv_impl::idiv( num, denom, round );
	}

	template< typename Num, typename Denom, typename Round >
	typename std::enable_if<
		tc::is_non_char_integral< Denom >::value && std::is_signed<Num>::value && !std::is_signed<Denom>::value
	,Num>::type idiv( Num num, Denom denom, Round round ) {
		static_assert( tc::is_non_char_integral<Num>::value, "" );
		return idiv_impl::idiv( num, tc::signed_cast(denom), round );
	}

	template< typename Num, typename Denom, typename Round >
	typename std::enable_if<
		std::is_integral< Denom >::value && !std::is_signed<Num>::value && std::is_signed<Denom>::value
	,Num>::type idiv( Num num, Denom denom, Round round ) {
		static_assert( tc::is_non_char_integral<Num>::value, "" );
		return idiv_impl::idiv( num, tc::unsigned_cast(denom), round );
	}

	template< typename Num, typename Denom, typename Round >
	typename std::enable_if<
		std::is_floating_point< Denom >::value
	,Num>::type	idiv( Num num, Denom denom, Round round ) {
		static_assert( tc::is_non_char_integral<Num>::value, "" );
		return tc::numeric_cast<Num>(round(num/denom));
	}

	/////////////////////////////////
	// scale_div

	template< typename Num, typename Denom, typename Round >
	typename std::enable_if< 
		tc::is_non_char_integral< Num >::value
	,Num>::type scale_div( Num num, Denom denom, Round round ) {
		return idiv(num,denom,round);
	}

	template< typename Num, typename Denom >
	typename std::enable_if<
		std::is_floating_point< Num >::value
	,Num>::type	scale_div( Num num, Denom denom, SRoundNearest ) {
		return tc::numeric_cast<Num>(num/denom);
	}

	template< typename Num, typename Denom >
	Num scale_div( Num num, Denom denom ) {
		return scale_div(num,denom,roundNEAREST);
	}

	template<typename T>
	typename std::enable_if< 
		tc::is_non_char_integral< T >::value
	,T>::type center( T begin, T end) {  
		return scale_div(begin + end, 2, roundFLOOR); 
	}

	template<typename T>
	typename std::enable_if< 
		std::is_floating_point< T >::value
	,T>::type center( T begin, T end) {  
		return (begin + end)/2; 
	}

	/////////////////////////////////
	// scale_mul

	template<typename T, typename Factor>
	typename std::enable_if<
		tc::is_non_char_integral< T >::value && std::is_integral< Factor >::value
	,T>::type scale_mul(T t, Factor factor, SRoundNearest ) {
		return tc::numeric_cast<T>(tc::prepare_argument<T,Factor>::prepare(t)*tc::prepare_argument<T,Factor>::prepare(factor));
	}

	template<typename T, typename Factor, typename TRound>
	typename std::enable_if<
		tc::is_non_char_integral< T >::value && std::is_floating_point< Factor >::value
	,T>::type scale_mul(T t, Factor factor, TRound round) {
		return tc::numeric_cast<T>(round(t*factor));
	}

	template<typename T, typename Factor>
	typename std::enable_if<
		std::is_floating_point< T >::value
	,T>::type scale_mul(T t, Factor factor, SRoundNearest ) {
		return tc::numeric_cast<T>(t*factor);
	}

	template<typename T, typename Factor>
	T scale_mul(T t, Factor factor) {
		return scale_mul(t,factor,roundNEAREST);
	}

	/////////////////////////////////
	// scale_muldiv

	template<typename T, typename Num, typename Den, typename TRound>
	typename std::enable_if<
		tc::is_non_char_integral< T >::value && tc::is_non_char_integral< Num >::value && tc::is_non_char_integral< Den >::value
	,T>::type scale_muldiv(T t, Num num, Den den, TRound round) {
		return tc::numeric_cast_or_identity<T>(idiv(mul(t, num), den, round));
	}

	template<typename T, typename Num, typename Den, typename TRound>
	typename std::enable_if<
		tc::is_non_char_integral< T >::value && tc::is_non_char_integral< Num >::value && tc::is_floating_point_or_similar< Den >::value
		, T>::type scale_muldiv(T t, Num num, Den den, TRound round) {
		return tc::numeric_cast_or_identity<T>(round(mul(t, num) / den));
	}

	template<typename T, typename Num, typename Den, typename TRound>
	typename std::enable_if<
		tc::is_non_char_integral< T >::value && tc::is_floating_point_or_similar< Num >::value
		, T>::type scale_muldiv(T t, Num num, Den den, TRound round) {
		return tc::numeric_cast_or_identity<T>(round(t * tc::fdiv(num, den)));
	}

	template<typename T, typename Num, typename Den>
	typename std::enable_if<
		tc::is_floating_point_or_similar< T >::value
	,T>::type scale_muldiv(T t, Num num, Den den, SRoundNearest) {
		return tc::numeric_cast_or_identity<T>(t * tc::fdiv(num, den));
	}

	template<typename T, typename Num, typename Den>
	T scale_muldiv(T const& x, Num const& num, Den const& den) {
		return scale_muldiv(x, num, den, roundNEAREST);
	}
}

