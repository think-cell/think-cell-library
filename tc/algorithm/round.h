
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/explicit_cast.h"
#include "../base/return_decltype.h"
#include "../base/inplace.h"
#include "../base/type_traits.h"
#include "../base/integer.h"
#include "compare.h"
#include <type_traits>
#include <chrono>

namespace tc {
	namespace floor_detail {
		template<typename T>
		constexpr T constexpr_abs(T t) noexcept { // TODO: replace by std::abs(...) when it becomes constexpr
			if( 0 <= t ) {
				return t;
			} else {
				return -t;
			}
		}

		template<std::floating_point TFloat> requires (std::numeric_limits<TFloat>::digits/*MANT_DIG*/ <= std::numeric_limits<std::intmax_t>::digits)
		constexpr TFloat constexpr_floor(TFloat f) noexcept { // TODO: replace by std::floor(...) when it becomes constexpr
			using mantint_t = typename tc::int_least_t<std::numeric_limits<TFloat>::digits/*MANT_DIG*/>;
			static_assert(std::numeric_limits<mantint_t>::lowest() < -std::numeric_limits<mantint_t>::max());
			return f != f || static_cast<TFloat>(std::numeric_limits<mantint_t>::max()) <= constexpr_abs(f)
				? f
				: static_cast<mantint_t>(f) <= f
					? static_cast<TFloat>(static_cast<mantint_t>(f))
					: static_cast<TFloat>(static_cast<mantint_t>(f) - 1);
		}

		template<std::floating_point TFloat> requires (std::numeric_limits<std::intmax_t>::digits < std::numeric_limits<TFloat>::digits)
		constexpr TFloat constexpr_floor(TFloat f) noexcept { // TODO: replace by std::floor(...) when it becomes constexpr
			static_assert(std::numeric_limits<std::intmax_t>::lowest() < -std::numeric_limits<std::intmax_t>::max());
			if(f != f) {
				return f;
			} else {
				_ASSERT(constexpr_abs(f)<=static_cast<TFloat>(std::numeric_limits<std::intmax_t>::max())); // depends on non-standard large integer types otherwise
				return static_cast<std::intmax_t>(f) <= f
					? static_cast<TFloat>(static_cast<std::intmax_t>(f))
					: static_cast<TFloat>(static_cast<std::intmax_t>(f) - 1);
			}
		}
	}

	template<std::floating_point TFloat>
	constexpr TFloat floor(TFloat f) noexcept { // TODO: replace by std::floor(...) when it becomes constexpr
		if(std::is_constant_evaluated()) {
			return tc::floor_detail::constexpr_floor(f);
		} else {
			return std::floor(f);
		}
	}

	//////////////////////////////
	// round

	template< typename T >
	[[nodiscard]] constexpr T round( T t ) noexcept {
		static_assert( std::floating_point<T> );
		return tc::floor(t+static_cast<T>(.5));
	}

	//////////////////////////////
	// type-safe multiplication and division
	namespace no_adl {
		template<tc::actual_integer auto val>
		struct integer_value_prefer_unsigned {
			using type = tc::int_value_least_t<val>;
		};

		template<tc::actual_integer auto val> requires (0<=val)
		struct integer_value_prefer_unsigned<val> {
			using type = tc::uint_value_least_t<val>;
		};

		template< typename T1, typename T2 >
		struct TMultiply {};

		template< tc::actual_integer_like T1, tc::actual_integer_like T2 > requires
			std::numeric_limits<T1>::is_signed || std::numeric_limits<T2>::is_signed
		struct TMultiply<T1, T2> {
			// For 2s complement representation,
			// in case of both operands signed, -(2^31) * -(2^31) = -2^62, which needs 62 bits (not including sign bit) to be represented.
			// In case of one unsigned and one signed, (2^32-1) * -2^31 = -2^63 + 2^31, which needs 63 bits (not including sign bit) to be represented.
			using type=tc::int_least_t<std::numeric_limits<T1>::digits+std::numeric_limits<T2>::digits>;
		};

		template< tc::actual_integer_like T1, tc::actual_integer_like T2 > requires
			(!std::numeric_limits<T1>::is_signed) && (!std::numeric_limits<T2>::is_signed)
		struct TMultiply<T1, T2> {
			using type=tc::uint_least_t<std::numeric_limits<T1>::digits+std::numeric_limits<T2>::digits>;
		};

		template< typename T1, typename T2 > requires std::floating_point< T1 > || std::floating_point< T2 >
		struct TMultiply<T1, T2> final {
			using type = decltype(std::declval<T1>()*std::declval<T2>());
		};

		template<tc::actual_integer_like T1, tc::actual_integer auto val2> requires (1==val2)
		struct TMultiply<T1, tc::constant<val2>> {
			using type = T1;
		};

		template<tc::actual_integer_like T1, tc::actual_integer auto val2> requires (1!=val2)
		struct TMultiply<T1, tc::constant<val2>>: TMultiply<T1, typename integer_value_prefer_unsigned<val2>::type> {
		};

		template<tc::actual_integer auto val1, tc::actual_integer_like T2>
		struct TMultiply<tc::constant<val1>, T2> final: TMultiply<T2, tc::constant<val1>> {};
	}

	template< typename Lhs, typename Rhs >
	[[nodiscard]]
	typename no_adl::TMultiply<Lhs,Rhs>::type
	constexpr mul( Lhs lhs, Rhs rhs ) noexcept {
		return tc::explicit_cast<typename no_adl::TMultiply<Lhs,Rhs>::type>(lhs)*tc::explicit_cast<typename no_adl::TMultiply<Lhs,Rhs>::type>(rhs);
	}

	template<tc::actual_integer auto val>
	using least_integer_constant_prefer_unsigned = tc::constant<tc::explicit_cast<typename no_adl::integer_value_prefer_unsigned<val>::type>(val)>;

	template<tc::actual_integer auto valLhs, tc::actual_integer auto valRhs>
	[[nodiscard]] consteval auto mul(tc::constant<valLhs> lhs, tc::constant<valRhs> rhs) noexcept {
		return tc::least_integer_constant_prefer_unsigned<tc::mul(
			tc::least_integer_constant_prefer_unsigned<valLhs>::value,
			tc::least_integer_constant_prefer_unsigned<valRhs>::value
		)>::value;
	}

	template<tc::actual_integer_like Lhs, tc::actual_integer auto valRhs> requires (0==valRhs)
	[[nodiscard]] constexpr auto mul(Lhs lhs, tc::constant<valRhs> rhs) noexcept {
		return tc::least_integer_constant_prefer_unsigned<0>::value;
	}

	template<tc::actual_integer auto valLhs, tc::actual_integer_like Rhs> requires (0==valLhs)
	[[nodiscard]] constexpr auto mul(tc::constant<valLhs> lhs, Rhs rhs) noexcept {
		return mul(rhs, lhs);
	}

	template<typename T>
	[[nodiscard]] constexpr auto sqr(T const& x)
		return_decltype_noexcept( mul(x,x) )

#ifdef _MSC_VER
	#pragma float_control(precise, on, push) // RT#17630: Disable compiler optimization num/den ---> num*(1/den) resulting in tc::fdiv(1287,100)==12.870000000000001
#endif
	[[nodiscard]] inline constexpr 
#ifdef __clang__
		__attribute__((optnone)) // Disable compiler optimization num/den ---> num*(1/den) resulting in tc::fdiv(49,49)==0.99999999999999989
#endif
	double fdiv(double const num, double const den) noexcept {
		return num/den;
	}
#ifdef _MSC_VER
	#pragma float_control(pop)
#endif
	
	template< typename Num, typename Den >
	[[nodiscard]] constexpr double fdiv(Num num, Den den) noexcept {
		return tc::fdiv(static_cast<double>(num), static_cast<double>(den));
	}

	struct SRoundFloor final {
		using is_rounding = void;

		template< std::integral T >
		constexpr T operator()( T t ) const& noexcept {
			return t;
		}
		template< std::floating_point T >
		constexpr T operator()( T t ) const& noexcept {
			return tc::floor(t);
		}
		template< typename T >
		static constexpr T PositiveOffset( T t ) noexcept {
			return 0;
		}
		template< typename T >
		static constexpr T NegativeOffset( T t ) noexcept {
			return t-1;
		}
	} inline constexpr roundFLOOR{};

	struct SRoundNearest final {
		using is_rounding = void;

		template< std::integral T >
		constexpr T operator()( T t ) const& noexcept {
			return t;
		}
		template< std::floating_point T >
		constexpr T operator()( T t ) const& noexcept {
			return tc::round(t);
		}
		template< typename T >
		static constexpr T PositiveOffset( T t ) noexcept {
			return t/2;
		}
		template< typename T >
		static constexpr T NegativeOffset( T t ) noexcept {
			return (t-1)/2;
		}
	} inline constexpr roundNEAREST{};

	struct SRoundAwayFromZero final {
		using is_rounding = void;

		template< std::integral T >
		constexpr T operator()( T t ) const& noexcept {
			return t;
		}
		template< std::floating_point T >
		T operator()( T t ) const& noexcept {
			return std::round(t);
		}
		template< typename T >
		static constexpr T PositiveOffset( T t ) noexcept {
			return t/2;
		}
		template< typename T >
		static constexpr T NegativeOffset( T t ) noexcept {
			return t/2;
		}
	} inline constexpr roundAWAYFROMZERO{};

	struct SRoundCeil final {
		using is_rounding = void;

		template< std::integral T >
		constexpr T operator()( T t ) const& noexcept {
			return t;
		}
		template< std::floating_point T >
		T operator()( T t ) const& noexcept {
			return std::ceil(t);
		}
		template< typename T >
		static constexpr T PositiveOffset( T t ) noexcept {
			return t-1;
		}
		template< typename T >
		static constexpr T NegativeOffset( T t ) noexcept {
			return 0;
		}
	} inline constexpr roundCEIL{};

	struct SRoundBanker final {
		using is_rounding = void;
	} inline constexpr roundBANKER{};

	struct SNoRounding final {
		using is_rounding = void;
	} inline constexpr norounding{};

	template<typename Round>
	concept rounding = requires { typename Round::is_rounding; };

	namespace idiv_impl {
		// The standard guarantees that integer division rounds to zero.
		// [expr.mul]/4 (oder 5.6/4) 
		template< typename Num, typename Denom, tc::rounding Round >
		constexpr Num idiv( Num num, Denom denom, Round round ) noexcept {
			static_assert( tc::actual_integer_like<Num> );
			static_assert( tc::actual_integer_like< Denom > );
			STATICASSERTEQUAL( std::numeric_limits<Num>::is_signed, std::numeric_limits<Denom>::is_signed );
			_ASSERTE( 0<denom );
			if constexpr(std::same_as<Round, SRoundBanker>) {
				auto result = num / denom;
				auto remainder = num - result * denom;
				if(remainder<0) -tc::inplace(remainder);
				if(tc_auto_cref(order, tc::compare(denom,remainder * 2)); std::is_lt(order) || tc::is_eq(order) && 0 != result % 2) {
					result += num < 0 ? -1 : 1;
				}
				tc_return_cast(tc_move(result));
			} else if constexpr(std::same_as<Round, SNoRounding>) {
				_ASSERTEQUAL(num%denom, 0);
				tc_return_cast(num/denom);
			} else {
				tc_return_cast( ( num<0 ? num-Round::NegativeOffset(denom) : num+Round::PositiveOffset(denom) )/denom );
			}
		}
	}

	template< typename Num, typename Denom, tc::rounding Round >
	[[nodiscard]] constexpr Num idiv(Num num, Denom denom, Round round) noexcept {
		static_assert( tc::actual_integer_like<Num> );
		if constexpr( tc::actual_integer_like<Denom> ) {
			if constexpr( std::numeric_limits<Num>::is_signed ) {
				if constexpr( std::numeric_limits<Denom>::is_signed ) {
					if (denom < 0) {
						-tc::inplace(num);
						-tc::inplace(denom);
					}
					return idiv_impl::idiv(num, denom, round);
				} else {
					return tc::idiv/*not idiv_impl::idiv*/( num, tc::as_signed(denom), round );
				}
			} else {
				return idiv_impl::idiv( num, tc::as_unsigned(denom), round );
			}
		} else {
			static_assert( std::floating_point< Denom > );
			tc_return_cast(round(num/denom));
		}
	}

	template< typename Num, typename Denom, tc::rounding Round >
	[[nodiscard]] std::pair<Num,Denom> idivmod( Num num, Denom denom, Round round ) noexcept {
		Num div=tc::idiv(num,denom,round);
		Denom mod=num-div*denom;
		return std::make_pair(div,mod);
	}

	/////////////////////////////////
	// scale_div

	template< tc::actual_integer Num, typename Denom, tc::rounding Round>
	[[nodiscard]] constexpr Num scale_div( Num num, Denom denom, Round round ) noexcept {
		return tc::idiv(num,denom,round);
	}

	template< std::floating_point Num, typename Denom>
	[[nodiscard]] constexpr Num scale_div( Num num, Denom denom, SRoundNearest ) noexcept {
		tc_return_cast(num/denom);
	}

	template< typename Num, typename Denom >
	[[nodiscard]] constexpr Num scale_div( Num num, Denom denom ) noexcept {
		return tc::scale_div(num,denom,tc::roundNEAREST);
	}

	template< bool bGeneralized, tc::actual_integer T>
	[[nodiscard]] constexpr T internal_lower_half(T t) noexcept {
		return tc::scale_div(t, 2, tc::roundFLOOR);
	}

	template< bool bGeneralized, std::floating_point T>
	[[nodiscard]] constexpr T internal_lower_half(T t) noexcept {
		return t / 2;
	}

	template< bool bGeneralized, typename Rep, typename Period> requires bGeneralized
	[[nodiscard]] constexpr std::chrono::duration<Rep, Period> internal_lower_half(std::chrono::duration<Rep, Period> const& dur) noexcept {
		return std::chrono::duration<Rep, Period>(tc::idiv(dur.count(), 2, tc::roundFLOOR));
	}

	template< typename T >
	[[nodiscard]] constexpr auto lower_half(T&& t) return_decltype_noexcept( tc::internal_lower_half</*bGeneralized*/ false>(tc_move_if_owned(t)) )

	template< bool bGeneralized, typename T >
	[[nodiscard]] constexpr auto internal_midpoint(T const& begin, T const& end) noexcept {
		return begin + tc::internal_lower_half<bGeneralized>(end - begin);
	}

	template< typename T >
	[[nodiscard]] constexpr auto midpoint(T const& begin, T const& end) return_decltype_noexcept( tc::internal_midpoint</*bGeneralized*/false>(begin, end) )

	/////////////////////////////////
	// scale_mul

	template<tc::actual_integer T, std::integral Factor>
	[[nodiscard]] constexpr T scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		tc_return_cast(tc::prepare_argument<T,Factor>(t)*tc::prepare_argument<T,Factor>(factor));
	}

	template<tc::actual_integer T, std::floating_point Factor, tc::rounding TRound>
	[[nodiscard]] constexpr T scale_mul(T t, Factor factor, TRound round) noexcept {
		tc_return_cast(round(t*factor));
	}

	template<std::floating_point T, typename Factor>
	[[nodiscard]] constexpr T scale_mul(T t, Factor factor, SRoundNearest ) noexcept {
		tc_return_cast(t*factor);
	}

	template<typename T, typename Factor>
	[[nodiscard]] constexpr T scale_mul(T t, Factor factor) noexcept {
		return scale_mul(t,factor,tc::roundNEAREST);
	}

	/////////////////////////////////
	// scale_muldiv

	namespace no_adl {
		template<typename T>
		struct is_actual_integer_like_or_constant_impl final: tc::constant<false> {};

		template<tc::actual_integer_like T>
		struct is_actual_integer_like_or_constant_impl<T> final: tc::constant<true> {
			using int_t = T;
		};

		template<tc::actual_integer auto val>
		struct is_actual_integer_like_or_constant_impl<tc::constant<val>> final: tc::constant<true> {
			using int_t = typename tc::constant<val>::value_type;
		};
	}
	template<typename T>
	using is_actual_integer_like_or_constant = no_adl::is_actual_integer_like_or_constant_impl<std::remove_cv_t<T>>;

	template<typename T>
	concept actual_integer_like_or_constant = tc::is_actual_integer_like_or_constant<T>::value;

	template<tc::actual_integer_like_or_constant T, tc::actual_integer_like_or_constant Num, tc::actual_integer Den, tc::rounding TRound>
	[[nodiscard]] constexpr typename tc::is_actual_integer_like_or_constant<T>::int_t scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		tc_return_cast(idiv(mul(t, num), den, round));
	}

	template<tc::actual_integer_like_or_constant T, tc::actual_integer_like_or_constant Num, tc::floating_point_like Den, tc::rounding TRound>
	[[nodiscard]] constexpr typename tc::is_actual_integer_like_or_constant<T>::int_t scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		tc_return_cast(round(mul(t, num) / den));
	}

	template<tc::actual_integer_like_or_constant T, tc::floating_point_like Num, typename Den, tc::rounding TRound>
	[[nodiscard]] constexpr typename tc::is_actual_integer_like_or_constant<T>::int_t scale_muldiv(T t, Num num, Den den, TRound round) noexcept {
		tc_return_cast(round(t * tc::fdiv(num, den)));
	}

	template<tc::floating_point_like T, typename Num, typename Den>
	[[nodiscard]] constexpr auto scale_muldiv(T t, Num num, Den den, SRoundNearest) noexcept {
		return tc::reluctant_explicit_cast<T>(t * tc::fdiv(num, den));
	}

	template<typename T, typename Num, typename Den>
	[[nodiscard]] constexpr auto scale_muldiv(T const& x, Num const& num, Den const& den) noexcept {
		return scale_muldiv(x, num, den, tc::roundNEAREST);
	}

	struct fraction {
		int m_nNum;
		int m_nDen;
	};

	template<typename T>
	[[nodiscard]] constexpr auto scale_muldiv(T const& x, tc::fraction const& fracn) noexcept {
		return scale_muldiv(x, fracn.m_nNum, fracn.m_nDen);
	}

	/////////////////////////////////
	// rounding_cast
	template<typename Dst, typename Src, tc::rounding TRound>
		requires
			(tc::actual_integer<tc::decay_t<Src>> && (tc::actual_integer<Dst> || tc::floating_point_like<Dst>))
			|| (tc::floating_point_like<tc::decay_t<Src>> && tc::floating_point_like<Dst>)
	[[nodiscard]] constexpr decltype(auto) rounding_cast(Src&& x, TRound) noexcept {
		return tc::reluctant_explicit_cast<Dst>(tc_move_if_owned(x));
	}

	template<tc::actual_integer Dst, typename Src, tc::rounding TRound>
		 requires tc::floating_point_like<tc::decay_t<Src>>
	[[nodiscard]] constexpr decltype(auto) rounding_cast(Src&& x, TRound round) noexcept {
		return tc::reluctant_explicit_cast<Dst>(round(tc_move_if_owned(x)));
	}

	template<typename Dst, typename Src>
	[[nodiscard]] constexpr decltype(auto) rounding_cast(Src&& x) noexcept {
		return rounding_cast<Dst>(tc_move_if_owned(x), roundNEAREST);
	}

	template<typename Lhs, typename Rhs, tc::rounding TRound>
	constexpr void assign_rounding_cast(Lhs& lhs, Rhs&& rhs, TRound round) noexcept {
		lhs=tc::rounding_cast<Lhs>(tc_move_if_owned(rhs), round);
	}

	template<typename Lhs, typename Rhs>
	constexpr void assign_rounding_cast(Lhs& lhs, Rhs&& rhs) noexcept {
		lhs=tc::rounding_cast<Lhs>(tc_move_if_owned(rhs));
	}

	namespace no_adl {
		struct fn_div {
			template<typename Num, typename Denom, typename Quot = decltype(std::declval<Num>() / std::declval<Denom>())>
			[[nodiscard]] constexpr Quot operator()(Num&& num, Denom&& denom) const& noexcept {
				if constexpr( tc::actual_integer_like<tc::decay_t<Num>> && tc::actual_integer_like<tc::decay_t<Denom>> ) {
					static_assert( dependent_false<Num, Denom>::value, "Do not rely on language int/int-behavior, use tc::scale_div " );
					return tc::idiv(tc::explicit_cast<Quot>(tc_move_if_owned(num)), tc_move_if_owned(denom), tc::roundNEAREST);
				} else {
					static_assert( std::floating_point<tc::decay_t<Num>> || std::floating_point<tc::decay_t<Denom>> || std::is_class<tc::decay_t<Num>>::value || std::is_class<tc::decay_t<Denom>>::value );
					static_assert( !tc::instance<tc::decay_t<Num>, tc::size_proxy> );
					static_assert( !tc::instance<tc::decay_t<Denom>, tc::size_proxy> );
					return tc_move_if_owned(num) / tc_move_if_owned(denom);
				}
			}
		};

		struct fn_assign_div {
			template<typename Num, typename Denom>
			[[nodiscard]] constexpr Num& operator()(Num& num, Denom&& denom) const& noexcept {
				if constexpr( tc::actual_integer_like<tc::decay_t<Num>> ) {
					static_assert( dependent_false<Num, Denom>::value, "Do not rely on language int/int-behavior, use tc::scale_div " );
					num = tc::idiv(num, tc_move_if_owned(denom), tc::roundNEAREST);
				} else {
					static_assert( std::floating_point<tc::decay_t<Num>> || std::is_class<tc::decay_t<Num>>::value );
					static_assert( !tc::instance<tc::decay_t<Num>, tc::size_proxy> );
					num /= tc_move_if_owned(denom);
				}
				return num;
			}
		};
	}
	using no_adl::fn_div;
	using no_adl::fn_assign_div;

	namespace explicit_convert_adl {
		template<tc::actual_integer Target, tc::actual_arithmetic Source, tc::rounding Round>
		[[nodiscard]] constexpr Target explicit_convert_impl(adl_tag_t, std::type_identity<Target>, Source src, Round round) noexcept {
			tc_return_cast(round(src));
		}
	}
}
