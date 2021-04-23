
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "explicit_cast.h"
#include "assert_defs.h"
#include "functors.h"
#include "return_decltype.h"
#include "variant.h" // our customized operator== may not be found by ADL in tc::equal_to
#ifndef __EMSCRIPTEN__
#include <atomic>
#endif

/////////////////////////////////////////////////////////////////////
// comparison functors, required for assign_min/assign_max


namespace tc {

namespace unbounded_adl {
	struct greatest {};

	template <typename T>
	[[nodiscard]] constexpr auto operator<(T const&, greatest) return_ctor_noexcept(std::true_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<=(T const&, greatest) return_ctor_noexcept(std::true_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<(greatest, T const&) return_ctor_noexcept(std::false_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<=(greatest, T const&) return_ctor_noexcept(std::false_type, ())

	struct least {};

	template <typename T>
	[[nodiscard]] constexpr auto operator<(T const&, least) return_ctor_noexcept(std::false_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<=(T const&, least) return_ctor_noexcept(std::false_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<(least, T const&) return_ctor_noexcept(std::true_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<=(least, T const&) return_ctor_noexcept(std::true_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<(least, greatest) return_ctor_noexcept(std::true_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<=(least, greatest) return_ctor_noexcept(std::true_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<(greatest, least) return_ctor_noexcept(std::false_type, ())

	template <typename T>
	[[nodiscard]] constexpr auto operator<=(greatest, least) return_ctor_noexcept(std::false_type, ())
}
using unbounded_adl::least;
using unbounded_adl::greatest;

////////////////////////////
// comparison functors
// Unlike std::less<> et. al., they are special-made for comparisons, so they only use operator== and operator< with constant arguments and expect noexcept and a bool return
// This avoids types having to support operator> and operator>=, which we do not want to use in our code.

// overloadable version of operator==
template<typename Lhs, typename Rhs, std::enable_if_t<
	std::is_class<Lhs>::value ||
	std::is_class<Rhs>::value ||
	std::is_pointer<Lhs>::value ||
	std::is_pointer<Rhs>::value ||
	std::is_same<std::remove_volatile_t<Lhs>, std::remove_volatile_t<Rhs>>::value
>* =nullptr>
[[nodiscard]] constexpr bool equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
	return lhs==rhs;
}

template<typename Lhs, typename Rhs, std::enable_if_t<
	!std::is_same<std::remove_volatile_t<Lhs>, std::remove_volatile_t<Rhs>>::value &&
	tc::is_actual_arithmetic<Lhs>::value &&
	tc::is_actual_arithmetic<Rhs>::value
>* =nullptr>
[[nodiscard]] constexpr bool equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
	return tc::explicit_cast<decltype(lhs+rhs)>(lhs)==tc::explicit_cast<decltype(lhs+rhs)>(rhs);
}

// overloadable version of operator<
template<typename Lhs, typename Rhs, std::enable_if_t< !(tc::is_actual_arithmetic<Lhs>::value && tc::is_actual_arithmetic<Rhs>::value) >* =nullptr >
[[nodiscard]] constexpr auto less(Lhs const& lhs, Rhs const& rhs) noexcept {
	auto result = lhs < rhs;
	static_assert(std::is_same<decltype(result), bool>::value || std::is_same<decltype(result), std::true_type>::value  || std::is_same<decltype(result), std::false_type>::value);
	return result;
}

template<typename Lhs, typename Rhs, std::enable_if_t< tc::is_actual_arithmetic<Lhs>::value && tc::is_actual_arithmetic<Rhs>::value >* =nullptr >
[[nodiscard]] constexpr auto less(Lhs const& lhs, Rhs const& rhs) noexcept {
	auto result = tc::explicit_cast<decltype(lhs+rhs)>(lhs)<tc::explicit_cast<decltype(lhs+rhs)>(rhs);
	static_assert(std::is_same<decltype(result), bool>::value || std::is_same<decltype(result), std::true_type>::value || std::is_same<decltype(result), std::false_type>::value);
	return result;
}

namespace no_adl
{
	struct[[nodiscard]] fn_equal_to{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
			return tc::equal_to(lhs,rhs);
		}
		using is_transparent = void;
	};

	struct[[nodiscard]] fn_not_equal_to{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
			return !tc::equal_to(lhs,rhs);
		}
		using is_transparent = void;
	};

	struct[[nodiscard]] fn_less{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
			return tc::less(lhs,rhs);
		}
		using is_transparent = void;

		using supremum = tc::greatest;
		using infimum = tc::least;
	};

	struct[[nodiscard]] fn_greater_equal{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
			return !tc::less(lhs,rhs);
		}
		using is_transparent = void;
	};

	struct[[nodiscard]] fn_greater{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
			return tc::less(rhs,lhs);
		}
		using is_transparent = void;

		using supremum = tc::least;
		using infimum = tc::greatest;
	};

	struct[[nodiscard]] fn_less_equal{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
			return !tc::less(rhs,lhs);
		}
		using is_transparent = void;
	};
}

using no_adl::fn_equal_to;
using no_adl::fn_not_equal_to;
using no_adl::fn_less;
using no_adl::fn_greater_equal;
using no_adl::fn_greater;
using no_adl::fn_less_equal;

/////////////////////////////////////////////////////////////////////
// assign

template< typename Better, typename Var, typename Val  >
constexpr auto assign_better( Better better, Var&& var, Val&& val ) noexcept {
	return tc::make_overload(
		[&](std::true_type b) noexcept {
			static_assert( tc::is_safely_assignable<Var&&, Val&&>::value );
			std::forward<Var>(var) = std::forward<Val>(val);
			return b;
		},
		[&](std::false_type b) noexcept {
			return b;
		},
		[&](bool b) noexcept {
			if (b) {
				static_assert( tc::is_safely_assignable<Var&&, Val&&>::value );
				std::forward<Var>(var) = std::forward<Val>(val);
				return true;
			} else {
				return false;
			}
		}
	)( better(tc::as_const(val), tc::as_const(var)) );
}

template< typename Var, typename Val >
constexpr bool change( Var&& var, Val&& val ) noexcept {
	return tc::assign_better( [](auto const& val_, auto const& var_) noexcept { return !tc::equal_to(var_, val_); }, std::forward<Var>(var), std::forward<Val>(val) ); // var==val, not val==var
}

#ifndef __EMSCRIPTEN__
template< typename Better, typename Var, typename Val >
bool assign_better( Better better, std::atomic<Var>& var, Val&& val ) noexcept {
	Var varOld=var;
	_ASSERTINITIALIZED( varOld );
	while( better(tc::as_const(VERIFYINITIALIZED(val)), tc::as_const(varOld)) ) {
		 if( var.compare_exchange_weak( varOld, val ) ) return true;
	}
	return false;
}

template< typename Better, typename Var, typename Val0, typename... Val >
bool assign_better( Better better, std::atomic<Var>&& var, Val0&& val0, Val&&... val ) noexcept =delete; // make passing rvalue ref an error

template< typename Var, typename Val >
bool change( std::atomic<Var> & var, Val&& val ) noexcept {
	_ASSERTINITIALIZED( val );
	return !tc::equal_to(var.exchange(val),val);
}

template< typename Var, typename Val >
bool change( std::atomic<Var>&& var, Val&& val ) noexcept; // make passing rvalue ref a linker error
#endif // __EMSCRIPTEN__

template< typename Better, typename Var, typename... Val, std::enable_if_t<1<sizeof...(Val)>* = nullptr >
constexpr bool assign_better( Better better, Var&& var, Val&&... val) noexcept {
	bool b=false;
	static_cast<void>(std::initializer_list<bool>{(b=(tc::assign_better(better, var, std::forward<Val>(val)) || b))...});
	return b;
}

template< typename Var, typename Val0, typename... Val >
constexpr bool assign_max( Var&& var, Val0&& val0, Val&&... val ) noexcept {
	return tc::assign_better( tc::fn_greater(), std::forward<Var>(var), std::forward<Val0>(val0), std::forward<Val>(val)... ); // use operator< for comparison just like tc::min/max
}

template< typename Var, typename Val0, typename... Val>
constexpr bool assign_min( Var&& var, Val0&& val0, Val&&... val ) noexcept {
	return tc::assign_better( tc::fn_less(), std::forward<Var>(var), std::forward<Val0>(val0), std::forward<Val>(val)... );
}

template<typename Var, typename Val>
void change_with_or(Var&& var, Val&& val, bool& bChanged) noexcept {
	// accessing an uninitialized variable is undefined behavior, so don't compare for equality if var is uninitialized!
	if( VERIFYINITIALIZED(bChanged) ) {
		_ASSERTINITIALIZED( val );
		std::forward<Var>(var) = std::forward<Val>(val);
	} else {
		bChanged = tc::change( std::forward<Var>(var), std::forward<Val>(val) );
	}
}

DEFINE_FN( assign_max );
DEFINE_FN( assign_min );

template< typename Better >
[[nodiscard]] auto fn_assign_better(Better better) {
	return [better=tc_move(better)](auto&& var, auto&& val0, auto&&... val) noexcept {
		return tc::assign_better(better, tc_move_if_owned(var), tc_move_if_owned(val0), tc_move_if_owned(val)...);
	};
}

////////////////////////////////////
// change for float/double

// Treat float/double assignment as bit pattern assignment, to avoid NaN problems.
// Assigning NaN to NaN should be !bChanged.
// Might not work because IEEE 754 does not specify a unique bit pattern for NaNs.
// RT#5691: Also, we must not rely on comparisons involving NaN to return false, as defined by the standard, because we are using /fp:fast:
// https://connect.microsoft.com/VisualStudio/feedback/ViewFeedback.aspx?FeedbackID=518015&ppud=0&wa=wsignin1.0

template< typename Lhs, typename Rhs >
[[nodiscard]] bool binary_equal( Lhs const& lhs, Rhs const& rhs ) noexcept {
	STATICASSERTEQUAL( sizeof(lhs), sizeof(rhs) );
	return 0==std::memcmp(std::addressof(lhs),std::addressof(rhs),sizeof(lhs));
}

template< typename Dst, typename Src >
bool binary_change( Dst& dst, Src const& src ) noexcept {
	STATICASSERTEQUAL( sizeof(Dst), sizeof(src) );
	if( !binary_equal(dst,src) ) {
		std::memcpy(std::addressof(dst),std::addressof(src),sizeof(dst));
		return true;
	} else {
		return false;
	}
}

#pragma push_macro("change_for_float")
#define change_for_float( TFloat ) \
template< typename S > \
bool change( TFloat& tVar, S&& sValue ) noexcept { \
	TFloat const fNAN=std::numeric_limits<TFloat>::quiet_NaN(); \
	_ASSERTINITIALIZED( tVar ); \
	_ASSERT( !std::isnan( tVar ) || binary_equal(tVar,fNAN) ); \
	_ASSERTINITIALIZED( sValue ); \
	TFloat tValue=std::forward<S>(sValue); \
	_ASSERT( !std::isnan( tValue ) || binary_equal(tValue,fNAN) ); \
	return binary_change( tVar, tValue ); \
}

change_for_float( float )
change_for_float( double )

#pragma pop_macro("change_for_float")

} // namespace tc
