
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "explicit_cast.h"
#include "range_defines.h"
#include "functors.h"
#include "return_decltype.h"
#ifndef __EMSCRIPTEN__
#include <atomic>
#endif

/////////////////////////////////////////////////////////////////////
// comparison functors, required for assign_min/assign_max


namespace tc {

namespace unbounded_adl {
	struct greatest {};

	template <typename T>
	constexpr auto operator<(T const&, greatest) noexcept return_ctor(std::true_type, ())

	template <typename T>
	constexpr auto operator<=(T const&, greatest) noexcept return_ctor(std::true_type, ())

	template <typename T>
	constexpr auto operator<(greatest, T const&) noexcept return_ctor(std::false_type, ())

	template <typename T>
	constexpr auto operator<=(greatest, T const&) noexcept return_ctor(std::false_type, ())

	struct least {};

	template <typename T>
	constexpr auto operator<(T const&, least) noexcept return_ctor(std::false_type, ())

	template <typename T>
	constexpr auto operator<=(T const&, least) noexcept return_ctor(std::false_type, ())

	template <typename T>
	constexpr auto operator<(least, T const&) noexcept return_ctor(std::true_type, ())

	template <typename T>
	constexpr auto operator<=(least, T const&) noexcept return_ctor(std::true_type, ())

	template <typename T>
	constexpr auto operator<(least, greatest) noexcept return_ctor(std::true_type, ())

	template <typename T>
	constexpr auto operator<=(least, greatest) noexcept return_ctor(std::true_type, ())

	template <typename T>
	constexpr auto operator<(greatest, least) noexcept return_ctor(std::false_type, ())

	template <typename T>
	constexpr auto operator<=(greatest, least) noexcept return_ctor(std::false_type, ())
}
using unbounded_adl::least;
using unbounded_adl::greatest;

////////////////////////////
// comparison functors
// Unlike std::less<> et. al., they are special-made for comparisons, so they only use operator== and operator< with constant arguments and expect noexcept and a bool return
// This avoids types having to support operator> and operator>=, which we do not want to use in our code.

// overloadable version of operator==
template<typename Lhs, typename Rhs, std::enable_if_t< !(tc::is_actual_arithmetic<Lhs>::value && tc::is_actual_arithmetic<Rhs>::value) >* =nullptr >
bool equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
	return lhs==rhs;
}

template<typename Lhs, typename Rhs, std::enable_if_t< tc::is_actual_arithmetic<Lhs>::value && tc::is_actual_arithmetic<Rhs>::value >* =nullptr >
bool equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
	return tc::explicit_cast<decltype(lhs+rhs)>(lhs)==tc::explicit_cast<decltype(lhs+rhs)>(rhs);
}

// overloadable version of operator<
template<typename Lhs, typename Rhs, std::enable_if_t< !(tc::is_actual_arithmetic<Lhs>::value && tc::is_actual_arithmetic<Rhs>::value) >* =nullptr >
auto less(Lhs const& lhs, Rhs const& rhs) noexcept {
	auto result = lhs < rhs;
	static_assert(std::is_same<decltype(result), bool>::value || std::is_same<decltype(result), std::true_type>::value  || std::is_same<decltype(result), std::false_type>::value);
	return result;
}

template<typename Lhs, typename Rhs, std::enable_if_t< tc::is_actual_arithmetic<Lhs>::value && tc::is_actual_arithmetic<Rhs>::value >* =nullptr >
auto less(Lhs const& lhs, Rhs const& rhs) noexcept {
	auto result = tc::explicit_cast<decltype(lhs+rhs)>(lhs)<tc::explicit_cast<decltype(lhs+rhs)>(rhs);
	static_assert(std::is_same<decltype(result), bool>::value || std::is_same<decltype(result), std::true_type>::value || std::is_same<decltype(result), std::false_type>::value);
	return result;
}

struct fn_equal_to {
	template<typename Lhs, typename Rhs>
	bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return tc::equal_to(lhs,rhs);
	}
	using is_transparent=void;
};

struct fn_not_equal_to {
	template<typename Lhs, typename Rhs>
	bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return !tc::equal_to(lhs,rhs);
	}
	using is_transparent = void;
};

struct fn_less {
	template<typename Lhs, typename Rhs>
	auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return tc::less(lhs,rhs);
	}
	using is_transparent = void;

	using supremum = tc::greatest;
	using infimum = tc::least;
};

struct fn_greater_equal {
	template<typename Lhs, typename Rhs>
	auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return !tc::less(lhs,rhs);
	}
	using is_transparent = void;
};

struct fn_greater {
	template<typename Lhs, typename Rhs>
	auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return tc::less(rhs,lhs);
	}
	using is_transparent = void;

	using supremum = tc::least;
	using infimum = tc::greatest;
};

struct fn_less_equal {
	template<typename Lhs, typename Rhs>
	auto operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return !tc::less(rhs,lhs);
	}
	using is_transparent = void;
};

/////////////////////////////////////////////////////////////////////
// assign

template< typename Var, typename Val, typename Better >
auto assign_better( Var&& var, Val&& val, Better better ) noexcept {
	return tc::make_overload<bool>(
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
	)( better(VERIFYINITIALIZED(val), VERIFYINITIALIZED(var)) );
}

template< typename Var, typename Val >
bool change( Var&& var, Val&& val ) noexcept {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), [](auto const& val_, auto const& var_) noexcept { return !tc::equal_to(var_, val_); } ); // var==val, not val==var
}

#ifndef __EMSCRIPTEN__
template< typename Var, typename Val, typename Better >
bool assign_better( std::atomic<Var>& var, Val&& val, Better better ) noexcept {
	Var varOld=var;
	_ASSERTINITIALIZED( varOld );
	while( better(VERIFYINITIALIZED(val), varOld) ) {
		 if( var.compare_exchange_weak( varOld, val ) ) return true;
	}
	return false;
}

template< typename Var, typename Val, typename Better >
bool assign_better( std::atomic<Var>&& var, Val&& val, Better better ) noexcept =delete; // make passing rvalue ref an error

template< typename Var, typename Val >
bool change( std::atomic<Var> & var, Val&& val ) noexcept {
	_ASSERTINITIALIZED( val );
	return !tc::equal_to(var.exchange(val),val);
}

template< typename Var, typename Val >
bool change( std::atomic<Var>&& var, Val&& val ) noexcept; // make passing rvalue ref a linker error
#endif // __EMSCRIPTEN__

template< typename Var, typename Val >
bool assign_max( Var&& var, Val&& val ) noexcept {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), tc::fn_greater() ); // use operator< for comparison just like tc::min/max
}

template< typename Var, typename Val >
bool assign_min( Var&& var, Val&& val ) noexcept {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), tc::fn_less() );
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

template< typename Func >
auto fn_assign_better(Func func) {
	return [func=tc_move(func)](auto&&... args) noexcept {
		return tc::assign_better(std::forward<decltype(args)>(args)..., func);
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
bool binary_equal( Lhs const& lhs, Rhs const& rhs ) noexcept {
	static_assert( sizeof(lhs)==sizeof(rhs) );
	return 0==std::memcmp(std::addressof(lhs),std::addressof(rhs),sizeof(lhs));
}

template< typename Dst, typename Src >
bool binary_change( Dst& dst, Src const& src ) noexcept {
	static_assert( sizeof(Dst)==sizeof(src) );
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
