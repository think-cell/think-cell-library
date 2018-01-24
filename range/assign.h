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
#include "range_defines.h"
#include "functors.h"
#include "return_decltype.h"
#include <atomic>

/////////////////////////////////////////////////////////////////////
// comparison functors, required for assign_min/assign_max


namespace tc {

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
bool less(Lhs const& lhs, Rhs const& rhs) noexcept {
	return lhs<rhs;
}

template<typename Lhs, typename Rhs, std::enable_if_t< tc::is_actual_arithmetic<Lhs>::value && tc::is_actual_arithmetic<Rhs>::value >* =nullptr >
bool less(Lhs const& lhs, Rhs const& rhs) noexcept {
	return tc::explicit_cast<decltype(lhs+rhs)>(lhs)<tc::explicit_cast<decltype(lhs+rhs)>(rhs);
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
	bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return tc::less(lhs,rhs);
	}
	using is_transparent = void;
};

struct fn_greater_equal {
	template<typename Lhs, typename Rhs>
	bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return !tc::less(lhs,rhs);
	}
	using is_transparent = void;
};

struct fn_greater {
	template<typename Lhs, typename Rhs>
	bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return tc::less(rhs,lhs);
	}
	using is_transparent = void;
};

struct fn_less_equal {
	template<typename Lhs, typename Rhs>
	bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
		return !tc::less(rhs,lhs);
	}
	using is_transparent = void;
};

/////////////////////////////////////////////////////////////////////
// assign

template< typename Var, typename Val, typename Better >
bool assign_better( Var&& var, Val&& val, Better better ) noexcept {
	static_assert( tc::is_safely_assignable<Var&&, Val&&>::value );
	if( better(VERIFYINITIALIZED(val), VERIFYINITIALIZED(var)) ) {
		std::forward<Var>(var)=std::forward<Val>(val);
		return true;
	} else {
		return false;
	}
}

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
bool change( Var&& var, Val&& val ) noexcept {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), [](auto const& val_, auto const& var_) noexcept { return !tc::equal_to(var_, val_); } ); // var==val, not val==var
}

template< typename Var, typename Val >
bool change( std::atomic<Var> & var, Val&& val ) noexcept {
	_ASSERTINITIALIZED( val );
	return !tc::equal_to(var.exchange(val),val);
}

template< typename Var, typename Val >
bool change( std::atomic<Var>&& var, Val&& val ) noexcept; // make passing rvalue ref a linker error

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
	return [func_=tc_move(func)](auto&&... args) noexcept {
		return tc::assign_better(std::forward<decltype(args)>(args)..., func_);
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