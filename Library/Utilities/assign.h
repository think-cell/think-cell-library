#pragma once
#include "Library/ErrorReporting/functors.h"

#include <boost/bind.hpp>
#include <atomic>

/////////////////////////////////////////////////////////////////////
// comparison functors, required for assign_min/assign_max

#include "return_decltype.h"
#include <boost/algorithm/string/compare.hpp>

namespace tc {

using fn_less = boost::is_less;
using fn_less_equal = boost::is_not_greater; // boost implements it as <=
using fn_equal_to = boost::is_equal;

// general functor equivalent of >, do not express as <
struct fn_greater {
	template<typename Lhs, typename Rhs>
	auto operator()( Lhs&& lhs, Rhs&& rhs ) const
		return_decltype( std::forward<Lhs>(lhs) > std::forward<Rhs>(rhs) )
};

// general functor equivalent of >=, do not express as not <
struct fn_greater_equal {
	template<typename Lhs, typename Rhs>
	auto operator()( Lhs&& lhs, Rhs&& rhs ) const
		return_decltype( std::forward<Lhs>(lhs) >= std::forward<Rhs>(rhs) )
};


/////////////////////////////////////////////////////////////////////
// assign


template< typename Var, typename Val, typename Better >
bool assign_better( Var&& var, Val&& val, Better better ) {
	if( better(VERIFYINITIALIZED(val), VERIFYINITIALIZED(var)) ) {
		std::forward<Var>(var)=std::forward<Val>(val);
		return true;
	} else {
		return false;
	}
}

template< typename Var, typename Val, typename Better >
bool assign_better( std::atomic<Var>& var, Val&& val, Better better ) {
	Var varOld=var;
	_ASSERTINITIALIZED( varOld );
	while( better(VERIFYINITIALIZED(val), varOld) ) {
		 if( var.compare_exchange_weak( varOld, val ) ) return true;
	}
	return false;
}

template< typename Var, typename Val, typename Better >
bool assign_better( std::atomic<Var>&& var, Val&& val, Better better ) =delete; // make passing rvalue ref an error

template< typename Var, typename Val >
bool change( Var&& var, Val&& val ) {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), !boost::bind<bool>(tc::fn_equal_to(), _2, _1) ); // var==val, not val==var
}

template< typename Var, typename Val >
bool change( std::atomic<Var> & var, Val&& val ) {
	_ASSERTINITIALIZED( val );
	return !boost::implicit_cast<bool>( var.exchange(val)==val );
}

template< typename Var, typename Val >
bool change( std::atomic<Var>&& var, Val&& val ); // make passing rvalue ref a linker error

template< typename Var, typename Val >
bool assign_max( Var&& var, Val&& val ) {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), boost::bind<bool>( tc::fn_less(), _2, _1 ) ); // use operator< for comparison just like std::min/max
}

template< typename Var, typename Val >
bool assign_min( Var&& var, Val&& val ) {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), tc::fn_less() );
}

template<typename Var, typename Val>
void change_with_or(Var&& var, Val&& val, bool& bChanged) {
	// accessing an uninitialized variable is undefined behavior, so don't compare for equality if var is uninitialized!
	if( VERIFYINITIALIZED(bChanged) ) {
		_ASSERTINITIALIZED( val );
		std::forward<Var>(var) = std::forward<Val>(val);
	} else {
		bChanged = tc::change( std::forward<Var>(var), std::forward<Val>(val) );
	}
}

template< typename Var, typename Val, typename Func >
bool assign_max_if_impl( Var&& var, Val&& val, Func func ) {
	return tc::assign_better( std::forward<Var>(var), std::forward<Val>(val), [&func](Val const& val, Var const& var) { return var < val && tc::bool_cast(func()); } );
}

#define tc_assign_max_if(var, val, expr) (tc::assign_max_if_impl( (var), (val), MAKE_LAZY(expr)  ))

DEFINE_FN( assign_max );
DEFINE_FN( assign_min );
DEFINE_FN( assign_better );

////////////////////////////////////
// change for float/double

// Treat float/double assignment as bit pattern assignment, to avoid NaN problems.
// Assigning NaN to NaN should be !bChanged.
// Might not work because IEEE 754 does not specify a unique bit pattern for NaNs.
// RT#5691: Also, we must not rely on comparisons involving NaN to return false, as defined by the standard, because we are using /fp:fast:
// https://connect.microsoft.com/VisualStudio/feedback/ViewFeedback.aspx?FeedbackID=518015&ppud=0&wa=wsignin1.0

template< typename Lhs, typename Rhs >
bool binary_equal( Lhs const& lhs, Rhs const& rhs ) {
	static_assert( sizeof(lhs)==sizeof(rhs), "" );
	return 0==std::memcmp(std::addressof(lhs),std::addressof(rhs),sizeof(lhs));
}

template< typename Dst, typename Src >
bool binary_change( Dst& dst, Src const& src ) {
	static_assert( sizeof(Dst)==sizeof(src), "" );
	if( !binary_equal(dst,src) ) {
		std::memcpy(std::addressof(dst),std::addressof(src),sizeof(dst));
		return true;
	} else {
		return false;
	}
}

#define change_for_float( TFloat ) \
template< typename S > \
bool change( TFloat& tVar, S&& sValue ) { \
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

#undef change_for_float

} // namespace tc