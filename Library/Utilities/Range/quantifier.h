#pragma once

#include "transform_adaptor.h"
#include "break_or_continue.h"
#include <boost/bind.hpp>
#include <utility>

/////////////////////////////////////////////
// std::all/any/none_of on ranges

namespace RANGE_PROPOSAL_NAMESPACE {

	struct bool_context {
		template< typename T >
		bool_context(T const& t) 
			: m_b(static_cast<bool>(t))
		{}
		operator bool() const { return m_b; }
	private:
		bool m_b;
	};

template< typename Rng >
bool any_of( Rng && rng ) {
	return accumulate(std::forward<Rng>(rng), false, [](bool& bAccu, bool_context b) {
		return continue_if(!( bAccu=bAccu || b ));
	} );
}

template< typename Rng >
bool all_of( Rng && rng ) {
	return accumulate(std::forward<Rng>(rng), true, [](bool& bAccu, bool_context b) {
		return continue_if(( bAccu=bAccu && b ));
	} );
}

template< typename Rng, typename Pred >
bool any_of( Rng && rng, Pred && pred ) {
	return any_of( tc::transform( std::forward<Rng>(rng), std::forward<Pred>(pred) ) );
}

template< typename Rng, typename Pred >
bool all_of( Rng && rng, Pred && pred ) {
	return all_of( tc::transform( std::forward<Rng>(rng), std::forward<Pred>(pred) ) );
}

template< typename Rng >
bool none_of( Rng && rng ) {
	return !any_of( std::forward<Rng>(rng) );
}

template< typename Rng, typename Pred >
bool none_of( Rng && rng, Pred && pred ) {
	return !any_of( std::forward<Rng>(rng), std::forward<Pred>(pred) );
}

template< typename Rng, typename T >
bool contains( Rng && rng, T const& t ) {
	return any_of( tc::transform( std::forward<Rng>(rng), boost::bind<bool>( fn_equal_to(), _1, boost::cref(t) ) ) );
}

// pair is in same order as if minmax_element( ..., operator<( bool, bool ) ) would have been used.
template< typename Rng >
std::pair<bool,bool> all_any_of( Rng const& rng ) {
	std::pair<bool,bool> pairb(true,false);
	ensure_index_range(rng)( [&](bool b) {
		pairb.first=pairb.first && b;
		pairb.second=pairb.second || b;
		return continue_if( pairb.first || !pairb.second );
	} );
	return pairb;
}

template< typename Rng, typename Pred >
std::pair<bool,bool> all_any_of( Rng const& rng, Pred && pred ) {
	return all_any_of( tc::transform(rng,std::forward<Pred>(pred)) );
}

}
