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

#include "range_fwd.h"
#include "transform_adaptor.h"
#include "break_or_continue.h"
#include <utility>

/////////////////////////////////////////////
// std::all/any/none_of on ranges

namespace tc {

DEFINE_FN(bool_cast)

template< typename Rng >
bool any_of(Rng&& rng) noexcept {
	//return !tc::empty( tc::filter( std::forward<Rng>(rng), tc::fn_bool_cast() ) );
	return tc::break_==tc::for_each(std::forward<Rng>(rng), [](bool_context b) noexcept {return tc::continue_if(!b);});
}

template< typename Rng >
bool all_of(Rng&& rng) noexcept {
	//return tc::empty( tc::filter( std::forward<Rng>(rng), tc::not_fn(tc::fn_bool_cast()) ) );
	return tc::continue_==tc::for_each(std::forward<Rng>(rng), [](bool_context b) noexcept {return tc::continue_if(b);});
}

template< typename Rng, typename Pred >
bool any_of(Rng&& rng, Pred&& pred) noexcept {
	return any_of( tc::transform( std::forward<Rng>(rng), std::forward<Pred>(pred) ) );
}

template< typename Rng, typename Pred >
bool all_of(Rng&& rng, Pred&& pred) noexcept {
	return all_of( tc::transform( std::forward<Rng>(rng), std::forward<Pred>(pred) ) );
}

template< typename Rng >
bool none_of(Rng&& rng) noexcept {
	return !any_of( std::forward<Rng>(rng) );
}

template< typename Rng, typename Pred >
bool none_of(Rng&& rng, Pred&& pred) noexcept {
	return !any_of( std::forward<Rng>(rng), std::forward<Pred>(pred) );
}

// pair is in same order as if minmax_element( ..., operator<( bool, bool ) ) would have been used.
template< typename Rng >
std::pair<bool,bool> all_any_of( Rng const& rng ) noexcept {
	std::pair<bool,bool> pairb(true,false);
	tc::for_each(rng, [&](bool b) {
		pairb.first=pairb.first && b;
		pairb.second=pairb.second || b;
		return continue_if( pairb.first || !pairb.second );
	} );
	return pairb;
}

template< typename Rng, typename Pred >
std::pair<bool,bool> all_any_of(Rng const& rng, Pred&& pred) noexcept {
	return all_any_of( tc::transform(rng,std::forward<Pred>(pred)) );
}

inline bool eager_or(std::initializer_list<tc::bool_context> ab) noexcept {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::any_of(ab);
}

inline bool eager_and(std::initializer_list<tc::bool_context> ab) noexcept {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::all_of(ab);
}

}
