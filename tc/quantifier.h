
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_fwd.h"
#include "transform_adaptor.h"
#include "break_or_continue.h"
#include "for_each.h"
#include "find.h"
#include <utility>

/////////////////////////////////////////////
// std::all/any/none_of on ranges

namespace tc {

DEFINE_FN(bool_cast)

template< typename Rng, typename Pred >
bool any_of(Rng&& rng, Pred&& pred) MAYTHROW {
	return tc::find_first_if<tc::return_bool>(std::forward<Rng>(rng), std::forward<Pred>(pred));
}

template< typename Rng >
bool any_of(Rng&& rng) MAYTHROW {
	return tc::any_of(std::forward<Rng>(rng), tc::fn_bool_cast());
}

template< typename Rng, typename Pred >
bool all_of(Rng&& rng, Pred&& pred) MAYTHROW {
	return !tc::any_of(std::forward<Rng>(rng), tc::not_fn(std::forward<Pred>(pred)));
}

template< typename Rng >
bool all_of(Rng&& rng) MAYTHROW {
	return tc::all_of(std::forward<Rng>(rng), tc::fn_bool_cast());
}

// pair is in same order as if minmax_element( ..., operator<( bool, bool ) ) would have been used.
template< typename Rng >
std::pair<bool,bool> all_any_of( Rng const& rng ) MAYTHROW {
	std::pair<bool,bool> pairb(true,false);
	tc::for_each(rng, [&](bool b) noexcept {
		pairb.first=pairb.first && b;
		pairb.second=pairb.second || b;
		return continue_if( pairb.first || !pairb.second );
	} );
	return pairb;
}

template< typename Rng, typename Pred >
std::pair<bool,bool> all_any_of(Rng const& rng, Pred&& pred) MAYTHROW {
	return all_any_of( tc::transform(rng,std::forward<Pred>(pred)) );
}

inline bool eager_or(std::initializer_list<tc::bool_context> ab) MAYTHROW {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::any_of(ab);
}

inline bool eager_and(std::initializer_list<tc::bool_context> ab) MAYTHROW {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::all_of(ab);
}

}
