
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/invoke.h"
#include "../range/range_fwd.h"
#include "../range/transform_adaptor.h"
#include "for_each.h"
#include "find.h"
#include "compare.h"
#include "any_accu.h"
#include <utility>

/////////////////////////////////////////////
// std::all/any/none_of on ranges

namespace tc {

template< typename Rng, typename Pred = tc::identity >
[[nodiscard]] constexpr bool any_of(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
	return tc::find_first_if<tc::return_bool>(std::forward<Rng>(rng), std::forward<Pred>(pred));
}

template< typename Rng, typename Pred = tc::identity >
[[nodiscard]] constexpr bool all_of(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
	return !tc::any_of(std::forward<Rng>(rng), std::not_fn(std::forward<Pred>(pred)));
}

// pair is in same order as if minmax_element( ..., operator<( bool, bool ) ) would have been used.
template< typename Rng >
[[nodiscard]] std::pair<bool,bool> all_any_of( Rng const& rng ) MAYTHROW {
	std::pair<bool,bool> pairb(true,false);
	tc::for_each(rng, [&](bool b) noexcept {
		pairb.first=pairb.first && b;
		pairb.second=pairb.second || b;
		return continue_if( pairb.first || !pairb.second );
	} );
	return pairb;
}

template< typename Rng, typename Pred >
[[nodiscard]] std::pair<bool,bool> all_any_of(Rng const& rng, Pred&& pred) MAYTHROW {
	return all_any_of( tc::transform(rng,std::forward<Pred>(pred)) );
}

[[nodiscard]] inline bool eager_or(std::initializer_list<tc::bool_context> ab) MAYTHROW {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::any_of(ab);
}

[[nodiscard]] inline bool eager_and(std::initializer_list<tc::bool_context> ab) MAYTHROW {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::all_of(ab);
}

template< typename Rng, typename Pred >
[[nodiscard]] bool eager_any_of(Rng&& rng, Pred pred) MAYTHROW {
	tc::any_accu any;
	tc::for_each(rng, [&](auto&& t) noexcept {
		any(tc::invoke(pred, tc_move_if_owned(t)));
	});
	return any;
}

template< typename Rng, typename Pred >
[[nodiscard]] bool eager_all_of(Rng&& rng, Pred&& pred) MAYTHROW {
	return !tc::eager_any_of(std::forward<Rng>(rng), std::not_fn(std::forward<Pred>(pred)));
}

}
