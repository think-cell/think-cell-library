
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "for_each.h"
#include "trivial_functors.h"

namespace tc {
	template<typename Rng>
	[[nodiscard]] constexpr bool empty(Rng const& rng) noexcept {
		if constexpr( has_constexpr_size<Rng>::value ) {
			return 0 == constexpr_size<Rng>::value;
		} else if constexpr( has_mem_fn_empty<Rng const>::value ) {
			return rng.empty();
		} else if constexpr( has_index<Rng>::value ) {
			return rng.at_end_index(rng.begin_index());
		} else if constexpr( is_range_with_iterators<Rng const>::value ) {
			return tc::begin(rng) == tc::end(rng);
		} else if constexpr( has_size<Rng const>::value ) {
			return 0 == tc::size(rng);
		} else {
			return continue_==tc::for_each( rng, MAKE_CONSTEXPR_FUNCTION(INTEGRAL_CONSTANT(tc::break_)()) );
		}
	}
}
