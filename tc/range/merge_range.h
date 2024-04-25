
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "union_adaptor.h"

namespace tc {
	template<typename Rng0, typename Rng1, typename Less = tc::fn_less>
	[[nodiscard]] auto merge_range(Rng0&& rng0, Rng1&& rng1, Less&& less = Less()) noexcept {
		return tc::disjoint_union_range(
			tc_move_if_owned(rng0),
			tc_move_if_owned(rng1),
			[less=tc::decay_copy(tc_move_if_owned(less))](auto const& lhs, auto const& rhs) noexcept {
				return less(rhs, lhs) ? std::weak_ordering::greater : std::weak_ordering::less;
			}
		);
	}
}
