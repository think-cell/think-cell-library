
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../algorithm/algorithm.h"
#include "../algorithm/sort_streaming.h"

namespace tc {
	template<typename RngRng, typename Less = tc::fn_less>
	auto merge_many(RngRng&& rngrng, Less&& less = Less()) noexcept {
		return tc::transform(
			tc::sort_streaming(
				tc::filter(
					tc::transform(
						tc_move_if_owned(rngrng),
						tc_fn(tc::all)
					),
					tc_fn(!tc::empty)
				),
				tc::projected(tc_move_if_owned(less), tc::fn_front()),
				[](auto& rng) noexcept {
					tc::drop_first_inplace(rng);
					return !tc::empty(rng);
				}
			),
			tc::fn_front()
		);
	}

	template<typename RngRng, typename Less = tc::fn_less>
	auto merge_many_unique(RngRng&& rngrng, Less&& less = Less()) noexcept {
		return tc::ordered_unique(tc::merge_many(tc_move_if_owned(rngrng), less), less);
	}
}
