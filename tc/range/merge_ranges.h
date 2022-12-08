
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
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
						std::forward<RngRng>(rngrng),
						TC_FN(tc::make_view)
					),
					TC_FN(!tc::empty)
				),
				tc::projected(std::forward<Less>(less), tc::fn_front()),
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
		return tc::ordered_unique(tc::merge_many(std::forward<RngRng>(rngrng), less), less);
	}
}
