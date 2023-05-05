
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../algorithm/compare.h"
#include "../algorithm/algorithm.h"
#include "range_fwd.h"
#include "range_adaptor.h"

namespace tc {
	template<typename Rng0, typename Rng1, typename Less = tc::fn_less>
	[[nodiscard]] auto merge_range(Rng0&& rng0, Rng1&& rng1, Less&& less = Less()) noexcept {
		return [
			rng0 = tc::make_reference_or_value(std::forward<Rng0>(rng0)),
			rng1 = tc::make_reference_or_value(std::forward<Rng1>(rng1)),
			less = std::forward<Less>(less)
		](auto&& sink) MAYTHROW {
			return tc::interleave_2(
				*rng0,
				*rng1,
				[&](auto const& lhs, auto const& rhs) noexcept {
					return less(rhs, lhs) ? std::weak_ordering::greater : std::weak_ordering::less;
				},
				sink,
				sink,
				tc::never_called()
			);
		};
	}
}
