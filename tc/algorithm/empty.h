
// think-cell public library
//
// Copyright (C) think-cell Software GmbH

// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/trivial_functors.h"
#include "for_each.h"

namespace tc {
	namespace empty_internal {
		// TODO: inline when clang supports lambdas in unevaluated contexts
		template<typename Rng>
		constexpr auto empty(Rng&& rng) noexcept {
			if constexpr(has_constexpr_size<Rng>) {
				return [&]() return_decltype_noexcept(0==constexpr_size<Rng>());
			} else if constexpr(has_mem_fn_empty<Rng>) {
				return [&]() return_MAYTHROW(tc_move_if_owned(rng).empty());
			} else if constexpr(tc::range_with_iterators<Rng>) {
				return [&]() return_MAYTHROW(tc::at_end_index(rng, tc::begin_index(rng)));
			} else if constexpr(has_size<Rng>) {
				return [&]() return_MAYTHROW(0==tc::size(tc_move_if_owned(rng))); // tc::size on files may throw
			} else {
				return [&]() return_MAYTHROW(tc::continue_==tc::for_each(tc_move_if_owned(rng), tc::constexpr_function<tc::constant<tc::break_>{}>()));
			}
		}
	}

	template<typename Rng>
	[[nodiscard]] constexpr bool empty(Rng&& rng) return_MAYTHROW(
		empty_internal::empty(tc_move_if_owned(rng))()
	)
}
