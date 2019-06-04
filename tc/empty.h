
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "for_each.h"
#include "trivial_functors.h"

namespace tc {
	template<typename Rng, std::enable_if_t<
		has_mem_fn_empty<Rng const>::value
		&& is_range_with_iterators<Rng const>::value
	>* = nullptr>
	bool empty(Rng const& rng) noexcept {
		return rng.empty();
	}

	template<typename Rng, std::enable_if_t<
		!has_mem_fn_empty<Rng const>::value
		&& is_range_with_iterators<Rng const>::value
		&& !has_index<Rng>::value
	>* = nullptr>
	bool empty(Rng const& rng) noexcept {
		return tc::begin(rng)==tc::end(rng);
	}

	template<typename Rng, std::enable_if_t<
		!has_mem_fn_empty<Rng const>::value
		&& is_range_with_iterators<Rng const>::value
		&& has_index<Rng>::value
	>* = nullptr>
	bool empty(Rng const& rngidx) noexcept {
		return rngidx.at_end_index(rngidx.begin_index());
	}

	template<typename Rng, std::enable_if_t<
		!has_mem_fn_empty<Rng const>::value
		&& !is_range_with_iterators<Rng const>::value
	>* = nullptr>
	bool empty(Rng const& rng) noexcept {
		return continue_==tc::for_each( rng, MAKE_CONSTEXPR_FUNCTION(INTEGRAL_CONSTANT(tc::break_)()) );
	}

	DEFINE_FN2(tc::empty, fn_empty) // Prevent ADL of std::empty (C++17)
}
