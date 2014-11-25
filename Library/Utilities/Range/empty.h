#pragma once

#include <utility>
#include "../trivial_functors.h"

#include "break_or_continue.h"
#include "range_defines.h"
#include "container_traits.h"

#include <functional>

#include "index_range.h"

namespace RANGE_PROPOSAL_NAMESPACE {
	template<typename Rng>
	typename std::enable_if< has_mem_fn_empty<typename std::decay<Rng>::type>::value,
	bool >::type empty(Rng const& rng) {
		return rng.empty();
	}

	template<typename Rng>
	typename std::enable_if< !has_mem_fn_empty<typename std::decay<Rng>::type>::value && is_range_with_iterators<typename std::decay<Rng>::type>::value,
	bool >::type empty(Rng const& rng) {
		auto const& rngidx = ensure_index_range(rng);
		return rngidx.at_end_index(rngidx.begin_index());
	}

	template<typename Rng>
	typename std::enable_if< !has_mem_fn_empty<typename std::decay<Rng>::type>::value && !is_range_with_iterators<typename std::decay<Rng>::type>::value,
	bool >::type empty(Rng const& rng) {
		return continue_==for_each( rng, MAKE_CONSTEXPR_FUNCTION(break_) );
	}
}
