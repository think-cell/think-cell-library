#pragma once

#include <utility>
#include "trivial_functors.h"

#include "break_or_continue.h"
#include "range_defines.h"
#include "container_traits.h"

#include <functional>

#include "index_range.h"

namespace RANGE_PROPOSAL_NAMESPACE {
	template<typename Rng>
	typename std::enable_if<
		has_mem_fn_empty<std::decay_t<Rng>>::value,
	bool >::type empty(Rng const& rng) {
		return rng.empty();
	}

	template<typename Rng>
	typename std::enable_if<
		!has_mem_fn_empty<std::decay_t<Rng>>::value &&
		is_range_with_iterators<Rng>::value &&
		!has_index<std::decay_t<Rng>>::value,
	bool >::type empty(Rng const& rng) {
		return boost::begin(rng)==boost::end(rng);
	}

	template<typename Rng>
	typename std::enable_if<
		!has_mem_fn_empty<std::decay_t<Rng>>::value &&
		is_range_with_iterators<Rng>::value &&
		has_index<std::decay_t<Rng>>::value,
	bool >::type empty(Rng const& rngidx) {
		return rngidx.at_end_index(rngidx.begin_index());
	}

	template<typename Rng>
	typename std::enable_if<
		!has_mem_fn_empty<std::decay_t<Rng>>::value &&
		!is_range_with_iterators<Rng>::value,
	bool >::type empty(Rng const& rng) {
		return continue_==tc::for_each( rng, MAKE_CONSTEXPR_FUNCTION(break_) );
	}

	DEFINE_FN2(tc::empty, fn_empty) // Prevent ADL of std::empty (C++17)
}
