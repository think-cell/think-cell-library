
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "partition_iterator.h"
#include "../range/subrange.h"

namespace tc {

	////////////////////////////////////////////////////////////////////////
	// Range functions

	namespace range {
		template<typename RangeReturn, typename Rng, typename UnaryPredicate>
		[[nodiscard]] constexpr decltype(auto) partition_point(Rng&& rng, UnaryPredicate&& pred) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return tc_rewrap_temporary(Rng&&, RangeReturn::pack_border(
				iterator::partition_point(tc::begin(tc_unwrap_temporary(rng)), tc::end(tc_unwrap_temporary(rng)), tc_move_if_owned(pred)),
				tc_unwrap_temporary(tc_move_if_owned(rng))
			));
		}

		template<typename RangeReturn, typename Rng, typename Value, typename SortPredicate = tc::fn_less>
		[[nodiscard]] decltype(auto) lower_bound(Rng&& rng, Value const& val, SortPredicate&& pred = {}) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return tc_rewrap_temporary(Rng&&, RangeReturn::pack_border(
				iterator::lower_bound(tc::begin(tc_unwrap_temporary(rng)), tc::end(tc_unwrap_temporary(rng)), val, tc_move_if_owned(pred)),
				tc_unwrap_temporary(tc_move_if_owned(rng))
			));
		}

		template<typename RangeReturn, typename Rng, typename Value, typename SortPredicate = tc::fn_less>
		[[nodiscard]] decltype(auto) upper_bound(Rng&& rng, Value const& val, SortPredicate&& pred = {}) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return tc_rewrap_temporary(Rng&&, RangeReturn::pack_border(
				iterator::upper_bound(tc::begin(tc_unwrap_temporary(rng)), tc::end(tc_unwrap_temporary(rng)), val, tc_move_if_owned(pred)),
				tc_unwrap_temporary(tc_move_if_owned(rng))
			));
		}

		template<typename Rng, typename Value, typename SortPredicate = tc::fn_less>
		[[nodiscard]] decltype(auto) equal_range(Rng&& rng, Value const& val, SortPredicate&& pred = {}) noexcept {
			auto pairit = iterator::equal_range(tc::begin(rng), tc::end(rng), val, tc_move_if_owned(pred));
			return tc::slice( tc_move_if_owned(rng), tc_move(pairit).first, tc_move(pairit).second );
		}

		template<typename Rng, typename Value, typename SortPredicate = tc::fn_less>
		[[nodiscard]] decltype(auto) range_in_interval_inclusive(Rng&& rng, tc::interval<Value> const& intvlval, SortPredicate&& pred = {}) noexcept {
			return tc_invoke(tc::chained(
				// clang crashes if we use return_decltype_allow_xvalue
				[&](auto&& rng) noexcept -> decltype(auto) { return upper_bound<tc::return_take>(tc_move_if_owned(rng), intvlval[tc::hi], tc_move_if_owned(pred)); },
				[&](auto&& rng) noexcept -> decltype(auto) { return lower_bound<tc::return_drop>(tc_move_if_owned(rng), intvlval[tc::lo], pred); }
			), tc_move_if_owned(rng));
		}

		template<typename Rng, typename Value, typename SortPredicate = tc::fn_less>
		[[nodiscard]] decltype(auto) range_in_interval(Rng&& rng, tc::interval<Value> const& intvlval, SortPredicate&& pred = {}) noexcept {
			return tc_invoke(tc::chained(
				// clang crashes if we use return_decltype_allow_xvalue
				[&](auto&& rng) noexcept -> decltype(auto) { return lower_bound<tc::return_take>(tc_move_if_owned(rng), intvlval[tc::hi], tc_move_if_owned(pred)); },
				[&](auto&& rng) noexcept -> decltype(auto) { return upper_bound<tc::return_drop>(tc_move_if_owned(rng), intvlval[tc::lo], pred); }
			), tc_move_if_owned(rng));
		}
	}
	using namespace range;
}

