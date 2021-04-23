
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "subrange.h"
#include "partition_iterator.h"

namespace tc {

	////////////////////////////////////////////////////////////////////////
	// Range functions

	namespace range {
		template<typename RangeReturn, typename Rng, typename UnaryPredicate>
		[[nodiscard]] decltype(auto) partition_point(Rng&& rng, UnaryPredicate&& pred) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return RangeReturn::pack_border(
				iterator::partition_point(tc::begin(rng), tc::end(rng), std::forward<UnaryPredicate>(pred)),
				std::forward<Rng>(rng)
			);
		}

		template<typename RangeReturn, typename Rng, typename Value>
		[[nodiscard]] decltype(auto) lower_bound(Rng&& rng, Value const& val) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return RangeReturn::pack_border(
				iterator::lower_bound(tc::begin(rng), tc::end(rng), val),
				std::forward<Rng>(rng)
			);
		}

		template<typename RangeReturn, typename Rng, typename Value, typename SortPredicate>
		[[nodiscard]] decltype(auto) lower_bound(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return RangeReturn::pack_border(
				iterator::lower_bound(tc::begin(rng), tc::end(rng), val, std::forward<SortPredicate>(pred)),
				std::forward<Rng>(rng)
			);
		}

		template<typename RangeReturn, typename Rng, typename Value>
		[[nodiscard]] decltype(auto) upper_bound(Rng&& rng, Value const& val) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return RangeReturn::pack_border(
				iterator::upper_bound(tc::begin(rng), tc::end(rng), val),
				std::forward<Rng>(rng)
			);
		}

		template<typename RangeReturn, typename Rng, typename Value, typename SortPredicate>
		[[nodiscard]] decltype(auto) upper_bound(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			static_assert( RangeReturn::allowed_if_always_has_border );
			return RangeReturn::pack_border(
				iterator::upper_bound(tc::begin(rng), tc::end(rng), val, std::forward<SortPredicate>(pred)),
				std::forward<Rng>(rng)
			);
		}

		template<typename Rng, typename Value>
		[[nodiscard]] decltype(auto) equal_range(Rng&& rng, Value const& val) noexcept {
			auto pairit = iterator::equal_range(tc::begin(rng), tc::end(rng), val);
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}

		template<typename Rng, typename Value, typename SortPredicate>
		[[nodiscard]] decltype(auto) equal_range(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			auto pairit = iterator::equal_range(tc::begin(rng), tc::end(rng), val, std::forward<SortPredicate>(pred));
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}
	}
	using namespace range;
}

