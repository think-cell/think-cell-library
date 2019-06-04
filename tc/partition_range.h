
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "sub_range.h"
#include "partition_iterator.h"

namespace tc {

	////////////////////////////////////////////////////////////////////////
	// Range functions

	namespace range {
		template< template<typename> class RangeReturn, typename Rng,
				typename UnaryPredicate >
		typename RangeReturn<Rng>::type
		partition_point(Rng&& rng, UnaryPredicate&& pred) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::partition_point(tc::begin(rng), tc::end(rng), std::forward<UnaryPredicate>(pred)), std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Value >
		typename RangeReturn<Rng>::type
		lower_bound(Rng&& rng, Value const& val) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::lower_bound(tc::begin(rng), tc::end(rng), val),
					 std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Value, typename SortPredicate >
		typename RangeReturn<Rng>::type
		lower_bound(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::lower_bound(tc::begin(rng), tc::end(rng), val, std::forward<SortPredicate>(pred)),
					 std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Value >
		auto upper_bound(Rng&& rng, Value const& val) noexcept return_decltype_xvalue_by_ref(
			RangeReturn<Rng>::
				pack_border(iterator::upper_bound(tc::begin(rng), tc::end(rng), val),
					 std::forward<Rng>(rng))
		)

		template< template<typename> class RangeReturn, typename Rng, typename Value, typename SortPredicate >
		typename RangeReturn<Rng>::type
		upper_bound(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::upper_bound(tc::begin(rng), tc::end(rng), val, std::forward<SortPredicate>(pred)),
					 std::forward<Rng>(rng));
		}

		template<typename Rng, typename Value>
		tc::make_sub_range_result_t<Rng>
		equal_range(Rng&& rng, Value const& val) noexcept {
			std::pair<
				typename boost::range_iterator<Rng>::type,
				typename boost::range_iterator<Rng>::type
			> pairit=iterator::equal_range(tc::begin(rng), tc::end(rng), val);
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}

		template<typename Rng, typename Value, typename SortPredicate>
		tc::make_sub_range_result_t<Rng>
		equal_range(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			std::pair<
				typename boost::range_iterator<Rng>::type,
				typename boost::range_iterator<Rng>::type
			> pairit=iterator::equal_range(tc::begin(rng), tc::end(rng), val, std::forward<SortPredicate>(pred));
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}
	}
	using namespace range;
}

