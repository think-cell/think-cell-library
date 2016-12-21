//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"
#include "sub_range.h"
#include "partition_iterator.h"

#include <boost/range/algorithm/mismatch.hpp>
#include <boost/algorithm/string/compare.hpp>

namespace tc {

	////////////////////////////////////////////////////////////////////////
	// Range functions

	namespace range {
		template< template<typename> class RangeReturn, typename Rng,
				typename UnaryPredicate >
		typename RangeReturn<Rng>::type
		partition_point(Rng&& rng, UnaryPredicate&& pred) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::partition_point(boost::begin(rng), boost::end(rng), std::forward<UnaryPredicate>(pred)), std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Value >
		typename RangeReturn<Rng>::type
		lower_bound(Rng&& rng, Value const& val) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::lower_bound(boost::begin(rng), boost::end(rng), val),
					 std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Value, typename SortPredicate >
		typename RangeReturn<Rng>::type
		lower_bound(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::lower_bound(boost::begin(rng), boost::end(rng), val, std::forward<SortPredicate>(pred)),
					 std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Value >
		typename RangeReturn<Rng>::type
		upper_bound(Rng&& rng, Value const& val) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::upper_bound(boost::begin(rng), boost::end(rng), val),
					 std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Value, typename SortPredicate >
		typename RangeReturn<Rng>::type
		upper_bound(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			return RangeReturn<Rng>::
				pack_border(iterator::upper_bound(boost::begin(rng), boost::end(rng), val, std::forward<SortPredicate>(pred)),
					 std::forward<Rng>(rng));
		}

		template<typename Rng, typename Value>
		typename make_sub_range_result<Rng>::type
		equal_range(Rng&& rng, Value const& val) noexcept {
			std::pair<
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type,
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type
			> pairit=iterator::equal_range(boost::begin(rng), boost::end(rng), val);
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}

		template<typename Rng, typename Value, typename SortPredicate>
		typename make_sub_range_result<Rng>::type
		equal_range(Rng&& rng, Value const& val, SortPredicate&& pred) noexcept {
			std::pair<
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type,
				typename boost::range_iterator< std::remove_reference_t<Rng> >::type
			> pairit=iterator::equal_range(boost::begin(rng), boost::end(rng), val, std::forward<SortPredicate>(pred));
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}
	}
	using namespace range;
}

