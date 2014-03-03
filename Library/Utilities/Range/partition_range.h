#pragma once

#include "range_defines.h"
#include "sub_range.h"
#include "partition_iterator.h"

#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm/mismatch.hpp>
#include <boost/algorithm/string/compare.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {

	////////////////////////////////////////////////////////////////////////
	// Range functions

	namespace range {
		template< range_return_value re, typename Rng,
				  class UnaryPredicate >
		typename range_return<Rng,re>::type
		partition_point(Rng&& rng, UnaryPredicate && pred) {
			static_assert( re!=return_index_or_npos, "return_index_or_npos maps end() to -1, which is probably not what you want." );
			return range_return<Rng,re>::
				pack(iterator::partition_point(boost::begin(rng), boost::end(rng), std::forward<UnaryPredicate>(pred)), std::forward<Rng>(rng));
		}

		template< range_return_value re, typename Rng, typename Value >
		typename range_return<Rng,re>::type
		lower_bound( Rng&& rng, Value const& val ) {
			static_assert( re!=return_index_or_npos, "return_index_or_npos maps end() to -1, which is probably not what you want." );
			return range_return<Rng,re>::
				pack(iterator::lower_bound(boost::begin(rng), boost::end(rng), val),
					 std::forward<Rng>(rng));
		}

		template< range_return_value re, typename Rng, typename Value, typename SortPredicate >
		typename range_return<Rng,re>::type
		lower_bound( Rng&& rng, Value const& val, SortPredicate && pred ) {
			static_assert( re!=return_index_or_npos, "return_index_or_npos maps end() to -1, which is probably not what you want." );
			return range_return<Rng,re>::
				pack(iterator::lower_bound(boost::begin(rng), boost::end(rng), val, std::forward<SortPredicate>(pred)),
					 std::forward<Rng>(rng));
		}

		template< range_return_value re, typename Rng, typename Value >
		typename range_return<Rng,re>::type
		upper_bound( Rng&& rng, Value const& val ) {
			static_assert( re!=return_index_or_npos, "return_index_or_npos maps end() to -1, which is probably not what you want." );
			return range_return<Rng,re>::
				pack(iterator::upper_bound(boost::begin(rng), boost::end(rng), val),
					 std::forward<Rng>(rng));
		}

		template< range_return_value re, typename Rng, typename Value, typename SortPredicate >
		typename range_return<Rng,re>::type
		upper_bound( Rng&& rng, Value const& val, SortPredicate && pred ) {
			static_assert( re!=return_index_or_npos, "return_index_or_npos maps end() to -1, which is probably not what you want." );
			return range_return<Rng,re>::
				pack(iterator::upper_bound(boost::begin(rng), boost::end(rng), val, std::forward<SortPredicate>(pred)),
					 std::forward<Rng>(rng));
		}

		template<typename Rng, typename Value>
		bool binary_search(const Rng& rng, Value const& val) {
			return iterator::binary_search(boost::begin(rng), boost::end(rng), val);
		}

		template<typename Rng, typename Value>
		typename make_sub_range_result<Rng>::type
		equal_range(Rng&& rng, Value const& val) {
			std::pair<
				typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type,
				typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type
			> pairit=iterator::equal_range(boost::begin(rng), boost::end(rng), val);
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}

		template<typename Rng, typename Value, typename SortPredicate>
		typename make_sub_range_result<Rng>::type
		equal_range(Rng&& rng, Value const& val, SortPredicate && pred) {
			std::pair<
				typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type,
				typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type
			> pairit=iterator::equal_range(boost::begin(rng), boost::end(rng), val, std::forward<SortPredicate>(pred));
			return tc::slice( std::forward<Rng>(rng), tc_move(pairit.first), tc_move(pairit.second) );
		}
	}
	using namespace range;
}

