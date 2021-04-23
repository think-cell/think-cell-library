
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"
#include "zip_range.h"
#include "interval.h" // for tc::lohi

namespace
{
	template<typename BaseRange, typename Traversal>
	struct traversal_override_adaptor : tc::index_range_adaptor<traversal_override_adaptor<BaseRange, Traversal>, BaseRange &, tc::range_adaptor_base_range<BaseRange>, Traversal> {
		traversal_override_adaptor(BaseRange & base_range) : traversal_override_adaptor::index_range_adaptor(tc::aggregate_tag, base_range) {}
	};
}

UNITTESTDEF(zip_range_supported_ops) {
	std::vector<int> data = { 1, 2, 3, 4, 5 };
	std::vector<float> dataf = { 1, 2, 3, 4, 5 };
	auto data_fwd = traversal_override_adaptor<tc::vector<int>, boost::iterators::forward_traversal_tag>(data);
	auto data_bidir = traversal_override_adaptor<tc::vector<int>, boost::iterators::bidirectional_traversal_tag>(data);
	auto data_gen = [&](auto sink) noexcept { return tc::for_each(data, sink); };

	auto zip_ff = tc::zip(data_fwd, data_fwd);
	auto zip_fb = tc::zip(data_fwd, data_bidir);
	auto zip_fr = tc::zip(data_fwd, data);
	auto zip_bb = tc::zip(data_bidir, data_bidir);
	auto zip_br = tc::zip(data_bidir, data);
	auto zip_rr = tc::zip(data, data);
	auto zip_rfgf = tc::zip(dataf, data_fwd, data_gen, data_fwd);

	tc::for_each(zip_ff, tc::noop());
	tc::for_each(zip_fb, tc::noop());
	tc::for_each(zip_fr, tc::noop());
	tc::for_each(zip_bb, tc::noop());
	tc::for_each(zip_br, tc::noop());
	tc::for_each(zip_rr, tc::noop());
	tc::for_each(zip_rfgf, [](float&,int&,int&,int&) noexcept{});

	static_assert(!tc::has_decrement_index<decltype(zip_ff)>::value);
	static_assert(!tc::has_decrement_index<decltype(zip_fb)>::value);
	static_assert(!tc::has_decrement_index<decltype(zip_fr)>::value);
	static_assert(!tc::has_advance_index<decltype(zip_ff)>::value);
	static_assert(!tc::has_advance_index<decltype(zip_fb)>::value);
	static_assert(!tc::has_advance_index<decltype(zip_fr)>::value);
	static_assert(!tc::has_distance_to_index<decltype(zip_ff)>::value);
	static_assert(!tc::has_distance_to_index<decltype(zip_fb)>::value);
	static_assert(!tc::has_distance_to_index<decltype(zip_fr)>::value);

	static_assert(tc::has_decrement_index<decltype(zip_bb)>::value);
	static_assert(tc::has_decrement_index<decltype(zip_br)>::value);
	static_assert(!tc::has_advance_index<decltype(zip_bb)>::value);
	static_assert(!tc::has_advance_index<decltype(zip_br)>::value);
	static_assert(!tc::has_distance_to_index<decltype(zip_bb)>::value);
	static_assert(!tc::has_distance_to_index<decltype(zip_br)>::value);

	static_assert(tc::has_decrement_index<decltype(zip_rr)>::value);
	static_assert(tc::has_advance_index<decltype(zip_rr)>::value);
	static_assert(tc::has_distance_to_index<decltype(zip_rr)>::value);

	static_assert(!tc::is_range_with_iterators<decltype(zip_rfgf)>::value);
}

UNITTESTDEF(zip_range_difference_type) {
	auto rngBigDiff = tc::iota(0, 2);
	auto rngSmallDiff = tc::all_values<tc::lohi>();

	using BigDiffType = boost::range_difference<decltype(rngBigDiff)>::type;
	using SmallDiffType = boost::range_difference<decltype(rngSmallDiff)>::type;
	// e.g. BigDiffType=int, SmallDiffType=signed char

	// If these initial assertions fail, the test needs to be adjusted. Find two ranges with different difference_types
	static_assert(!std::is_same<BigDiffType, SmallDiffType>::value);
	static_assert(sizeof(BigDiffType) > sizeof(SmallDiffType));

	static_assert(std::is_same<boost::range_difference<decltype(tc::zip(rngBigDiff, rngBigDiff))>::type, BigDiffType>::value);
	static_assert(std::is_same<boost::range_difference<decltype(tc::zip(rngBigDiff, rngSmallDiff))>::type, SmallDiffType>::value);
	static_assert(std::is_same<boost::range_difference<decltype(tc::zip(rngSmallDiff, rngSmallDiff))>::type, SmallDiffType>::value);
}
