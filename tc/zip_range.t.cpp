
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"
#include "zip_range.h"

namespace
{
	template<typename BaseRange, typename Traversal>
	struct traversal_override_adaptor : tc::range_adaptor<traversal_override_adaptor<BaseRange, Traversal>, BaseRange &, Traversal> {
		using base_ = tc::range_adaptor<traversal_override_adaptor<BaseRange, Traversal>, BaseRange &, Traversal>;
		traversal_override_adaptor(BaseRange & base_range) : base_(tc::aggregate_tag, base_range) {}
	};
}

UNITTESTDEF(zip_range_supported_ops) {
	std::vector<int> data = { 1, 2, 3, 4, 5 };
	auto data_fwd = traversal_override_adaptor<std::vector<int>, boost::iterators::forward_traversal_tag>(data);
	auto data_bidir = traversal_override_adaptor<std::vector<int>, boost::iterators::bidirectional_traversal_tag>(data);

	auto zip_ff = tc::zip(data_fwd, data_fwd);
	auto zip_fb = tc::zip(data_fwd, data_bidir);
	auto zip_fr = tc::zip(data_fwd, data);
	auto zip_bb = tc::zip(data_bidir, data_bidir);
	auto zip_br = tc::zip(data_bidir, data);
	auto zip_rr = tc::zip(data, data);

	tc::for_each(zip_ff, tc::noop());
	tc::for_each(zip_fb, tc::noop());
	tc::for_each(zip_fr, tc::noop());
	tc::for_each(zip_bb, tc::noop());
	tc::for_each(zip_br, tc::noop());
	tc::for_each(zip_rr, tc::noop());

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
}