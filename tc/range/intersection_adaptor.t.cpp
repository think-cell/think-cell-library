// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "intersection_adaptor.h"

UNITTESTDEF(intersection_difference) {
	int rngn[] = {2,3,5,7};
	double rngf[] = {1,3,5};
	auto rngnDifference = tc::difference(rngn, rngf);
	auto rngnIntersection = tc::intersect(rngn, rngf);
	STATICASSERTSAME(tc::range_value_t<decltype(rngnDifference)>, int);
	STATICASSERTSAME(tc::range_value_t<decltype(rngnIntersection)>, int);
	_ASSERT(tc::equal(rngnDifference, tc::make_array(tc::aggregate_tag, 2, 7)));
	_ASSERT(tc::equal(rngnIntersection, tc::make_array(tc::aggregate_tag, 3, 5)));

	auto rngndiff2 = tc::difference(
		tc::generator_range_output<int>([](auto sink) noexcept {
			if (tc::break_ == sink(1)) return tc::break_;
			if (tc::break_ == sink(2)) return tc::break_;
			if (tc::break_ == sink(3)) return tc::break_;
			return tc::continue_;
		}),
		tc::single(2)
	);

	_ASSERT(tc::equal(rngndiff2, tc::make_array(tc::aggregate_tag,1,3)));

	auto rngndiff3 = tc::difference(
		tc::make_array(tc::aggregate_tag,0,1,3,5),
		tc::generator_range_output<int>([](auto sink) noexcept {
			if (tc::break_ == sink(1)) return tc::break_;
			if (tc::break_ == sink(2)) return tc::break_;
			if (tc::break_ == sink(3)) return tc::break_;
			return tc::continue_;
		})
	);

	tc::for_each(rngndiff3, [](auto n) noexcept {
	});
	_ASSERT(tc::equal(rngndiff3, tc::make_array(tc::aggregate_tag,0,5)));


}
