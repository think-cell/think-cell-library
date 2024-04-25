
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../algorithm/algorithm.h"
#include "reverse_adaptor.h"

#include <list>

UNITTESTDEF(tc_reverse_random_access) {
	TEST_RANGE_EQUAL(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3)), tc::reverse(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 3,2,1))));

	// test iterators:
	auto rng = tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2,3));
	auto revrng = tc::reverse(rng);
	auto it = tc::begin(revrng);
	_ASSERTEQUAL(*it++, 3);
	_ASSERTEQUAL(*it++, 2);
	_ASSERTEQUAL(*it++, 1);
	_ASSERTEQUAL(tc::end(revrng), it);

	--it;
	_ASSERTEQUAL(*it,1);

	// Test random access
	it -= 2;
	_ASSERTEQUAL(*it,3);

	it += 3;
	_ASSERTEQUAL(tc::end(revrng), it);
}

#include <numeric>

UNITTESTDEF(tc_reverse_binary_search) {
	tc::vector<int> vecn(64);
	std::iota(tc::begin(vecn), tc::end(vecn), 0);

	// random access:
	_ASSERT(tc::binary_find_unique<tc::return_bool>(tc::reverse(vecn),32, tc::fn_greater()));

	// bidirectional:
	_ASSERT(
		tc::binary_find_unique<tc::return_bool>(
			tc::reverse(
				tc::filter(vecn,[](auto) noexcept {return true;})
			),
			32,
			tc::fn_greater()
		)
	);
}

UNITTESTDEF(tc_reverse_bidirectional) {
	std::list<int> rng{1,2,3};
	auto rngrev = tc::reverse(rng);
	auto it = tc::begin(rngrev);
	_ASSERTEQUAL(*it++, 3);
	_ASSERTEQUAL(*it++, 2);
	_ASSERTEQUAL(*it++, 1);
	_ASSERTEQUAL(tc::end(rngrev), it);

	TEST_RANGE_EQUAL(tc::reverse(rng), tc_as_constexpr(tc::make_array(tc::aggregate_tag, 3,2,1)));
	TEST_RANGE_EQUAL(tc::reverse(tc::reverse(rng)), rng);
}

UNITTESTDEF(tc_reverse_base_bound) {
	std::list<int> rng{1,2,3,4};
	auto rngreverse = tc::reverse(rng);
	auto it = tc_modified(tc_modified(tc::begin(rngreverse), ++_), ++_);
	TEST_RANGE_EQUAL(
		tc::reverse(tc::take(rngreverse,it)),
		tc::drop(rng, it.border_base())
	);

	_ASSERTEQUAL(*it,*it.element_base());
}

UNITTESTDEF(tc_reverse_drop_first_last_inplace) {
	tc::vector<int> vecn{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	auto&& rngnReverse = tc::reverse(tc::all(vecn));

	tc::drop_first_inplace(rngnReverse, 0);
	TEST_RANGE_EQUAL(rngnReverse, (tc::vector<int>{9, 8, 7, 6, 5, 4, 3, 2, 1, 0}));

	tc::drop_first_inplace(rngnReverse, 1);
	TEST_RANGE_EQUAL(rngnReverse, (tc::vector<int>{8, 7, 6, 5, 4, 3, 2, 1, 0}));

	tc::drop_first_inplace(rngnReverse, 2);
	TEST_RANGE_EQUAL(rngnReverse, (tc::vector<int>{6, 5, 4, 3, 2, 1, 0}));

	tc::drop_last_inplace(rngnReverse, 0);
	TEST_RANGE_EQUAL(rngnReverse, (tc::vector<int>{6, 5, 4, 3, 2, 1, 0}));

	tc::drop_last_inplace(rngnReverse, 1);
	TEST_RANGE_EQUAL(rngnReverse, (tc::vector<int>{6, 5, 4, 3, 2, 1}));

	tc::drop_last_inplace(rngnReverse, 2);
	TEST_RANGE_EQUAL(rngnReverse, (tc::vector<int>{6, 5, 4, 3}));
}
