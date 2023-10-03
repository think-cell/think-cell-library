
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "cont_assign.h"
#include "container.h"
#include "../unittest.h"
#include "../range/empty_range.h"
#include "../range/literal_range.h"

UNITTESTDEF(cont_assign_vec) {
	tc::vector<int> vec;

	tc::cont_assign(vec, tc::literal_range_of<1, 2, 3, 4, 5, 6>);
	TEST_RANGE_EQUAL(vec, (tc::literal_range_of<1, 2, 3, 4, 5, 6>));

	tc::cont_assign(vec, tc::literal_range_of<10, 20, 30>, tc::literal_range_of<40>);
	TEST_RANGE_EQUAL(vec, (tc::literal_range_of<10, 20, 30, 40>));

	tc::cont_assign(vec);
	TEST_RANGE_EQUAL(vec, tc::empty_range{});
}

UNITTESTDEF(cont_assign_slice) {
	tc::vector<int> vec = {-1, -1, 0, 0, 0, -1, -1};

	tc::cont_assign(tc::slice(vec, tc::begin(vec) + 2, tc::end(vec) - 2), tc::literal_range_of<1, 2, 3>);
	TEST_RANGE_EQUAL(vec, (tc::literal_range_of<-1, -1, 1, 2, 3, -1, -1>));
}

UNITTESTDEF(cont_assign_take_drop) {
	tc::vector<int> vec = {0, 0, 0, -1, 0, 0, 0};

	tc::cont_assign(tc::begin_next<tc::return_take>(vec, 3), tc::literal_range_of<1, 2, 3>);
	tc::cont_assign(tc::begin_next<tc::return_drop>(vec, 4), tc::literal_range_of<4, 5, 6>);
	TEST_RANGE_EQUAL(vec, (tc::literal_range_of<1, 2, 3, -1, 4, 5, 6>));

	tc::cont_assign(tc::begin_next<tc::return_take>(vec, 3), tc::begin_next<tc::return_drop>(vec, 4));
	TEST_RANGE_EQUAL(vec, (tc::literal_range_of<4, 5, 6, -1, 4, 5, 6>));
}
