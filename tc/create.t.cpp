
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "base/assert_defs.h"
#include "unittest.h"
#include "container/container.h" // tc::vector
#include "container/string.h" // tc::string
#include "interval.h"

#include <forward_list>

namespace {

UNITTESTDEF( create_range ) {

	int arr[] = {1,2,3,4,5,6,7};  UNUSED_TEST_VARIABLE(arr);
	tc::vector<int> v {1,2,3,4,5,6,7,8};
	std::forward_list<int> l {1,2,3,4,5,6,7,8,9};
	tc::vector<int> exp_sub_r {3,4,5};

	char const* cch = "char const*"; UNUSED_TEST_VARIABLE(cch);
	char ach[] = "char*";
	char* mch = ach; UNUSED_TEST_VARIABLE(mch);
	wchar_t const* cwch = L"wchar_t const*"; UNUSED_TEST_VARIABLE(cwch);
	wchar_t awch[] = L"wchar_t*";
	wchar_t* mwch = awch; UNUSED_TEST_VARIABLE(mwch);

	tc::string<char> str = "string";

	auto arr_r = tc::make_view(arr); TEST_RANGE_LENGTH(arr_r, 7);
	auto v_r = tc::slice(v); TEST_RANGE_LENGTH(v_r, 8);

	auto sub_r = tc::slice_by_interval(v_r, tc::make_interval(2, 5)); TEST_RANGE_LENGTH(sub_r, 3);
	TEST_RANGE_EQUAL(exp_sub_r, sub_r);

	auto str_r = tc::slice(str); TEST_RANGE_LENGTH(str_r, 6);

	auto wchar_ir = tc::slice_by_interval(mwch, tc::make_interval(0, 8)); TEST_RANGE_LENGTH(wchar_ir, 8);
}

}

