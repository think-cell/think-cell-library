//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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

#include "range.h"
#include "container.h" // tc::vector
#include "range.t.h"
#include "interval.h"

#include <string>
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

	std::string str = "string";

	auto arr_r = tc::make_range(arr); TEST_RANGE_LENGTH(arr_r, 7);
	auto v_r = tc::slice(v); TEST_RANGE_LENGTH(v_r, 8);

	auto sub_r = tc::slice_by_interval(v_r, tc::make_interval(2, 5)); TEST_RANGE_LENGTH(sub_r, 3);
	TEST_RANGE_EQUAL(exp_sub_r, sub_r);

	auto str_r = tc::slice(str); TEST_RANGE_LENGTH(str_r, 6);

	auto wchar_ir = tc::slice_by_interval(mwch, tc::make_interval(0, 8)); TEST_RANGE_LENGTH(wchar_ir, 8);
}

}

