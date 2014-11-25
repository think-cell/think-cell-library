#include "../Range.h"
#include "range.t.h"

#include <string>
#include <vector>
#include <list>

namespace {
	using namespace RANGE_PROPOSAL_NAMESPACE;

UNITTESTDEF( create_range ) {
	
	int arr[] = {1,2,3,4,5,6,7};  UNUSED_TEST_VARIABLE(arr);
	std::vector<int> v {1,2,3,4,5,6,7,8};
	std::list<int> l {1,2,3,4,5,6,7,8,9};
	std::vector<int> exp_sub_r {3,4,5};

	const char* cchar = "const char*"; UNUSED_TEST_VARIABLE(cchar);
	char* mchar = "char*"; UNUSED_TEST_VARIABLE(mchar);
	const wchar_t* cwchar = L"const wchar_t*"; UNUSED_TEST_VARIABLE(cwchar);
	wchar_t* mwchar = L"wchar_t*"; UNUSED_TEST_VARIABLE(mwchar);

	std::string str = "string";

	//auto arr_r = make_range(arr); TEST_RANGE_LENGTH(arr_r, 7);
	//auto v_r = ensure_index_range(v); TEST_RANGE_LENGTH(v_r, 8); // Todo: fix?
	auto v_r = slice(v); TEST_RANGE_LENGTH(v_r, 8);
	//auto l_r = ensure_range(l); TEST_RANGE_LENGTH(l_r, 9);

	auto sub_r = tc::slice(v_r, 2, 5); TEST_RANGE_LENGTH(sub_r, 3); // Todo!
	//TEST_RANGE_EQUAL(exp_sub_r, sub_r);

	auto str_r = slice(str); TEST_RANGE_LENGTH(str_r, 6);

	//auto cchar_r = ensure_range(cchar); TEST_RANGE_LENGTH(cchar_r, 11);        // Todo!
	//auto mchar_r = ensure_range(mchar); TEST_RANGE_LENGTH(mchar_r, 5);
	//auto cwchar_r = ensure_range(cwchar); TEST_RANGE_LENGTH(cwchar_r, 14);
	//auto mwchar_r = ensure_range(mwchar); TEST_RANGE_LENGTH(mwchar_r, 8);
	
	auto wchar_ir = slice(mwchar, 0, 8); TEST_RANGE_LENGTH(wchar_ir, 8);

	////TEST_OUTPUT_RANGE(arr_r);
	//TEST_OUTPUT_RANGE(v_r);
	//TEST_OUTPUT_RANGE(l_r);
	//TEST_OUTPUT_RANGE(sub_r);
	//TEST_OUTPUT_RANGE(str_r);
	//TEST_OUTPUT_RANGE(cchar_r);
	//TEST_OUTPUT_RANGE(mchar_r);
	//TEST_OUTPUT_RANGE(cwchar_r);
	//TEST_OUTPUT_RANGE(mwchar_r);
}


}

