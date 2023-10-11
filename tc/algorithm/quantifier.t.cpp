
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../container/container.h" // tc::vector
#include "../unittest.h"
#include "quantifier.h"

UNITTESTDEF( quantifiers ) {

	tc::vector<int> v{1,2,3,4,5,6,7};

	tc::vector<int> all_even{2,4,6,8};
	tc::vector<int> all_odd{3,5,7,9};

	static auto constexpr even = [](int const i) noexcept { return i%2==0; };

	int const existing_value = 5;
	int const non_existing_value = 9;

	auto const_range = tc::all(tc::as_const(v)); TEST_RANGE_LENGTH(const_range, 7);

	_ASSERT( tc::find_first<tc::return_bool>(const_range, existing_value));
	_ASSERT(!tc::find_first<tc::return_bool>(const_range, non_existing_value));

	_ASSERT(! tc::all_of(const_range, even));
	_ASSERT(  tc::any_of(const_range, even));

	_ASSERT(  tc::all_of(all_even, even));
	_ASSERT(  tc::any_of(all_even, even));

	_ASSERT(! tc::all_of(all_odd, even));
	_ASSERT(! tc::any_of(all_odd, even));
}

UNITTESTDEF( all_same ) {
	tc::vector<int> const v_empty{};
	tc::vector<int> const v_same{1, 1, 1, 1, 1};
	tc::vector<int> const v_different{1, 1, 2, 1, 1};

	_ASSERT(tc::all_same(v_empty));
	_ASSERT(!tc::all_same_element<tc::return_bool>(v_empty));

	_ASSERT(tc::all_same(v_same));
	_ASSERT(tc::all_same_element<tc::return_bool>(v_same));
	_ASSERTEQUAL(tc::all_same_element<tc::return_value>(v_same), 1);

	_ASSERT(!tc::all_same(v_different));
	_ASSERT(!tc::all_same_element<tc::return_bool>(v_different));
}
