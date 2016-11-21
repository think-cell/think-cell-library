//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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
#include "range.t.h"

#include "reverse_adaptor.h"

UNITTESTDEF(tc_reverse_random_access) {
	using namespace tc;
	
	TEST_RANGE_EQUAL(tc::make_initializer_list({1,2,3}), tc::reverse(tc::make_initializer_list({3,2,1})));
	
	// test iterators:
	auto rng = tc::make_initializer_list({1,2,3});
	auto revrng = tc::reverse(rng);
	auto it = boost::begin(revrng);
	_ASSERTEQUAL(*it++, 3);
	_ASSERTEQUAL(*it++, 2);
	_ASSERTEQUAL(*it++, 1);
	_ASSERT(boost::end(revrng) == it);

	--it;
	_ASSERTEQUAL(*it,1);

	// Test random access
	it -= 2;
	_ASSERTEQUAL(*it,3);

	it += 3;
	_ASSERT(boost::end(revrng) == it);
}

#include <numeric>

UNITTESTDEF(tc_reverse_binary_search) {
	using namespace tc;
	
	tc::vector<int> vecn(64);
	std::iota(boost::begin(vecn), boost::end(vecn), 0);
	
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
	using namespace tc;

	std::list<int> rng{1,2,3};
	auto rngrev = tc::reverse(rng);
	auto it = boost::begin(rngrev);
	_ASSERTEQUAL(*it++, 3);
	_ASSERTEQUAL(*it++, 2);
	_ASSERTEQUAL(*it++, 1);
	_ASSERT(boost::end(rngrev) == it);

	TEST_RANGE_EQUAL(tc::reverse(rng), tc::make_initializer_list({3,2,1}));
	TEST_RANGE_EQUAL(tc::reverse(tc::reverse(rng)), rng);
}

UNITTESTDEF(tc_reverse_base_bound) {
	using namespace tc;

	std::list<int> rng{1,2,3,4};
	auto rngreverse = tc::reverse(rng);
	auto it = boost::next(boost::next(boost::begin(rngreverse)));
	TEST_RANGE_EQUAL(
		tc::reverse(tc::take(rngreverse,it)),
		tc::drop(rng, it.border_base())
	);

	_ASSERTEQUAL(*it,*it.element_base());
}

UNITTESTDEF(tc_reverse_drop_first_last_inplace) {
	using namespace tc;

	tc::vector<int> vecn{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	auto&& rngnReverse = tc::reverse(tc::make_range(vecn));

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
