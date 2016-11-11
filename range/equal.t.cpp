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

namespace {

//---- Equal with vector<int> -------------------------------------------------------------------------------------------------
UNITTESTDEF( equal_vec_int ) {
	using namespace tc;

	tc::vector<int> ve;
	tc::vector<int> ve_1;
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v123_1, {1,2,3});
	TEST_init_hack(tc::vector, int, v143, {1,4,3});
	TEST_init_hack(tc::vector, int, v1234, {1,2,3,4});
	
	TEST_RANGE_EQUAL(ve, ve);
	TEST_RANGE_EQUAL(ve, ve_1);

	TEST_RANGE_NOT_EQUAL(ve, v123);
	TEST_RANGE_NOT_EQUAL(v123, ve);
	
	TEST_RANGE_EQUAL(v123, v123);
	TEST_RANGE_EQUAL(v123, v123_1);

	
	TEST_RANGE_NOT_EQUAL(v123, v143);
	TEST_RANGE_NOT_EQUAL(v123, v1234);
	TEST_RANGE_NOT_EQUAL(v1234, v123);
}

UNITTESTDEF( equal_vec_int_pred ) {
	using namespace tc;

	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v234, {2,3,4});

	auto ofByOne = [](int lhs, int rhs) noexcept { return ((lhs + 1) == rhs); }; // unsymmetrical to uncover wrong order of argument application to the predicate

	_ASSERT(tc::equal(v123, v234, ofByOne));
	_ASSERT(!tc::equal(v234, v123, ofByOne));
	_ASSERT(!tc::equal(v123, v123, ofByOne));

}

//---- Equal with generators --------------------------------------------------------------------------------------------------

UNITTESTDEF( equal_generator ) {
	using namespace tc;

	tc::vector<int> ve;
	tc::vector<int> ve_1;
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v123_1, {1,2,3});
	TEST_init_hack(tc::vector, int, v143, {1,4,3});
	TEST_init_hack(tc::vector, int, v1234, {1,2,3,4});

	auto ge = make_generator_range(ve);
	auto g123 = make_generator_range(v123);
	auto g143 = make_generator_range(v143);
	auto g1234 = make_generator_range(v1234);
	
	STATIC_ASSERT(is_range_with_iterators<decltype(ve)>::value);
	STATIC_ASSERT(!is_range_with_iterators<decltype(ge)>::value);

	TEST_RANGE_EQUAL(ve, ge);
	TEST_RANGE_EQUAL(ge, ve);
	//TEST_RANGE_EQUAL(ge, ge); // Fails to compile with proper msg, as it should be.

	TEST_RANGE_EQUAL(v123, g123);
	TEST_RANGE_EQUAL(g123, v123);
	TEST_RANGE_EQUAL(v1234, g1234);
	TEST_RANGE_EQUAL(g1234, v1234);

	TEST_RANGE_NOT_EQUAL(v123, g143);
	TEST_RANGE_NOT_EQUAL(g123, v143);
	TEST_RANGE_NOT_EQUAL(v123, g1234);
	TEST_RANGE_NOT_EQUAL(g123, v1234);
	TEST_RANGE_NOT_EQUAL(v1234, g123);
	TEST_RANGE_NOT_EQUAL(g1234, v123);

}

UNITTESTDEF( equal_generator_pred ) {
	using namespace tc;

	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v234, {2,3,4});

	auto g123 = make_generator_range(v123);
	auto g234 = make_generator_range(v234);

	auto ofByOne = [](int lhs, int rhs) noexcept { return ((lhs + 1) == rhs); };  // unsymmetrical to uncover wrong order of argument application to the predicate

	//_ASSERT(tc::equal(g123, g234, ofByOne)); // Fails to compile with proper msg, as it should be.

	_ASSERT(tc::equal(v123, g234, ofByOne));
	_ASSERT(!tc::equal(v234, g123, ofByOne));
	_ASSERT(!tc::equal(v123, g123, ofByOne));

	_ASSERT(tc::equal(g123, v234, ofByOne));
	_ASSERT(!tc::equal(g234, v123, ofByOne));
	_ASSERT(!tc::equal(g123, v123, ofByOne));
}

}

