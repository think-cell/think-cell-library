
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.h"
#include "range.t.h"

namespace {

//---- Equal with vector<int> -------------------------------------------------------------------------------------------------
UNITTESTDEF( equal_vec_int ) {
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
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v234, {2,3,4});

	auto ofByOne = [](int lhs, int rhs) noexcept { return (lhs + 1) == rhs; }; // unsymmetrical to uncover wrong order of argument application to the predicate

	_ASSERT(tc::equal(v123, v234, ofByOne));
	_ASSERT(!tc::equal(v234, v123, ofByOne));
	_ASSERT(!tc::equal(v123, v123, ofByOne));

}

//---- Equal with generators --------------------------------------------------------------------------------------------------

UNITTESTDEF( equal_generator ) {
	tc::vector<int> ve;
	tc::vector<int> ve_1;
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v123_1, {1,2,3});
	TEST_init_hack(tc::vector, int, v143, {1,4,3});
	TEST_init_hack(tc::vector, int, v1234, {1,2,3,4});

	auto ge = tc::make_generator_range(ve);
	auto g123 = tc::make_generator_range(v123);
	auto g143 = tc::make_generator_range(v143);
	auto g1234 = tc::make_generator_range(v1234);

	STATIC_ASSERT(tc::is_range_with_iterators<decltype(ve)>::value);
	STATIC_ASSERT(!tc::is_range_with_iterators<decltype(ge)>::value);

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
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v234, {2,3,4});

	auto g123 = tc::make_generator_range(v123);
	auto g234 = tc::make_generator_range(v234);

	auto ofByOne = [](int lhs, int rhs) noexcept { return (lhs + 1) == rhs; };  // unsymmetrical to uncover wrong order of argument application to the predicate

	//_ASSERT(tc::equal(g123, g234, ofByOne)); // Fails to compile with proper msg, as it should be.

	_ASSERT(tc::equal(v123, g234, ofByOne));
	_ASSERT(!tc::equal(v234, g123, ofByOne));
	_ASSERT(!tc::equal(v123, g123, ofByOne));

	_ASSERT(tc::equal(g123, v234, ofByOne));
	_ASSERT(!tc::equal(g234, v123, ofByOne));
	_ASSERT(!tc::equal(g123, v123, ofByOne));
}

}

