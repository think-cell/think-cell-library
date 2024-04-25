
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "base/assert_defs.h"
#include "unittest.h"
#include "container/container.h" // tc::vector
#include "range/filter_adaptor.h"
#include "range/transform_adaptor.h"

namespace {
	static_assert(!tc::range_with_iterators<int>, "has..._iterator reports that int has an iterator");
	static_assert(tc::range_with_iterators<tc::vector<int>>, "has..._iterator reports that vector has no iterator");

UNITTESTDEF( const_range ) {

	tc::vector<int> original {1,2,3,4,5,6,7,8};
	tc::vector<int> modified {2,3,4,5,6,7,8,9};

	tc::vector<int> v = original;

	auto mutable_range = tc::all(v); TEST_RANGE_LENGTH(mutable_range, 8);

	TEST_RANGE_EQUAL(original, mutable_range);
	TEST_RANGE_NOT_EQUAL(modified, mutable_range);
	tc::for_each(mutable_range, [](int& i) noexcept { i += 1; });
	TEST_RANGE_EQUAL(modified, mutable_range);
	TEST_RANGE_NOT_EQUAL(original, mutable_range);

	v = original;
	auto const_range = tc::all(tc::as_const(v)); TEST_RANGE_LENGTH(const_range, 8);

	TEST_RANGE_EQUAL(original, const_range);
	TEST_RANGE_NOT_EQUAL(modified, const_range);
	//tc::for_each(const_range, [](int& i) noexcept { i += 1; });        // breaks with a horrible error msg. Todo: see if we can make a better msg.
	//tc::for_each(const_range, [](int const i) noexcept { i += 1; });  // breaks with clear msg as it should.
	tc::for_each(const_range, [](int const&) noexcept { });
	TEST_RANGE_EQUAL(original, const_range);
	TEST_RANGE_NOT_EQUAL(modified, const_range);
}

UNITTESTDEF( filter_const_filter_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto fr = tc::filter(v, [](int const i) noexcept { return i % 2 == 0; });

	tc::discard( tc::filter(tc::as_const(fr), [](int const i) noexcept { return i % 2 == 0; }) );
	tc::discard( tc::filter(fr, [](int const i) noexcept { return i % 2 == 0; }) );
}

UNITTESTDEF( filter_filter_const_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto frc = tc::filter(tc::as_const(v), [](int const i) noexcept { return i % 2 == 0; });

	tc::discard( tc::filter(frc, [](int const i) noexcept { return i % 2 == 0; }) );
	tc::discard( tc::filter(tc::as_const(frc), [](int const i) noexcept { return i % 2 == 0; }) );
	tc::discard( tc::filter(tc::filter(tc::as_const(v), [](int const i) noexcept { return i % 2 == 0; }), [](int const i) noexcept { return i % 2 == 0; }) );
}

UNITTESTDEF( transform_const_transform_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto tr = tc::transform(v, [](int const i) noexcept { return i * 2; });

	tc::discard( tc::transform(tc::as_const(tr), [](int const i) noexcept { return i * 2; }) );
	tc::discard( tc::transform(tr, [](int const i) noexcept { return i * 2; }) );
}

UNITTESTDEF( transform_transform_const_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto trc = tc::transform(tc::as_const(v), [](int const i) noexcept { return i * 2; });

	tc::discard( tc::transform(trc, [](int const i) noexcept { return i * 2; }) );
	tc::discard( tc::transform(tc::transform(tc::as_const(v), [](int const i) noexcept { return i * 2; }), [](int const i) noexcept { return i * 2; }) );

}

//-----------------------------------------------------------------------------------------------------------------------------
// mixed ranges constness tests

UNITTESTDEF( transform_const_filter_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto tr = tc::filter(v, [](int const i) noexcept { return i % 2 == 0; });

	tc::discard( tc::transform(tc::as_const(tr), [](int const i) noexcept { return i * 2; }) );
	tc::discard( tc::transform(tr, [](int const i) noexcept { return i * 2; }) );
}

//-----------------------------------------------------------------------------------------------------------------------------

// Todo: more tests with more complex setups, i.e. chains of (sub)ranges with variying constness

}

