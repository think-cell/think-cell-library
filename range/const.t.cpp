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
#include "container.h" // tc::vector
#include "range.t.h"

namespace {
	static_assert(!tc::is_range_with_iterators<int>::value, "has..._iterator reports that int has an iterator");
	static_assert(tc::is_range_with_iterators<tc::vector<int>>::value, "has..._iterator reports that vector has no iterator");

UNITTESTDEF( const_range ) {

	tc::vector<int> original {1,2,3,4,5,6,7,8};
	tc::vector<int> modified {2,3,4,5,6,7,8,9};

	tc::vector<int> v = original;

	auto mutable_range = tc::slice(v); TEST_RANGE_LENGTH(mutable_range, 8);

	TEST_RANGE_EQUAL(original, mutable_range);
	TEST_RANGE_NOT_EQUAL(modified, mutable_range);
	tc::for_each(mutable_range, [](int& i) noexcept { i += 1; });
	TEST_RANGE_EQUAL(modified, mutable_range);
	TEST_RANGE_NOT_EQUAL(original, mutable_range);

	v = original;
	auto const_range = tc::const_slice(v); TEST_RANGE_LENGTH(const_range, 8);
	static_assert(std::is_same<decltype(tc::const_slice(v)), decltype(tc::slice(tc::as_const(v)))>::value, "wrong const type");

	TEST_RANGE_EQUAL(original, const_range);
	TEST_RANGE_NOT_EQUAL(modified, const_range);
	//tc::for_each(const_range, [](int& i) noexcept { i += 1; });        // breaks with a horrible error msg. Todo: see if we can make a better msg.
	//tc::for_each(const_range, [](int const& i) noexcept { i += 1; });  // breaks with clear msg as it should.
	tc::for_each(const_range, [](int const& i) noexcept { i; });
	TEST_RANGE_EQUAL(original, const_range);
	TEST_RANGE_NOT_EQUAL(modified, const_range);
}

UNITTESTDEF( filter_const_filter_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto fr = tc::filter(v, [](int const& i) noexcept { return (i % 2 == 0); });

	auto fcfr = tc::filter(tc::as_const(fr), [](int const& i) noexcept { return (i % 2 == 0); });
	auto ffr = tc::filter(fr, [](int const& i) noexcept { return (i % 2 == 0); });
}

UNITTESTDEF( filter_filter_const_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto frc = tc::filter(tc::as_const(v), [](int const& i) noexcept { return (i % 2 == 0); });

	auto ffrc = tc::filter(frc, [](int const& i) noexcept { return (i % 2 == 0); });
	auto fcfrc = tc::filter(tc::as_const(frc), [](int const& i) noexcept { return (i % 2 == 0); });
	auto ffrc2 = tc::filter(tc::filter(tc::as_const(v), [](int const& i) noexcept { return (i % 2 == 0); }), [](int const& i) noexcept { return (i % 2 == 0); });
}

UNITTESTDEF( transform_const_transform_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto tr = tc::transform(v, [](int const& i) noexcept { return (i * 2); });

	auto tctr = tc::transform(tc::as_const(tr), [](int const& i) noexcept { return (i * 2); });
	auto ttr = tc::transform(tr, [](int const& i) noexcept { return (i * 2); });
}

UNITTESTDEF( transform_transform_const_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto trc = tc::transform(tc::as_const(v), [](int const& i) noexcept { return (i * 2); });

	auto ttrc = tc::transform(trc, [](int const& i) noexcept { return (i * 2); });
	auto ttrc2 = tc::transform(tc::transform(tc::as_const(v), [](int const& i) noexcept { return (i * 2); }), [](int const& i) noexcept { return (i * 2); });

}

//-----------------------------------------------------------------------------------------------------------------------------
// mixed ranges constness tests

UNITTESTDEF( transform_const_filter_test ) {

	TEST_init_hack(tc::vector, int, v, {1,2,3,4,5,6,7,8,9});

	auto tr = tc::filter(v, [](int const& i) noexcept { return (i % 2 == 0); });

	auto tcfr = tc::transform(tc::as_const(tr), [](int const& i) noexcept { return (i * 2); });
	auto tfr = tc::transform(tr, [](int const& i) noexcept { return (i * 2); });
}

//-----------------------------------------------------------------------------------------------------------------------------

// Todo: more tests with more complex setups, i.e. chains of (sub)ranges with variying constness

}

