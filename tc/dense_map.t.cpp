
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"
#include "dense_map.h"
#include "enumset.h"

static_assert(!std::is_convertible<tc::array<int,10>, tc::array<int, 9>>::value);
static_assert(!std::is_convertible<tc::array<int,9>, tc::array<int, 10>>::value);

DEFINE_ENUM(
	MyEnum,
	myenum,
	(ONE)(TWO)
)

struct NonCopyNonMoveable {
	NonCopyNonMoveable(NonCopyNonMoveable const&) = delete;
	NonCopyNonMoveable(NonCopyNonMoveable&&) = delete;

	NonCopyNonMoveable(int) {}
};

UNITTESTDEF(dense_map_with_non_moveable_type) {

	tc::dense_map<MyEnum, NonCopyNonMoveable> anoncopy(tc::fill_tag, 17);

	static_assert(
		tc::econstructionFORBIDDEN==tc::construction_restrictiveness<NonCopyNonMoveable, NonCopyNonMoveable&&>::value,
		"not forbidden...?"
	);
	static_assert(
		!std::is_move_constructible<tc::dense_map<MyEnum, NonCopyNonMoveable>>::value,
		"Is  move constructible"
	);

	tc::array<
		tc::array<NonCopyNonMoveable,2>,
		2
	> asanoncopy(tc::fill_tag, tc::fill_tag, 17);


	tc::dense_map<
		MyEnum,
		tc::dense_map<MyEnum, NonCopyNonMoveable>
	> aanoncopy(tc::fill_tag, tc::fill_tag, 17);

	static_cast<void>(myenumONE);
	static_cast<void>(myenumTWO);

#ifndef _MSC_VER // MSVC support for guaranteed copy elision seems to be incomplete.
	// https://developercommunity.visualstudio.com/t/chained-copy-elision-with-function-resul/1404507
	[[maybe_unused]] auto anoncopyFromTransform = tc::make_dense_map<MyEnum>(47, 11).transform([](int n) {
		return NonCopyNonMoveable(n);
	});
#endif
}

UNITTESTDEF(test_dense_map_recursive_transform) {
	auto const dm = tc::make_dense_map<MyEnum>(5, 10);
	auto const dm2 = dm.transform([](auto const n) noexcept { return n * 0.5; });
	_ASSERTEQUAL(dm2[myenumONE], 2.5);
	_ASSERTEQUAL(dm2[myenumTWO], 5);

	auto const dm3 = tc::make_dense_map<MyEnum>(tc::make_dense_map<bool>(5, 10), tc::make_dense_map<bool>(3, 6));
	auto const dm4 = dm3.transform<1>([](auto const n) noexcept { return n * 0.5; });
	_ASSERTEQUAL(dm4[myenumONE][false], 2.5);
	_ASSERTEQUAL(dm4[myenumONE][true], 5);
	_ASSERTEQUAL(dm4[myenumTWO][false], 1.5);
	_ASSERTEQUAL(dm4[myenumTWO][true], 3);
}

namespace {
	constexpr tc::dense_map<MyEnum, int> an(tc::func_tag, [n = 0](auto e) mutable noexcept {
		return n++ | tc::underlying_cast(e) << 4;
	});

	static_assert( 0x00 == an[myenumONE] );
	static_assert( 0x11 == an[myenumTWO] );
}

UNITTESTDEF(test_dense_map_recursive_func_tag) {
	tc::dense_map<MyEnum, tc::dense_map<MyEnum, tc::dense_map<bool, int>>> aaan(tc::func_tag, [n = 0](auto e1, auto e2, bool b3) mutable noexcept {
		return n++ | tc::underlying_cast(e1) << 6 | tc::underlying_cast(e2) << 5 | tc::underlying_cast(b3) << 4;
	});

	_ASSERTEQUAL( aaan[myenumONE][myenumONE][false], 0x00 );
	_ASSERTEQUAL( aaan[myenumONE][myenumONE][true], 0x11 );
	_ASSERTEQUAL( aaan[myenumONE][myenumTWO][false], 0x22 );
	_ASSERTEQUAL( aaan[myenumONE][myenumTWO][true], 0x33 );
	_ASSERTEQUAL( aaan[myenumTWO][myenumONE][false], 0x44 );
	_ASSERTEQUAL( aaan[myenumTWO][myenumONE][true], 0x55 );
	_ASSERTEQUAL( aaan[myenumTWO][myenumTWO][false], 0x66 );
	_ASSERTEQUAL( aaan[myenumTWO][myenumTWO][true], 0x77 );
}

UNITTESTDEF(test_dense_map_enumset_as_key) {
	constexpr tc::dense_map<tc::enumset<MyEnum>, int> dm(1,2,3,4);
	static_assert(dm[tc::empty_range()] == 1);
	static_assert(dm[tc::enumset<MyEnum>(myenumONE)] == 2);
	static_assert(dm[tc::enumset<MyEnum>(myenumTWO)] == 3);
	static_assert(dm[myenumONE | myenumTWO] == 4);
	_ASSERT(tc::equal(dm, tc::iota(1, 5)));
}

UNITTESTDEF(test_dense_map_transform_move) {
	auto const dm = tc::dense_map<MyEnum, int>(1,2).transform([](int&& n) {
		return n;
	});
	_ASSERT(tc::equal(dm, tc::iota(1, 3)));
}
