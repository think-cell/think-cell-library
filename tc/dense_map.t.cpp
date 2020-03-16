
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"
#include "dense_map.h"

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
	[[maybe_unused]] auto anoncopyFromTransform = tc::make_dense_map<MyEnum>(47, 11).transform([](int n) {
		return NonCopyNonMoveable(n);
	});
#endif
}

UNITTESTDEF(test_only) {
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