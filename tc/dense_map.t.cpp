
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
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

	tc::dense_map<MyEnum, NonCopyNonMoveable> anoncopy(tc::fill_tag(), 17);

	static_assert(
		construction_restrictiveness<NonCopyNonMoveable, NonCopyNonMoveable&&>::value == forbidden_construction,
		"not forbidden...?"
	);
	static_assert(
		!std::is_move_constructible<tc::dense_map<MyEnum, NonCopyNonMoveable>>::value,
		"Is  move constructible"
	);

	tc::array<
		tc::array<NonCopyNonMoveable,2>,
		2
	> asanoncopy(tc::fill_tag(), tc::fill_tag(), 17);


	tc::dense_map<
		MyEnum,
		tc::dense_map<MyEnum, NonCopyNonMoveable>
	> aanoncopy(tc::fill_tag(), tc::fill_tag(), 17);

	static_cast<void>(myenumONE);
	static_cast<void>(myenumTWO);
}
