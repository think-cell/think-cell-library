//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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
}