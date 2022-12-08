
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "round.h"

UNITTESTDEF( rounding_cast ) {
	STATICASSERTSAME(decltype(tc::rounding_cast<int>(1, tc::roundFLOOR)), int&&, "tc::rounding_cast<T> doesn't return T");
	STATICASSERTSAME(decltype(tc::rounding_cast<int>(1.0, tc::roundFLOOR)), int, "tc::rounding_cast<T> doesn't return T");
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundCEIL), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundNEAREST), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundCEIL), 2);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundNEAREST), 2);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundCEIL), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundNEAREST), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5), 2);
}
