
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "element.h"
#include "../unittest.h"
#include "../array.h"

namespace {
	UNITTESTDEF( front ) {
		_ASSERTEQUAL(tc::front<tc::return_value>(tc::single(1)), 1);
		_ASSERTEQUAL(*tc::front<tc::return_element>(tc::single(1)), 1);
		_ASSERTEQUAL(tc::front<tc::return_value_or_none>(tc::make_empty_range<int>()), std::nullopt);
	}
}
