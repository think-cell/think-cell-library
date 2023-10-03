
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "literal_range.h"
#include "../string/make_c_str.h"
#include "../array.h"

UNITTESTDEF(literal_range) {
	std::same_as<tc::literal_range<int, 1, 2, 3>> auto rng = tc::literal_range_of<1, 2, 3>;
	TEST_RANGE_EQUAL(rng, (std::array<int, 3>{1, 2, 3}));
	_ASSERTEQUAL(tc::make_array(rng), (std::array<int, 3>{1, 2, 3}));
}

UNITTESTDEF(literal_range_string) {
	std::same_as<tc::literal_range<char, 'a', 'b', 'c'>> auto rng = tc::literal_range_of<'a', 'b', 'c'>;
	TEST_RANGE_EQUAL(rng, (std::array<char, 3>{'a', 'b', 'c'}));
	_ASSERTEQUAL(tc::make_array(rng), (std::array<char, 3>{'a', 'b', 'c'}));

	char const* implicit_str = rng;
	_ASSERTEQUAL(implicit_str[0], 'a');
	_ASSERTEQUAL(implicit_str[1], 'b');
	_ASSERTEQUAL(implicit_str[2], 'c');
	_ASSERTEQUAL(implicit_str[3], '\0');

	_ASSERTEQUAL(tc::as_c_str(rng), implicit_str);
	// GCC doesn't like it without the implicit cast.
	_ASSERTEQUAL(tc::implicit_cast<char const*>(tc::make_c_str(rng)), implicit_str);
	_ASSERTEQUAL(tc::make_str(rng), "abc");
}
