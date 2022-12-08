
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "sort_streaming.h"
#include "../string/ascii.h"
#include "../unittest.h"

UNITTESTDEF( sort_streaming ) {
	_ASSERTEQUAL( tc::make_str(tc::sort_streaming("5714926380")), "0123456789" );
	_ASSERTEQUAL( tc::make_str<char>(
		tc::transform(tc::sort_streaming("5714926380", tc::fn_greater(), TC_FN(tc::isasciidigit)), [](char& ch) noexcept {
			char chOrig = ch;
			ch -= 7;
			return chOrig;
		})),
		"9876543221100"
	);
}
