// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "safe_comparison.h"
#include "../unittest.h"
#include "../string/char.h"

// Same type is safe.
static_assert(tc::safe_comparison<int, int>);
static_assert(tc::safe_comparison<unsigned, unsigned>);
static_assert(tc::safe_comparison<tc::char16, tc::char16>);
// Mixed signedness is unsafe.
static_assert(!tc::safe_comparison<int, unsigned>);
static_assert(!tc::safe_comparison<unsigned, int>);
// Mixed chars are unsafe.
static_assert(!tc::safe_comparison<char, char16_t>);
static_assert(!tc::safe_comparison<char8_t, char32_t>);
// chars and ints are unsafe.
static_assert(!tc::safe_comparison<char, short>);
static_assert(!tc::safe_comparison<char32_t, int>);
