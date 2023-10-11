
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "longest_common_prefix.h"
#include "../unittest.h"

namespace {
UNITTESTDEF( longest_common_prefix ) {
	static auto constexpr CheckLCP = [](auto const& strLhs, auto const& strRhs, auto const& strPrefix, auto const& strSuffixLhs, auto const& strSuffixRhs) noexcept {
		tc_auto_cref(pairstrstrPrefix, tc::longest_common_prefix<tc::return_take>(strLhs, strRhs));
		_ASSERT(tc::equal(pairstrstrPrefix.first, pairstrstrPrefix.second));
		_ASSERT(tc::equal(pairstrstrPrefix.first, strPrefix));
		tc_auto_cref(pairstrstrSuffix, tc::longest_common_prefix<tc::return_drop>(strLhs, strRhs));
		_ASSERT(tc::equal(pairstrstrSuffix.first, strSuffixLhs));
		_ASSERT(tc::equal(pairstrstrSuffix.second, strSuffixRhs));
	};
	CheckLCP("abcd", "ab", "ab", "cd", "");
	CheckLCP("ac", "abcd", "a", "c", "bcd");
	CheckLCP("x", "abcd", "", "x", "abcd");
}
}

