
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "convert_enc.h"

namespace {
	// Only one of is_single_codeunit, is_leading_codeunit, and is_trailing_codeunit should be true for a code unit. These predicates help asserting this.
	template <typename Char>
	[[nodiscard]] bool constexpr IsSingle(Char const ch) noexcept {
		return tc::is_single_codeunit(ch) && !tc::is_leading_codeunit(ch) && !tc::is_trailing_codeunit(ch);
	}

	template <typename Char>
	[[nodiscard]] bool constexpr IsLeading(Char const ch) noexcept {
		return !tc::is_single_codeunit(ch) && tc::is_leading_codeunit(ch) && !tc::is_trailing_codeunit(ch);
	}

	template <typename Char>
	[[nodiscard]] bool constexpr IsTrailing(Char const ch) noexcept {
		return !tc::is_single_codeunit(ch) && !tc::is_leading_codeunit(ch) && tc::is_trailing_codeunit(ch);
	}
}

// UTF-8

static_assert(IsSingle('\0'));
static_assert(IsSingle('!'));
static_assert(IsSingle('~'));
static_assert(IsSingle('\x7f'));

static_assert(IsLeading('\xc2'));
static_assert(IsLeading('\xf4'));

static_assert(IsTrailing('\x80'));
static_assert(IsTrailing('\xbf'));

static_assert(IsLeading(static_cast<char>("\u200B"[0])) && IsTrailing(static_cast<char>("\u200B"[1])) && IsTrailing(static_cast<char>("\u200B"[2])));

// UTF-16

static_assert(IsSingle(tc_utf16('\x7FFF')));
static_assert(IsLeading(tc_utf16('\xD800')));
static_assert(IsLeading(tc_utf16('\xDBFF')));
static_assert(IsTrailing(tc_utf16('\xDC00')));
static_assert(IsTrailing(tc_utf16('\xDFFF')));
