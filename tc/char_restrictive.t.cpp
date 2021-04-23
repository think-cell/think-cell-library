
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"
#include "char_restrictive.h"

static_assert(std::is_trivial<tc::char_ascii>::value);
static_assert(std::is_trivially_copyable<tc::char_ascii>::value);
static_assert(std::is_standard_layout<tc::char_ascii>::value);

static_assert('a' == tc::char_ascii('a'));
static_assert(tc::char_ascii('a') == u'a');
static_assert(tc::equal("abc", ASCIISTR("abc")));

UNITTESTDEF(append_char_ascii) {
	std::basic_string<char> str1;
	tc::cont_emplace_back(str1, tc::char_ascii('a'));
	_ASSERT(tc::equal(str1, "a"));
	tc::append(str1, ASCIISTR("bcd"));
	_ASSERT(tc::equal(str1, "abcd"));

	std::basic_string<char32_t> str2;
	tc::cont_emplace_back(str2, tc::char_ascii('a'));
	_ASSERT(tc::equal(str2, U"a"));
	tc::append(str2, ASCIISTR("bcd"));
	_ASSERT(tc::equal(str2, U"abcd"));

	std::basic_string<char> str3;
	tc::append(str3, "a", ASCIISTR("bc"), "d");
	_ASSERT(tc::equal(str3, "abcd"));

	std::basic_string<char32_t> str4;
	tc::append(str4, U"a", ASCIISTR("bc"), U"d");
	_ASSERT(tc::equal(str4, U"abcd"));

	std::basic_string<char32_t> str5;
	tc::append(str5, "a", ASCIISTR("bc"), "d");
	_ASSERT(tc::equal(str5, U"abcd"));

	std::basic_string<tc::char_ascii> str6;
	tc::append(str6, ASCIISTR("abc"));
	_ASSERT(tc::equal(str6, "abc"));
	tc::cont_emplace_back(str6, tc::char_ascii('d'));
	_ASSERT(tc::equal(str6, "abcd"));
	tc::drop_first_inplace(str6);
	_ASSERT(tc::equal(str6, "bcd"));
	tc::drop_last_inplace(str6);
	_ASSERT(tc::equal(str6, "bc"));
	tc::cont_clear(str6);
	_ASSERT(tc::empty(str6));
	tc::append(str6, ASCIISTR("abc"));

	tc::ptr_range<tc::char_ascii const> str7 = str6;
	_ASSERT(tc::equal(str7, "abc"));

	tc::char_ascii const* str8 = tc::ptr_begin(str6);
	_ASSERT(tc::equal(str8, "abc"));
}

static_assert(std::is_same<char, tc::common_type_t<char, tc::char_ascii>>::value);
static_assert(std::is_same<char, tc::common_type_t<tc::char_ascii, char>>::value);
static_assert(std::is_same<wchar_t, tc::common_type_t<wchar_t, tc::char_ascii>>::value);
static_assert(std::is_same<wchar_t, tc::common_type_t<tc::char_ascii, wchar_t>>::value);
