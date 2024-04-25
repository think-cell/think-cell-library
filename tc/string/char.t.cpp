
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../algorithm/append.h"
#include "../container/insert.h"
#include "../container/cont_assign.h"
#include "char.h"

static_assert(std::is_trivial<tc::char_ascii>::value);
static_assert(std::is_trivially_copyable<tc::char_ascii>::value);
static_assert(std::is_standard_layout<tc::char_ascii>::value);

template<typename T>
struct is_convertible_to_ascii_supporting_types final {
	static constexpr auto value = tc::safely_convertible_to<tc::char_asciilower, tc::char_ascii> | tc::safely_convertible_to<tc::char_asciilower, char> | tc::safely_convertible_to<tc::char_asciilower, tc::char16>;
};

static_assert(is_convertible_to_ascii_supporting_types<tc::char_asciilower>::value);
static_assert(is_convertible_to_ascii_supporting_types<tc::char_asciiupper>::value);
static_assert(is_convertible_to_ascii_supporting_types<tc::char_asciidigit>::value);

static_assert(tc::char_ascii('a') == 'a' && 'a' == tc::char_ascii('a'));
static_assert(tc::char_ascii('a') == u'a' && u'a' == tc::char_ascii('a'));
static_assert(tc::size("abc"_tc) == 3);
static_assert(tc::equal("abc", "abc"_tc));

static_assert(tc::char_ascii('a') == tc::char_asciilower('a') && tc::char_asciilower('a') == tc::char_ascii('a'));
static_assert(tc::char_ascii('z') == tc::char_asciilower('z') && tc::char_asciilower('z') == tc::char_ascii('z'));
static_assert(tc::char_ascii('z') != tc::char_asciilower('a') && tc::char_asciilower('a') != tc::char_ascii('z'));
static_assert(tc::char_ascii(' ') != tc::char_asciilower('a') && tc::char_asciilower('a') != tc::char_ascii(' '));

static_assert(tc::char_ascii('A') == tc::char_asciiupper('A') && tc::char_asciiupper('A') == tc::char_ascii('A'));
static_assert(tc::char_ascii('Z') == tc::char_asciiupper('Z') && tc::char_asciiupper('Z') == tc::char_ascii('Z'));
static_assert(tc::char_ascii('Z') != tc::char_asciiupper('A') && tc::char_asciiupper('A') != tc::char_ascii('Z'));
static_assert(tc::char_ascii(' ') != tc::char_asciiupper('A') && tc::char_asciiupper('A') != tc::char_ascii(' '));

static_assert(tc::char_ascii('0') == tc::char_asciidigit('0') && tc::char_asciidigit('0') == tc::char_ascii('0'));
static_assert(tc::char_ascii('9') == tc::char_asciidigit('9') && tc::char_asciidigit('9') == tc::char_ascii('9'));
static_assert(tc::char_ascii('9') != tc::char_asciidigit('0') && tc::char_asciidigit('0') != tc::char_ascii('9'));
static_assert(tc::char_ascii(' ') != tc::char_asciidigit('0') && tc::char_asciidigit('0') != tc::char_ascii(' '));

UNITTESTDEF(append_char_ascii) {
	tc::string<char> str1;
	tc::cont_emplace_back(str1, tc::char_ascii('a'));
	_ASSERT(tc::equal(str1, "a"));
	tc::append(str1, "bcd"_tc);
	_ASSERT(tc::equal(str1, "abcd"));

	tc::string<char32_t> str2;
	tc::cont_emplace_back(str2, tc::char_ascii('a'));
	_ASSERT(tc::equal(str2, U"a"));
	tc::append(str2, "bcd"_tc);
	_ASSERT(tc::equal(str2, U"abcd"));

	tc::string<char> str3;
	tc::append(str3, "a", "bc"_tc, "d");
	_ASSERT(tc::equal(str3, "abcd"));

	tc::string<char32_t> str4;
	tc::append(str4, U"a", "bc"_tc, U"d");
	_ASSERT(tc::equal(str4, U"abcd"));

	tc::string<char32_t> str5;
	tc::append(str5, "a", "bc"_tc, "d");
	_ASSERT(tc::equal(str5, U"abcd"));

	tc::string<tc::char_ascii> str6;
	tc::append(str6, "abc"_tc);
	_ASSERT(tc::equal(str6, "abc"));
	tc::cont_emplace_back(str6, tc::char_ascii('d'));
	_ASSERT(tc::equal(str6, "abcd"));
	tc::drop_first_inplace(str6);
	_ASSERT(tc::equal(str6, "bcd"));
	tc::drop_last_inplace(str6);
	_ASSERT(tc::equal(str6, "bc"));
	tc::cont_assign(str6);
	_ASSERT(tc::empty(str6));
	tc::append(str6, "abc"_tc);

	tc::span<tc::char_ascii const> str7 = str6;
	_ASSERT(tc::equal(str7, "abc"));
}

static_assert(std::is_same<char, tc::common_type_t<char, tc::char_ascii>>::value);
static_assert(std::is_same<char, tc::common_type_t<tc::char_ascii, char>>::value);
static_assert(std::is_same<wchar_t, tc::common_type_t<wchar_t, tc::char_ascii>>::value);
static_assert(std::is_same<wchar_t, tc::common_type_t<tc::char_ascii, wchar_t>>::value);

static_assert(0 == std::numeric_limits<tc::char16>::lowest() && 0xFFFF == std::numeric_limits<tc::char16>::max());

UNITTESTDEF(string_literal) {
	auto ascii = "abc"_tc;
	STATICASSERTSAME(decltype(ascii), TC_FWD(tc::literal_range<tc::char_ascii, 'a', 'b', 'c'>));

	// Due to the bug shown in https://godbolt.org/z/Kzc8orP4j, we can't use the same characters as the ASCII string.
	// (MSVC thinks u8"abc" is the same instantiation as "abc" and re-uses the same result.)
	// It is already fixed in newer versions of MSVC and shouldn't affect our codebase - why would we have an ASCII and a UTF-8 string with the same characters.
	auto utf8 = u8"ABC"_tc;
	STATICASSERTSAME(decltype(utf8), TC_FWD(tc::literal_range<char, u8'A', u8'B', u8'C'>));

	auto utf16 = u"abc"_tc;
	STATICASSERTSAME(decltype(utf16), TC_FWD(tc::literal_range<tc::char16, u'a', u'b', u'c'>));

	auto utf32 = U"abc"_tc;
	STATICASSERTSAME(decltype(utf32), TC_FWD(tc::literal_range<char32_t, U'a', U'b', U'c'>));
}

UNITTESTDEF(char_literal) {
	std::same_as<tc::char_ascii> auto ascii = 'a'_tc;
	_ASSERTEQUAL(ascii, 'a');

	std::same_as<char> auto utf8 = u8'a'_tc;
	_ASSERTEQUAL(utf8, 'a');

	std::same_as<tc::char16> auto utf16 = u'a'_tc;
	_ASSERTEQUAL(utf16, static_cast<tc::char16>('a'));

	std::same_as<char32_t> auto utf32 = U'a'_tc;
	_ASSERTEQUAL(utf32, U'a');
}
