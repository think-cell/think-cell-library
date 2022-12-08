
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../algorithm/append.h"
#include "../container/insert.h"
#include "../container/cont_assign.h"
#include "value_restrictive.h"

static_assert(std::is_trivial<tc::char_ascii>::value);
static_assert(std::is_trivially_copyable<tc::char_ascii>::value);
static_assert(std::is_standard_layout<tc::char_ascii>::value);

template<typename T>
struct is_convertible_to_ascii_supporting_types final {
	static constexpr auto value = tc::is_safely_convertible<tc::char_asciilower, tc::char_ascii>::value | tc::is_safely_convertible<tc::char_asciilower, char>::value | tc::is_safely_convertible<tc::char_asciilower, tc::char16>::value;
};

static_assert(is_convertible_to_ascii_supporting_types<tc::char_asciilower>::value);
static_assert(is_convertible_to_ascii_supporting_types<tc::char_asciiupper>::value);
static_assert(is_convertible_to_ascii_supporting_types<tc::char_asciidigit>::value);

static_assert(tc::char_ascii('a') == 'a' && 'a' == tc::char_ascii('a'));
static_assert(tc::char_ascii('a') == u'a' && u'a' == tc::char_ascii('a'));
static_assert(tc::equal("abc", ASCIISTR("abc")));

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

namespace {
	// It's safe to compare when the character type can represent all allowed values with a single code point.
	using char16_ascii = tc::value_restrictive<tc::char16, 0, 127>;
	using char32_ascii = tc::value_restrictive<char32_t, 0, 127>;
	static_assert(char16_ascii('0') == '0');
	static_assert(char16_ascii('0') == UTF16('0'));
	static_assert(char16_ascii('0') == U'0');
	static_assert(char32_ascii('0') == '0');
	static_assert(char32_ascii('0') == UTF16('0'));
	static_assert(char32_ascii('0') == U'0');

	using char16_cyrillic = tc::value_restrictive<tc::char16, 0x400, 0x4ff>;
	// static_assert(char16_cyrillic(UTF16('\u0410')) == 'A');
	static_assert(char16_cyrillic(UTF16('\u0410')) == UTF16('\u0410'));

	using char32_cyrillic = tc::value_restrictive<char32_t, 0x400, 0x4ff>;
	// static_assert(char32_cyrillic(U'\u0410') == 'A');
	static_assert(char32_cyrillic(U'\u0410') == UTF16('\u0410'));
	static_assert(char32_cyrillic(U'\u0410') == U'\u0410');

	using char32_miscellaneous_symbols_and_pictographs = tc::value_restrictive<char32_t, 0x1f300, 0x1f5ff>;
	// static_assert(char32_miscellaneous_symbols_and_pictographs(U'\U0001f47e') == '0');
	// static_assert(char32_miscellaneous_symbols_and_pictographs(U'\U0001f47e') == UTF16('0'));
	static_assert(char32_miscellaneous_symbols_and_pictographs(U'\U0001f47e') == U'\U0001f47e');


	void implicit_cast() noexcept {
		// Should implicitly convert to less restrictive ranges.
		using char16_asciilower = tc::value_restrictive<tc::char16, UTF16('a'), UTF16('z')>;
		char16_asciilower chAsciiLower('a');
		{
			char16_ascii chAscii(chAsciiLower);
		}
		{
			char16_ascii chAscii = chAsciiLower;
			chAscii = chAsciiLower;
		}
	}

	void test() {
		using char16_including_surrogates = tc::value_restrictive<tc::char16, UTF16('\0'), UTF16('\uffff')>;
		// char16_including_surrogates ch; // Doesn't compile: there are code units in this range that are not a code point
	}
}

namespace {
	DEFINE_ENUM(ELetter, eletter, (A)(B)(C));
}

namespace {
	using ELetterAB = tc::value_restrictive<ELetter, eletterA, eletterB>;
	using ELetterB = tc::value_restrictive<ELetter, eletterB, eletterB>;
	using ELetterBC = tc::value_restrictive<ELetter, eletterB, eletterC>;
	void restricted_enums() noexcept {
		static constexpr ELetterAB eletterabA(eletterA);
		static constexpr ELetterAB eletterabB(eletterB);
		ELetterAB eletterabOutput;
		eletterabOutput = eletterabA;
		eletterabOutput = eletterA;
		ELetterBC eletterbcOutput;
		eletterbcOutput = eletterabA;
		_ASSERTEQUAL(eletterabOutput, eletterbcOutput);
		// _ASSERTEQUAL(eletterabOutput, tc::char_ascii('0')); // Doesn't compile: nonsensical comparison.
		static constexpr ELetterB eletterb(eletterB);
		_ASSERTEQUAL(eletterabA - eletterabOutput, 0);
		_ASSERTEQUAL(eletterabOutput + (eletterabB - eletterabOutput), eletterabB);

		[](ELetter) noexcept {}(eletterA);
		[](ELetter) noexcept {}(eletterabA);

		// [](ELetterAB) noexcept {}(eletterA); // Doesn't compile: no conversion operator
		// [](ELetterAB) noexcept {}(tc::implicit_cast<ELetterAB>(eletterA)); // Doesn't compile: can't be implicit
		[](ELetterAB) noexcept {}(tc::explicit_cast<ELetterAB>(eletterA)); // Compiles: it's explicit
		[](ELetterAB) noexcept {}(eletterb); // Compiles: it's more specific

		[](ELetterB) noexcept {}(tc::explicit_cast<ELetterB>(eletterabA)); // Compiles: it's explicit
		[](ELetterB) noexcept {}(tc::explicit_cast<ELetterB>(eletterA)); // Compiles: it's explicit

		[](ELetterBC) noexcept {}(tc::explicit_cast<ELetterBC>(eletterB)); // Compiles: it's explicit
		[](ELetterBC) noexcept {}(eletterb); // Compiles: it's more specific
	}
}

UNITTESTDEF(value_restrictive_arithmetic) {
	tc::char_ascii ch('b');
	ch += 1;
	_ASSERTEQUAL(ch, 'c');
	ch -= 2;
	_ASSERTEQUAL(ch, 'a');

	ELetterAB eletterab(eletterA);
	eletterab += 1;
	_ASSERTEQUAL(eletterab, eletterB);
	eletterab -= 1;
	_ASSERTEQUAL(eletterab, eletterA);
}

UNITTESTDEF(append_char_ascii) {
	tc::string<char> str1;
	tc::cont_emplace_back(str1, tc::char_ascii('a'));
	_ASSERT(tc::equal(str1, "a"));
	tc::append(str1, ASCIISTR("bcd"));
	_ASSERT(tc::equal(str1, "abcd"));

	tc::string<char32_t> str2;
	tc::cont_emplace_back(str2, tc::char_ascii('a'));
	_ASSERT(tc::equal(str2, U"a"));
	tc::append(str2, ASCIISTR("bcd"));
	_ASSERT(tc::equal(str2, U"abcd"));

	tc::string<char> str3;
	tc::append(str3, "a", ASCIISTR("bc"), "d");
	_ASSERT(tc::equal(str3, "abcd"));

	tc::string<char32_t> str4;
	tc::append(str4, U"a", ASCIISTR("bc"), U"d");
	_ASSERT(tc::equal(str4, U"abcd"));

	tc::string<char32_t> str5;
	tc::append(str5, "a", ASCIISTR("bc"), "d");
	_ASSERT(tc::equal(str5, U"abcd"));

	tc::string<tc::char_ascii> str6;
	tc::append(str6, ASCIISTR("abc"));
	_ASSERT(tc::equal(str6, "abc"));
	tc::cont_emplace_back(str6, tc::char_ascii('d'));
	_ASSERT(tc::equal(str6, "abcd"));
	tc::drop_first_inplace(str6);
	_ASSERT(tc::equal(str6, "bcd"));
	tc::drop_last_inplace(str6);
	_ASSERT(tc::equal(str6, "bc"));
	tc::cont_assign(str6);
	_ASSERT(tc::empty(str6));
	tc::append(str6, ASCIISTR("abc"));

	tc::ptr_range<tc::char_ascii const> str7 = str6;
	_ASSERT(tc::equal(str7, "abc"));
}

static_assert(std::is_same<char, tc::common_type_t<char, tc::char_ascii>>::value);
static_assert(std::is_same<char, tc::common_type_t<tc::char_ascii, char>>::value);
static_assert(std::is_same<wchar_t, tc::common_type_t<wchar_t, tc::char_ascii>>::value);
static_assert(std::is_same<wchar_t, tc::common_type_t<tc::char_ascii, wchar_t>>::value);
