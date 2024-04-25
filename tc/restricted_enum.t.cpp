
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "unittest.h"
#include "restricted_enum.h"
#include "string/char.h"
#include "base/assert_defs.h"

namespace {
	// It's safe to compare when the character type can represent all allowed values with a single code point.
	using char16_ascii = tc::restricted_enum<tc::char16, 0, 127>;
	using char32_ascii = tc::restricted_enum<char32_t, 0, 127>;
	static_assert(char16_ascii('0') == '0');
	static_assert(char16_ascii('0') == tc_utf16('0'));
	static_assert(char16_ascii('0') == U'0');
	static_assert(char32_ascii('0') == '0');
	static_assert(char32_ascii('0') == tc_utf16('0'));
	static_assert(char32_ascii('0') == U'0');

	using char16_cyrillic = tc::restricted_enum<tc::char16, 0x400, 0x4ff>;
	// static_assert(char16_cyrillic(tc_utf16('\u0410')) == 'A');
	static_assert(char16_cyrillic(tc_utf16('\u0410')) == tc_utf16('\u0410'));

	using char32_cyrillic = tc::restricted_enum<char32_t, 0x400, 0x4ff>;
	// static_assert(char32_cyrillic(U'\u0410') == 'A');
	static_assert(char32_cyrillic(U'\u0410') == U'\u0410');

	using char32_miscellaneous_symbols_and_pictographs = tc::restricted_enum<char32_t, 0x1f300, 0x1f5ff>;
	// static_assert(char32_miscellaneous_symbols_and_pictographs(U'\U0001f47e') == '0');
	// static_assert(char32_miscellaneous_symbols_and_pictographs(U'\U0001f47e') == tc_utf16('0'));
	static_assert(char32_miscellaneous_symbols_and_pictographs(U'\U0001f47e') == U'\U0001f47e');


	void implicit_cast() noexcept {
		// Should implicitly convert to less restrictive ranges.
		using char16_asciilower = tc::restricted_enum<tc::char16, tc_utf16('a'), tc_utf16('z')>;
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
		using char16_including_surrogates = tc::restricted_enum<tc::char16, tc_utf16('\0'), tc_utf16('\uffff')>;
		// char16_including_surrogates ch; // Doesn't compile: there are code units in this range that are not a code point
	}
}

namespace {
	TC_DEFINE_ENUM(ELetter, eletter, (A)(B)(C));

	using ELetterAB = tc::restricted_enum<ELetter, eletterA, eletterB>;
	using ELetterB = tc::restricted_enum<ELetter, eletterB, eletterB>;
	using ELetterBC = tc::restricted_enum<ELetter, eletterB, eletterC>;
	void restricted_enums() noexcept {
		tc_static_auto_constexpr(eletterabA, ELetterAB(eletterA));
		tc_static_auto_constexpr(eletterabB, ELetterAB(eletterB));
		ELetterAB eletterabOutput;
		eletterabOutput = eletterabA;
		eletterabOutput = eletterA;
		ELetterBC eletterbcOutput;
		eletterbcOutput = eletterabA;
		_ASSERTEQUAL(eletterabOutput, eletterbcOutput);
		// _ASSERTEQUAL(eletterabOutput, tc::char_ascii('0')); // Doesn't compile: nonsensical comparison.
		tc_static_auto_constexpr(eletterb, ELetterB(eletterB));
		_ASSERTEQUAL(eletterabA - eletterabOutput, 0);
		_ASSERTEQUAL(eletterabOutput + (eletterabB - eletterabOutput), eletterabB);

		[](ELetter) noexcept {}(eletterA);
		[](ELetter) noexcept {}(eletterabA);

		// [](ELetterAB) noexcept {}(eletterA); // Doesn't compile: can't be implicit
		// [](ELetterAB) noexcept {}(tc::implicit_cast<ELetterAB>(eletterA)); // Doesn't compile: can't be implicit
		[](ELetterAB) noexcept {}(tc::explicit_cast<ELetterAB>(eletterA)); // Compiles: it's explicit
		[](ELetterAB) noexcept {}(eletterb); // Compiles: it's more specific

		[](ELetterB) noexcept {}(tc::explicit_cast<ELetterB>(eletterabA)); // Compiles: it's explicit
		[](ELetterB) noexcept {}(tc::explicit_cast<ELetterB>(eletterA)); // Compiles: it's explicit

		[](ELetterBC) noexcept {}(tc::explicit_cast<ELetterBC>(eletterB)); // Compiles: it's explicit
		[](ELetterBC) noexcept {}(eletterb); // Compiles: it's more specific
	}
}

UNITTESTDEF(restricted_enum_arithmetic) {
	char16_ascii ch('b');
	ch += 1;
	_ASSERTEQUAL(ch, 'c');
	ch -= 2;
	_ASSERTEQUAL(ch, 'a');

	_ASSERTEQUAL(char16_ascii('a') + 2, 'c');
	_ASSERTEQUAL(char16_ascii('c') - 2, 'a');
	_ASSERTEQUAL(char16_ascii('c') - char16_ascii('a'), 2);

	ELetterAB eletterab(eletterA);
	eletterab += 1;
	_ASSERTEQUAL(eletterab, eletterB);
	eletterab -= 1;
	_ASSERTEQUAL(eletterab, eletterA);

	_ASSERTEQUAL(ELetterAB(eletterA) + 1, eletterB);
	_ASSERTEQUAL(ELetterAB(eletterB) - 1, eletterA);
	_ASSERTEQUAL(ELetterAB(eletterB) - ELetterAB(eletterA), 1);
}
