/*=============================================================================
	Copyright (c) 2018-2023 Nikita Kniazev

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3/support/utility/utf8.hpp"
#include <string>

static bool nowarn_constexpr(bool x) { return x; }

UNITTESTDEF(x3_test_to_utf8)
{
	using boost::spirit::x3::to_utf8;
	using namespace std::string_literals;

	_ASSERT(tc::equal("\xED\x9F\xBF", to_utf8(0xD7FFul).c_str()));
	_ASSERT(tc::equal("\xEE\x80\x80", to_utf8(0xE000ul).c_str()));

	if (nowarn_constexpr(sizeof(L"\u00FF") == 2))
		_ASSERT(tc::equal("\xC3\xBF", to_utf8(L"\u00FF"[0]).c_str()));
	_ASSERT(tc::equal("\xC3\xBF", to_utf8(U'\u00FF').c_str()));
	if (nowarn_constexpr(sizeof(L"\uFFE1") == 2))
		_ASSERT(tc::equal("\xEF\xBF\xA1", to_utf8(L"\uFFE1"[0]).c_str()));
	_ASSERT(tc::equal("\xEF\xBF\xA1", to_utf8(U'\uFFE1').c_str()));
	if (nowarn_constexpr(sizeof(L"\U0001F9D0") == 2))
		_ASSERT(tc::equal("\xF0\x9F\xA7\x90", to_utf8(L"\U0001F9D0"[0]).c_str()));
	_ASSERT(tc::equal("\xF0\x9F\xA7\x90", to_utf8(U'\U0001F9D0').c_str()));
	_ASSERT(tc::equal("\xF0\x9F\xA7\x90\xF0\x9F\xA7\xA0", to_utf8(L"\U0001F9D0\U0001F9E0").c_str()));
	_ASSERT(tc::equal("\xF0\x9F\xA7\x90\xF0\x9F\xA7\xA0", to_utf8(U"\U0001F9D0\U0001F9E0").c_str()));
	_ASSERT(tc::equal("\xF0\x9F\xA7\x90\xF0\x9F\xA7\xA0", to_utf8(L"\U0001F9D0\U0001F9E0"s).c_str()));
	_ASSERT(tc::equal("\xF0\x9F\xA7\x90\xF0\x9F\xA7\xA0", to_utf8(U"\U0001F9D0\U0001F9E0"s).c_str()));
}
