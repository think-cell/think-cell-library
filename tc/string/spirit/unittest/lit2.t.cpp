/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser
	http://spirit.sourceforge.net/

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

#include <iostream>

UNITTESTDEF(x3_test_lit2)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::lit;

	{
		_ASSERT((test("kimpo", lit("kimpo"))));

		std::basic_string<char> s("kimpo");
		std::basic_string<wchar_t> ws(L"kimpo");
		_ASSERT((test("kimpo", lit(s))));
		_ASSERT((test(L"kimpo", lit(ws))));
	}

	{
		std::basic_string<char> s("kimpo");
		_ASSERT((test("kimpo", lit(s))));

		std::basic_string<wchar_t> ws(L"kimpo");
		_ASSERT((test(L"kimpo", lit(ws))));
	}

	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT((test("    kimpo", lit("kimpo"), space)));
		_ASSERT((test(L"    kimpo", lit(L"kimpo"), space)));
	}

	{
		using namespace boost::spirit::x3::iso8859_1;
		_ASSERT((test("    kimpo", lit("kimpo"), space)));
		_ASSERT((test(L"    kimpo", lit(L"kimpo"), space)));
	}
}
