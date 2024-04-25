/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	http://spirit.sourceforge.net/

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <iostream>

UNITTESTDEF(x3_test_no_case)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::no_case;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(no_case[x3::lit('x')]);

	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT(test("x", no_case[char_]));
		_ASSERT(test("X", no_case[char_('x')]));
		_ASSERT(test("X", no_case[char_('X')]));
		_ASSERT(test("x", no_case[char_('X')]));
		_ASSERT(test("x", no_case[char_('x')]));
		_ASSERT(!test("z", no_case[char_('X')]));
		_ASSERT(!test("z", no_case[char_('x')]));
		_ASSERT(test("x", no_case[char_('a', 'z')]));
		_ASSERT(test("X", no_case[char_('a', 'z')]));
		_ASSERT(!test("a", no_case[char_('b', 'z')]));
		_ASSERT(!test("z", no_case[char_('a', 'y')]));
	}
	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT(test("X", no_case[lit('x')]));
		_ASSERT(test("X", no_case[lit('X')]));
		_ASSERT(test("x", no_case[lit('X')]));
		_ASSERT(test("x", no_case[lit('x')]));
		_ASSERT(!test("z", no_case[lit('X')]));
		_ASSERT(!test("z", no_case[lit('x')]));
	}

	{
		using namespace boost::spirit::x3::iso8859_1;
		_ASSERT(test("X", no_case[char_("a-z")]));
		_ASSERT(!test("1", no_case[char_("a-z")]));
	}

	{ // test extended ASCII characters
		using namespace boost::spirit::x3::iso8859_1;
		_ASSERT(test("\xC1", no_case[char_('\xE1')]));

		_ASSERT(test("\xC9", no_case[char_("\xE5-\xEF")]));
		_ASSERT(!test("\xFF", no_case[char_("\xE5-\xEF")]));

		_ASSERT(test("\xC1\xE1", no_case[lit("\xE1\xC1")]));
		_ASSERT(test("\xE1\xE1", no_case[no_case[lit("\xE1\xC1")]]));
	}

	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT(test("Bochi Bochi", no_case[lit("bochi bochi")]));
		_ASSERT(test("BOCHI BOCHI", no_case[lit("bochi bochi")]));
		_ASSERT(!test("Vavoo", no_case[lit("bochi bochi")]));
	}

	{
		// should work!
		using namespace boost::spirit::x3::ascii;
		_ASSERT(test("x", no_case[no_case[char_]]));
		_ASSERT(test("x", no_case[no_case[char_('x')]]));
		_ASSERT(test("yabadabadoo", no_case[no_case[lit("Yabadabadoo")]]));
	}

	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT(test("X", no_case[alnum]));
		_ASSERT(test("6", no_case[alnum]));
		_ASSERT(!test(":", no_case[alnum]));

		_ASSERT(test("X", no_case[lower]));
		_ASSERT(test("x", no_case[lower]));
		_ASSERT(test("X", no_case[upper]));
		_ASSERT(test("x", no_case[upper]));
		_ASSERT(!test(":", no_case[lower]));
		_ASSERT(!test(":", no_case[upper]));
	}

	{
		using namespace boost::spirit::x3::iso8859_1;
		_ASSERT(test("X", no_case[alnum]));
		_ASSERT(test("6", no_case[alnum]));
		_ASSERT(!test(":", no_case[alnum]));

		_ASSERT(test("X", no_case[lower]));
		_ASSERT(test("x", no_case[lower]));
		_ASSERT(test("X", no_case[upper]));
		_ASSERT(test("x", no_case[upper]));
		_ASSERT(!test(":", no_case[lower]));
		_ASSERT(!test(":", no_case[upper]));
	}

	{
		using namespace boost::spirit::x3::standard;
		_ASSERT(test("X", no_case[alnum]));
		_ASSERT(test("6", no_case[alnum]));
		_ASSERT(!test(":", no_case[alnum]));

		_ASSERT(test("X", no_case[lower]));
		_ASSERT(test("x", no_case[lower]));
		_ASSERT(test("X", no_case[upper]));
		_ASSERT(test("x", no_case[upper]));
		_ASSERT(!test(":", no_case[lower]));
		_ASSERT(!test(":", no_case[upper]));
	}

	{
		// chsets
		namespace standard = boost::spirit::x3::standard;
		namespace standard_wide = boost::spirit::x3::standard_wide;

		_ASSERT(test("x", no_case[standard::char_("a-z")]));
		_ASSERT(test("X", no_case[standard::char_("a-z")]));
		_ASSERT(test(L"X", no_case[standard_wide::char_(L"a-z")]));
		_ASSERT(test(L"X", no_case[standard_wide::char_(L"X")]));
	}

	{
		using namespace boost::spirit::x3::standard;
		std::string s("bochi bochi");
		_ASSERT(test("Bochi Bochi", no_case[lit(s.c_str())]));
		_ASSERT(test("Bochi Bochi", no_case[lit(s)]));
		_ASSERT(test("Bochi Bochi", no_case[lit(s.c_str())]));
		_ASSERT(test("Bochi Bochi", no_case[lit(s)]));
	}

	{
		{
			using namespace boost::spirit::x3::standard;
			_ASSERT(!test("ą", no_case[lit('a')]));
		}
	}
}
