/*=============================================================================
	Copyright (c) 2013 Carl Barron
	Copyright (c) 2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#define BOOST_SPIRIT_X3_DEBUG
#include "test.hpp"
#include "../x3.hpp"
#include "../support/char_encoding/unicode.hpp"
#include <boost/fusion/include/at.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/mpl/int.hpp>
#include <iostream>
#include <numeric>
#include <vector>
#include <optional>

struct roman
{
	std::optional<int> a;
	std::optional<int> b;
	std::optional<int> c;
};

BOOST_FUSION_ADAPT_STRUCT(roman,
	a, b, c
)

int eval(roman const & c)
{
	return c.a.value_or(0) + c.b.value_or(0) + c.c.value_or(0);
}

UNITTESTDEF(x3_test_symbols3)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::symbols;

	{ // construction from initializer-list
		symbols<int> const ones =
		{
			{"I", 1}, {"II", 2}, {"III", 3}, {"IV", 4},
			{"V", 5}, {"VI", 6}, {"VII", 7}, {"VIII", 8},
			{"IX", 9}
		};
		symbols<int> const tens =
		{
			{"X", 10}, {"XX", 20}, {"XXX", 30}, {"XL", 40},
			{"L", 50}, {"LX", 60}, {"LXX", 70}, {"LXXX", 80},
			{"XC", 90}
		};
		symbols<int> const hundreds
		{
			{"C", 100}, {"CC", 200}, {"CCC", 300}, {"CD", 400},
			{"D", 500}, {"DC", 600}, {"DCC", 700}, {"DCCC", 800},
			{"CM", 900}
		};

		auto number = -hundreds >> -tens >> -ones;

		roman r;
		_ASSERT((test_attr("CDXLII", number, r)));
		_ASSERT(eval(r) == 442);
	}

	{ // construction from initializer-list without attribute
		symbols<> foo = {"a1", "a2", "a3"};

		_ASSERT((test("a3", foo)));
	}

	{ // assignment from initializer-list
		symbols<> foo;
		foo = {"a1", "a2", "a3"};

		_ASSERT((test("a3", foo)));
	}

	{ // unicode | construction from initializer-list
		using namespace boost::spirit;
		x3::symbols_parser<char_encoding::unicode, int> foo = {{U"a1", 1}, {U"a2", 2}, {U"a3", 3}};

		int r=0;
		_ASSERT((test_attr(U"a3", foo, r)));
		_ASSERT(r == 3);
	}
}
