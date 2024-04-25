/*=============================================================================
	Copyright (c) 2001-2013 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "utils.hpp"
#include "../x3.hpp"
#include <string>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <iostream>

using namespace spirit_test;

UNITTESTDEF(x3_test_list)
{
	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(char_ % lit(','));

	{
		_ASSERT(test("a,b,c,d,e,f,g,h", char_ % lit(',')));
		_ASSERT(test("a,b,c,d,e,f,g,h,", char_ % lit(','), false));
	}

	{
		_ASSERT(test("a, b, c, d, e, f, g, h", char_ % lit(','), space));
		_ASSERT(test("a, b, c, d, e, f, g, h,", char_ % lit(','), space, false));
	}

	{
		std::string s;
		_ASSERT(test_attr("a,b,c,d,e,f,g,h", char_ % lit(','), s));
		_ASSERT(s == "abcdefgh");

		_ASSERT(!test("a,b,c,d,e,f,g,h,", char_ % lit(',')));
	}

	{
		std::string s;
		_ASSERT(test_attr("ab,cd,ef,gh", (char_ >> char_) % lit(','), s));
		_ASSERT(s == "abcdefgh");

		_ASSERT(!test("ab,cd,ef,gh,", (char_ >> char_) % lit(',')));
		_ASSERT(!test("ab,cd,ef,g", (char_ >> char_) % lit(',')));

		s.clear();
		_ASSERT(test_attr("ab,cd,efg", (char_ >> char_) % lit(',') >> char_, s));
		_ASSERT(s == "abcdefg");
	}

	{ // regression test for has_attribute
		using boost::spirit::x3::int_;
		using boost::spirit::x3::omit;

		int i;
		_ASSERT(test_attr("1:2,3", int_ >> lit(':') >> omit[int_] % lit(','), i));
		_ASSERTEQUAL(i, 1);
	}

	{
		using boost::spirit::x3::int_;

		std::vector<int> v;
		_ASSERT(test_attr("1,2", int_ % lit(','), v));
		_ASSERT(2 == v.size() && 1 == v[0] && 2 == v[1]);
	}

	{
		using boost::spirit::x3::int_;

		std::vector<int> v;
		_ASSERT(test_attr("(1,2)", lit('(') >> int_ % lit(',') >> lit(')'), v));
		_ASSERT(2 == v.size() && 1 == v[0] && 2 == v[1]);
	}

	{
		std::vector<std::string> v;
		_ASSERT(test_attr("a,b,c,d", +alpha % lit(','), v));
		_ASSERT(4 == v.size() && "a" == v[0] && "b" == v[1]
			&& "c" == v[2] && "d" == v[3]);
	}

	{
		std::vector<std::optional<char> > v;
		_ASSERT(test_attr("#a,#", (lit('#') >> -alpha) % lit(','), v));
		_ASSERT(2 == v.size() &&
			!!v[0] && 'a' == *v[0] && !v[1]);

		std::vector<char> v2;
		_ASSERT(test_attr("#a,#", (lit('#') >> -alpha) % lit(','), v2));
		_ASSERT(1 == v2.size() && 'a' == v2[0]);
	}

	{ // actions
		using boost::spirit::x3::_attr;

		std::string s;
		auto f = [&](auto& ctx){ s = std::string(_attr(ctx).begin(), _attr(ctx).end()); };

		_ASSERT(test("a,b,c,d,e,f,g,h", (char_ % lit(','))[f]));
		_ASSERT(s == "abcdefgh");
	}

	{ // test move only types
		std::vector<move_only> v;
		_ASSERT(test_attr("s.s.s.s", synth_move_only % lit('.'), v));
		_ASSERTEQUAL(v.size(), 4);
	}
}
