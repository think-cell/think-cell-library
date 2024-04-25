/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2013 Agustin Berge

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <iostream>

UNITTESTDEF(x3_test_skip)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::ascii::space;
	using boost::spirit::x3::ascii::space_type;
	using boost::spirit::x3::ascii::char_;
	using boost::spirit::x3::ascii::alpha;
	using boost::spirit::x3::lexeme;
	using boost::spirit::x3::skip;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(skip('x')[lit('y')]);

	{
		_ASSERT((test("a b c d", skip(space)[*char_])));
	}

	{ // test attribute
		std::string s;
		_ASSERT((test_attr("a b c d", skip(space)[*char_], s)));
		_ASSERT(s == "abcd");
	}

	{ // reskip
		_ASSERT((test("ab c d", lexeme[lit('a') >> lit('b') >> skip[lit('c') >> lit('d')]], space)));
		_ASSERT((test("abcd", lexeme[lit('a') >> lit('b') >> skip[lit('c') >> lit('d')]], space)));
		_ASSERT(!(test("a bcd", lexeme[lit('a') >> lit('b') >> skip[lit('c') >> lit('d')]], space)));

		_ASSERT((test("ab c d", lexeme[lexeme[lit('a') >> lit('b') >> skip[lit('c') >> lit('d')]]], space)));
		_ASSERT((test("abcd", lexeme[lexeme[lit('a') >> lit('b') >> skip[lit('c') >> lit('d')]]], space)));
		_ASSERT(!(test("a bcd", lexeme[lexeme[lit('a') >> lit('b') >> skip[lit('c') >> lit('d')]]], space)));
	}
}
