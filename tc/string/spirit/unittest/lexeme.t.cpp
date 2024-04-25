/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

#include <iostream>

UNITTESTDEF(x3_test_lexeme)
{
	using spirit_test::test;
	using boost::spirit::x3::ascii::space;
	using boost::spirit::x3::ascii::space_type;
	using boost::spirit::x3::ascii::digit;
	using boost::spirit::x3::lexeme;
	using boost::spirit::x3::rule;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(lexeme[lit('x')]);

	{
		_ASSERT((test(" 1 2 3 4 5", +digit, space)));
		_ASSERT((!test(" 1 2 3 4 5", lexeme[+digit], space)));
		_ASSERT((test(" 12345", lexeme[+digit], space)));
		_ASSERT((test(" 12345  ", lexeme[+digit], space, false)));

		// lexeme collapsing
		_ASSERT((!test(" 1 2 3 4 5", lexeme[lexeme[+digit]], space)));
		_ASSERT((test(" 12345", lexeme[lexeme[+digit]], space)));
		_ASSERT((test(" 12345  ", lexeme[lexeme[+digit]], space, false)));

		auto r = +digit;
		auto rr = lexeme[r];

		_ASSERT((!test(" 1 2 3 4 5", rr, space)));
		_ASSERT((test(" 12345", rr, space)));
	}
}
