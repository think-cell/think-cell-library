/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2013 Agustin Berge

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <iostream>

UNITTESTDEF(x3_test_no_skip)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::ascii::space;
	using boost::spirit::x3::ascii::space_type;
	using boost::spirit::x3::ascii::char_;
	using boost::spirit::x3::lexeme;
	using boost::spirit::x3::no_skip;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(no_skip[lit('x')]);

	// without skipping no_skip is equivalent to lexeme
	{
		std::string str;
		_ASSERT((test_attr("'  abc '", lit('\'') >> no_skip[+~char_('\'')] >> lit('\''), str)));
		_ASSERT(str == "  abc ");
	}
	{
		std::string str;
		_ASSERT((test_attr("'  abc '", lit('\'') >> lexeme[+~char_('\'')] >> lit('\''), str)));
		_ASSERT(str == "  abc ");
	}

	// with skipping, no_skip allows to match a leading skipper
	{
		std::string str;
		_ASSERT((test_attr("'  abc '", lit('\'') >> no_skip[+~char_('\'')] >> lit('\''), str, space)));
		_ASSERT(str == "  abc ");
	}
	{
		std::string str;
		_ASSERT((test_attr("'  abc '", lit('\'') >> lexeme[+~char_('\'')] >> lit('\''), str, space)));
		_ASSERT(str == "abc ");
	}
}
