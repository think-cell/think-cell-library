/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2010 Hartmut Kaiser

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <iostream>

UNITTESTDEF(x3_test_matches)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::matches;
	using boost::spirit::x3::char_;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(matches[lit('x')]);

	{
		_ASSERT(test("x", matches[char_]));
		bool result = false;
		_ASSERT(test_attr("x", matches[char_], result) && result);
	}

	{
		_ASSERT(!test("y", matches[char_('x')]));
		_ASSERT(!test("y", matches[lit('x')]));
		bool result = true;
		_ASSERT(test_attr("y", matches[char_('x')], result, false) && !result);
		result = true;
		_ASSERT(test_attr("y", matches[lit('x')], result, false) && !result);
	}
}
