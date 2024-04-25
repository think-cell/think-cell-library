/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <string>

UNITTESTDEF(x3_test_lit)
{
	using spirit_test::test_attr;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::char_;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(lit("x"));

	{
		std::string attr;
		auto p = char_ >> lit("\n");
		_ASSERT(test_attr("A\n", p, attr));
		_ASSERT(attr == "A");
	}

	{
		using namespace boost::spirit::x3::ascii;
		std::string attr;
		auto p = char_ >> lit("\n");
		_ASSERT(test_attr("A\n", p, attr));
		_ASSERT(attr == "A");
	}
}
