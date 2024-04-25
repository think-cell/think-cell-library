/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

#include <string>
#include <iostream>

UNITTESTDEF(x3_test_difference)
{
	using boost::spirit::x3::ascii::char_;
	using boost::spirit::x3::ascii::space;
	using boost::spirit::x3::lit;
	using spirit_test::test;
	using spirit_test::test_attr;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(char_ - lit('a'));

	// Basic tests
	{
		_ASSERT(test("b", char_ - lit('a')));
		_ASSERT(!test("a", char_ - lit('a')));
		_ASSERT(test("/* abcdefghijk */", lit("/*") >> *(char_ - lit("*/")) >> lit("*/")));
		_ASSERT(!test("switch", lit("switch") - lit("switch")));
	}

	// Test attributes
	{
		char attr='\0';
		_ASSERT(test_attr("xg", (char_ - lit('g')) >> lit('g'), attr));
		_ASSERT(attr == 'x');
	}

	// Test handling of container attributes
	{
		std::string attr;
		_ASSERT(test_attr("abcdefg", *(char_ - lit('g')) >> lit('g'), attr));
		_ASSERT(attr == "abcdef");
	}

	{
		using boost::spirit::x3::_attr;

		std::string s;

		_ASSERT(test(
			"/*abcdefghijk*/"
		  , lit("/*") >> *(char_ - lit("*/"))[([&](auto& ctx){ s += _attr(ctx); })] >> lit("*/")
		));
		_ASSERT(s == "abcdefghijk");
		s.clear();

		_ASSERT(test(
			"    /*abcdefghijk*/"
		  , lit("/*") >> *(char_ - lit("*/"))[([&](auto& ctx){ s += _attr(ctx); })] >> lit("*/")
		  , space
		));
		_ASSERT(s == "abcdefghijk");
	}
}
