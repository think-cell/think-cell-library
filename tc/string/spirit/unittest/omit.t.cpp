/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/at.hpp>

#include <string>
#include <iostream>

using boost::spirit::x3::rule;

namespace {
	rule<class direct_rule, int> direct_rule = "direct_rule";
	rule<class indirect_rule, int> indirect_rule = "indirect_rule";

	auto const direct_rule_def = boost::spirit::x3::int_;
	auto const indirect_rule_def = direct_rule;

	BOOST_SPIRIT_DEFINE(direct_rule, indirect_rule)
}

UNITTESTDEF(x3_test_omit)
{
	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::omit;
	using boost::spirit::x3::unused_type;
	using boost::spirit::x3::unused;
	using boost::spirit::x3::int_;

	using boost::fusion::vector;
	using boost::fusion::at_c;

	using spirit_test::test;
	using spirit_test::test_attr;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(omit[lit('x')]);

	{
		_ASSERT(test("a", omit[lit('a')]));
	}

	{
		// omit[] means we don't receive the attribute
		char attr=char();
		_ASSERT((test_attr("abc", omit[char_] >> omit[lit('b')] >> char_, attr)));
		_ASSERT((attr == 'c'));
	}

	{
		// If all elements except 1 is omitted, the attribute is
		// a single-element sequence. For this case alone, we allow
		// naked attributes (unwrapped in a fusion sequence).
		char attr=char();
		_ASSERT((test_attr("abc", omit[char_] >> lit('b') >> char_, attr)));
		_ASSERT((attr == 'c'));
	}

	{
		// omit[] means we don't receive the attribute
		vector<> attr;
		_ASSERT((test_attr("abc", omit[char_] >> omit[lit('b')] >> omit[char_], attr)));
	}

	{
		// omit[] means we don't receive the attribute
		// this test is merely a compile test, because using a unused as the
		// explicit attribute doesn't make any sense
		unused_type attr;
		_ASSERT((test_attr("abc", omit[char_ >> lit('b') >> char_], attr)));
	}

	{
		// omit[] means we don't receive the attribute, if all elements of a
		// sequence have unused attributes, the whole sequence has an unused
		// attribute as well
		vector<char, char> attr;
		_ASSERT((test_attr("abcde",
			char_ >> (omit[char_] >> omit[lit('c')] >> omit[char_]) >> char_, attr)));
		_ASSERT((at_c<0>(attr) == 'a'));
		_ASSERT((at_c<1>(attr) == 'e'));
	}

	{
		// "hello" has an unused_type. unused attrubutes are not part of the sequence
		vector<char, char> attr;
		_ASSERT((test_attr("a hello c", char_ >> lit("hello") >> char_, attr, space)));
		_ASSERT((at_c<0>(attr) == 'a'));
		_ASSERT((at_c<1>(attr) == 'c'));
	}

	{
		// if only one node in a sequence is left (all the others are omitted),
		// then we need "naked" attributes (not wrapped in a tuple)
		int attr=0;
		_ASSERT((test_attr("a 123 c", omit[lit('a')] >> int_ >> omit[lit('c')], attr, space)));
		_ASSERT((attr == 123));
	}

	{
		// unused means we don't care about the attribute
		_ASSERT((test_attr("abc", char_ >> lit('b') >> char_, unused)));
	}

	{   // test action with omitted attribute
		char c = 0;
		auto f = [&](auto& ctx){ c = _attr(ctx); };

		_ASSERT(test("x123\"a string\"", (char_ >> omit[int_] >> lit("\"a string\""))[f]));
		_ASSERT(c == 'x');
	}

	{   // test action with omitted attribute
		int n = 0;
		auto f = [&](auto& ctx){ n = _attr(ctx); };

		_ASSERT(test("x 123 \"a string\"", (omit[char_] >> int_ >> lit("\"a string\""))[f], space));
		_ASSERT(n == 123);
	}

	{
		// test with simple rule
		_ASSERT((test_attr("123", omit[direct_rule], unused)));
	}

	{
		// test with complex rule
		_ASSERT((test_attr("123", omit[indirect_rule], unused)));
	}
}
