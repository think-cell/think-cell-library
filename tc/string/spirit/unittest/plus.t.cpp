/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "utils.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/vector.hpp>
#include <string>
#include <vector>
#include <string>
#include <iostream>


struct x_attr
{
};

namespace boost { namespace spirit { namespace x3 { namespace traits
{
	template <>
	struct container_value<x_attr>
	{
		typedef char type; // value type of container
	};

	template <>
	struct push_back_container<x_attr>
	{
		static bool call(x_attr& /*c*/, char /*val*/)
		{
			// push back value type into container
			return true;
		}
	};
}}}}

UNITTESTDEF(x3_test_plus)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::char_;
	using boost::spirit::x3::alpha;
	using boost::spirit::x3::upper;
	using boost::spirit::x3::space;
	using boost::spirit::x3::digit;
	//~ using boost::spirit::x3::no_case;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::omit;
	using boost::spirit::x3::lit;
	//~ using boost::spirit::x3::_1;
	using boost::spirit::x3::lexeme;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(+char_);

	{
		_ASSERT(test("aaaaaaaa", +char_));
		_ASSERT(test("a", +char_));
		_ASSERT(!test("", +char_));
		_ASSERT(test("aaaaaaaa", +alpha));
		_ASSERT(!test("aaaaaaaa", +upper));
	}

	{
		_ASSERT(test(" a a aaa aa", +char_, space));
		_ASSERT(test("12345 678 9 ", +digit, space));
	}

	//~ {
		//~ _ASSERT(test("aBcdeFGH", no_case[+char_]));
		//~ _ASSERT(test("a B cde FGH  ", no_case[+char_], space));
	//~ }

	{
		std::vector<int> v;
		_ASSERT(test_attr("123 456 789 10", +int_, v, space) && 4 == v.size() &&
			v[0] == 123 && v[1] == 456 && v[2] == 789 &&  v[3] == 10);
	}

	{
		std::vector<std::string> v;
		_ASSERT(test_attr("a b c d", +lexeme[+alpha], v, space) && 4 == v.size() &&
			v[0] == "a" && v[1] == "b" && v[2] == "c" &&  v[3] == "d");
	}

	{
		_ASSERT(test("Kim Kim Kim", +lit("Kim"), space));
	}

	// $$$ Fixme $$$
	/*{
		// The following 2 tests show that omit does not inhibit explicit attributes

		std::string s;
		_ASSERT(test_attr("bbbb", omit[+char_('b')], s) && s == "bbbb");

		s.clear();
		_ASSERT(test_attr("b b b b ", omit[+char_('b')], s, space) && s == "bbbb");
	}*/

	{ // actions
		std::string v;
		auto f = [&](auto& ctx){ v = _attr(ctx); };

		_ASSERT(test("bbbb", (+char_)[f]) && 4 == v.size() &&
			v[0] == 'b' && v[1] == 'b' && v[2] == 'b' &&  v[3] == 'b');
	}

	{ // more actions
		std::vector<int> v;
		auto f = [&](auto& ctx){ v = _attr(ctx); };

		_ASSERT(test("1 2 3", (+int_)[f], space) && 3 == v.size() &&
			v[0] == 1 && v[1] == 2 && v[2] == 3);
	}

	{ // attribute customization

		x_attr x;
		test_attr("abcde", +char_, x);
	}

	// single-element fusion vector tests
	{
		boost::fusion::vector<std::string> fs;
		_ASSERT((test_attr("12345", +char_, fs))); // ok
		_ASSERT(boost::fusion::at_c<0>(fs) == "12345");
	}

	{ // test move only types
		std::vector<move_only> v;
		_ASSERT(test_attr("sss", +synth_move_only, v));
		_ASSERTEQUAL(v.size(), 3);
	}
}
