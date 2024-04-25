/*=============================================================================
	Copyright (c) 2001-2011 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include "test.hpp"
#include "utils.hpp"
#include "../x3.hpp"
#include <boost/utility/enable_if.hpp>
#include <string>
#include <iostream>
#include <string>
#include <vector>

#if defined(__GNUC__) && (__GNUC__ >= 8)
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=92539
# pragma GCC diagnostic ignored "-Warray-bounds"
#endif

UNITTESTDEF(x3_test_repeat)
{
	using spirit_test::test_attr;
	using spirit_test::test;

	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::repeat;
	using boost::spirit::x3::inf;
	using boost::spirit::x3::omit;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::lexeme;
	using boost::spirit::x3::char_;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(repeat[lit('x')]);
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(repeat(3)[lit('x')]);
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(repeat(3, 5)[lit('x')]);
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(repeat(3, inf)[lit('x')]);

	{
		_ASSERT(test("aaaaaaaa", repeat[char_])); // kleene synonym
		_ASSERT(test("aaaaaaaa", repeat(8)[char_]));
		_ASSERT(!test("aa", repeat(3)[char_]));
		_ASSERT(test("aaa", repeat(3, 5)[char_]));
		_ASSERT(test("aaaaa", repeat(3, 5)[char_]));
		_ASSERT(!test("aaaaaa", repeat(3, 5)[char_]));
		_ASSERT(!test("aa", repeat(3, 5)[char_]));

		_ASSERT(test("aaa", repeat(3, inf)[char_]));
		_ASSERT(test("aaaaa", repeat(3, inf)[char_]));
		_ASSERT(test("aaaaaa", repeat(3, inf)[char_]));
		_ASSERT(!test("aa", repeat(3, inf)[char_]));
	}
	{
		std::string s;
		_ASSERT(test_attr("aaaaaaaa", repeat[char_ >> char_], s)); // kleene synonym
		_ASSERT(s == "aaaaaaaa");

		s.clear();
		_ASSERT(test_attr("aaaaaaaa", repeat(4)[char_ >> char_], s));
		_ASSERT(s == "aaaaaaaa");

		_ASSERT(!test("aa", repeat(3)[char_ >> char_]));
		_ASSERT(!test("a", repeat(1)[char_ >> char_]));

		s.clear();
		_ASSERT(test_attr("aa", repeat(1, 3)[char_ >> char_], s));
		_ASSERT(s == "aa");

		s.clear();
		_ASSERT(test_attr("aaaaaa", repeat(1, 3)[char_ >> char_], s));
		_ASSERT(s == "aaaaaa");

		_ASSERT(!test("aaaaaaa", repeat(1, 3)[char_ >> char_]));
		_ASSERT(!test("a", repeat(1, 3)[char_ >> char_]));

		s.clear();
		_ASSERT(test_attr("aaaa", repeat(2, inf)[char_ >> char_], s));
		_ASSERT(s == "aaaa");

		s.clear();
		_ASSERT(test_attr("aaaaaa", repeat(2, inf)[char_ >> char_], s));
		_ASSERT(s == "aaaaaa");

		_ASSERT(!test("aa", repeat(2, inf)[char_ >> char_]));
	}

	{ // from classic spirit tests
		_ASSERT(test("", repeat(0, inf)[lit('x')]));

		//  repeat exact 8
		#define rep8 repeat(8)[alpha] >> lit('X')
		_ASSERT(!test("abcdefgX", rep8, false));
		_ASSERT(test("abcdefghX", rep8));
		_ASSERT(!test("abcdefghiX", rep8, false));
		_ASSERT(!test("abcdefgX", rep8, false));
		_ASSERT(!test("aX", rep8, false));

		//  repeat 2 to 8
		#define rep28 repeat(2, 8)[alpha] >> lit('*')
		_ASSERT(test("abcdefg*", rep28));
		_ASSERT(test("abcdefgh*", rep28));
		_ASSERT(!test("abcdefghi*", rep28, false));
		_ASSERT(!test("a*", rep28, false));

		//  repeat 2 or more
		#define rep2_ repeat(2, inf)[alpha] >> lit('+')
		_ASSERT(test("abcdefg+", rep2_));
		_ASSERT(test("abcdefgh+", rep2_));
		_ASSERT(test("abcdefghi+", rep2_));
		_ASSERT(test("abcdefg+", rep2_));
		_ASSERT(!test("a+", rep2_, false));

		//  repeat 0
		#define rep0 repeat(0)[alpha] >> lit('/')
		_ASSERT(test("/", rep0));
		_ASSERT(!test("a/", rep0, false));

		//  repeat 0 or 1
		#define rep01 repeat(0, 1)[alpha >> digit] >> lit('?')
		_ASSERT(!test("abcdefg?", rep01, false));
		_ASSERT(!test("a?", rep01, false));
		_ASSERT(!test("1?", rep01, false));
		_ASSERT(!test("11?", rep01, false));
		_ASSERT(!test("aa?", rep01, false));
		_ASSERT(test("?", rep01));
		_ASSERT(test("a1?", rep01));
	}

	{
		_ASSERT(test(" a a aaa aa", repeat(7)[char_], space));
		_ASSERT(test("12345 678 9", repeat(9)[digit], space));
	}

	{
		std::vector<std::string> v;
		_ASSERT(test_attr("a b c d", repeat(4)[lexeme[+alpha]], v, space) && 4 == v.size() &&
			v[0] == "a" && v[1] == "b" && v[2] == "c" &&  v[3] == "d");
	}
	{
		_ASSERT(test("1 2 3", int_ >> repeat(2)[int_], space));
		_ASSERT(!test("1 2", int_ >> repeat(2)[int_], space));
	}

	{
		std::vector<int> v;
		_ASSERT(test_attr("1 2 3", int_ >> repeat(2)[int_], v, space));
		_ASSERT(v.size() == 3 && v[0] == 1 && v[1] == 2 && v[2] == 3);

		_ASSERT(!test("1 2", int_ >> repeat(2)[int_], space));
	}

	{ // test move only types
		std::vector<move_only> v;
		_ASSERT(test_attr("sss", repeat(3)[synth_move_only], v));
		_ASSERTEQUAL(v.size(), 3);
	}
}
