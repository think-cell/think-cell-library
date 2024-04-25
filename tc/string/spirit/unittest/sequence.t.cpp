/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "utils.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/deque.hpp>
#include <boost/fusion/include/at.hpp>
#include <boost/fusion/include/comparison.hpp>
#include <string>
#include <iostream>

UNITTESTDEF(x3_test_sequence)
{
	using boost::spirit::x3::unused_type;

	using boost::spirit::x3::char_;
	using boost::spirit::x3::space;
	using boost::spirit::x3::string;
	using boost::spirit::x3::attr;
	using boost::spirit::x3::omit;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::unused;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::float_;
	using boost::spirit::x3::no_case;
	using boost::spirit::x3::rule;
	using boost::spirit::x3::alnum;

	using boost::spirit::x3::traits::attribute_of;

	using boost::fusion::vector;
	using boost::fusion::deque;
	using boost::fusion::at_c;

	using spirit_test::test;
	using spirit_test::test_attr;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(char_ >> char_);

	{
		_ASSERT((test("aa", char_ >> char_)));
		_ASSERT((test("aa", char_ >> lit('a'))));
		_ASSERT((test("aaa", char_ >> char_ >> char_('a'))));
		_ASSERT((test("xi", char_('x') >> char_('i'))));
		_ASSERT((!test("xi", char_('x') >> char_('o'))));
		_ASSERT((test("xin", char_('x') >> char_('i') >> char_('n'))));
	}

#ifdef BOOST_SPIRIT_COMPILE_ERROR_CHECK
	{
		// Compile check only
		struct x {};
		char_ >> x(); // this should give a reasonable error message
	}
#endif

	{
		_ASSERT((test(" a a", char_ >> char_, space)));
		_ASSERT((test(" x i", char_('x') >> char_('i'), space)));
		_ASSERT((!test(" x i", char_('x') >> char_('o'), space)));
	}


	{
		_ASSERT((test(" Hello, World", lit("Hello") >> lit(',') >> lit("World"), space)));
	}


	{
		vector<char, char> attr_;
		_ASSERT((test_attr("ab", char_ >> char_, attr_)));
		_ASSERT((at_c<0>(attr_) == 'a'));
		_ASSERT((at_c<1>(attr_) == 'b'));
	}

#ifdef BOOST_SPIRIT_COMPILE_ERROR_CHECK
	{
		// Compile check only
		vector<char, char> attr_;

		// error: attr does not have enough elements
		test_attr("abc", char_ >> char_ >> char_, attr_);
	}
#endif

	{
		vector<char, char, char> attr_;
		_ASSERT((test_attr(" a\n  b\n  c", char_ >> char_ >> char_, attr_, space)));
		_ASSERT((at_c<0>(attr_) == 'a'));
		_ASSERT((at_c<1>(attr_) == 'b'));
		_ASSERT((at_c<2>(attr_) == 'c'));
	}

	{
		// 'b' has an unused_type. unused attributes are not part of the sequence
		vector<char, char> attr_;
		_ASSERT((test_attr("abc", char_ >> lit('b') >> char_, attr_)));
		_ASSERT((at_c<0>(attr_) == 'a'));
		_ASSERT((at_c<1>(attr_) == 'c'));
	}

	{
		// 'b' has an unused_type. unused attributes are not part of the sequence
		vector<char, char> attr_;
		_ASSERT((test_attr("acb", char_ >> char_ >> lit('b'), attr_)));
		_ASSERT((at_c<0>(attr_) == 'a'));
		_ASSERT((at_c<1>(attr_) == 'c'));
	}

	{
		// "hello" has an unused_type. unused attributes are not part of the sequence
		vector<char, char> attr_;
		_ASSERT((test_attr("a hello c", char_ >> lit("hello") >> char_, attr_, space)));
		_ASSERT((at_c<0>(attr_) == 'a'));
		_ASSERT((at_c<1>(attr_) == 'c'));
	}

	{
		// a single element
		char attr_=0;
		_ASSERT((test_attr("ab", char_ >> lit('b'), attr_)));
		_ASSERT((attr_ == 'a'));
	}

	{
		// a single element fusion sequence
		vector<char> attr_;
		_ASSERT((test_attr("ab", char_ >> lit('b'), attr_)));
		_ASSERT((at_c<0>(attr_) == 'a'));
	}

	{
		// make sure single element tuples get passed through if the rhs
		// has a single element tuple as its attribute. Edit JDG 2014:
		// actually he issue here is that if the rhs in this case a rule
		// (r), it should get it (i.e. the sequence parser should not
		// unwrap it). It's odd that the RHS (r) does not really have a
		// single element tuple (it's a deque<char, int>), so the original
		// comment is not accurate.

		typedef deque<char, int> attr_type;
		attr_type fv;

		auto r = rule<class r_id, attr_type>()
			= char_ >> lit(',') >> int_;

		_ASSERT((test_attr("test:x,1", lit("test:") >> r, fv) &&
			fv == attr_type('x', 1)));
	}

	{
		// make sure single element tuples get passed through if the rhs
		// has a single element tuple as its attribute. This is a correction
		// of the test above.

		typedef deque<int> attr_type;
		attr_type fv;

		auto r = rule<class r_id, attr_type>()
			= int_;

		_ASSERT((test_attr("test:1", lit("test:") >> r, fv) &&
			fv == attr_type(1)));
	}

	{
		// unused means we don't care about the attribute
		_ASSERT((test_attr("abc", char_ >> lit('b') >> char_, unused)));
	}

	{
		_ASSERT((test("aA", no_case[char_('a') >> lit('a')])));
		_ASSERT((test("BEGIN END", no_case[lit("begin") >> lit("end")], space)));
		_ASSERT((!test("BEGIN END", no_case[lit("begin") >> lit("nend")], space)));
	}

	{ // check attribute is passed through unary to another sequence
		using boost::spirit::x3::eps;
		std::string s;
		_ASSERT(test_attr("ab", eps >> no_case[char_ >> char_], s));
		_ASSERT("ab" == s);
		s.clear();
		_ASSERT(test_attr("ab", no_case[char_ >> char_] >> eps, s));
		_ASSERT("ab" == s);
		s.clear();
		_ASSERT(test_attr("abc", char_ >> no_case[char_ >> char_], s));
		_ASSERT("abc" == s);
		s.clear();
		_ASSERT(test_attr("abc", no_case[char_ >> char_] >> char_, s));
		_ASSERT("abc" == s);
	}

	{
#ifdef SPIRIT_NO_COMPILE_CHECK
		char_ >> char_ = char_ >> char_; // disallow this!
#endif
	}

	{ // alternative forms of attributes. Allow sequences to take in
	  // stl containers.

		std::vector<char> v;
		_ASSERT(test_attr("abc", char_ >> char_ >> char_, v));
		_ASSERT(v.size() == 3);
		_ASSERT(v[0] == 'a');
		_ASSERT(v[1] == 'b');
		_ASSERT(v[2] == 'c');
	}

	{ // alternative forms of attributes. Allow sequences to take in
	  // stl containers.

		std::vector<char> v;
		_ASSERT(test_attr("a,b,c", char_ >> *(lit(',') >> char_), v));
		_ASSERT(v.size() == 3);
		_ASSERT(v[0] == 'a');
		_ASSERT(v[1] == 'b');
		_ASSERT(v[2] == 'c');
	}

	{ // alternative forms of attributes. Allow sequences to take in
	  // stl containers.

		std::vector<char> v;
		_ASSERT(test_attr("abc", char_ >> *char_, v));
		_ASSERT(v.size() == 3);
		_ASSERT(v[0] == 'a');
		_ASSERT(v[1] == 'b');
		_ASSERT(v[2] == 'c');
	}

	{ // alternative forms of attributes. Allow sequences to take in
	  // stl containers.
		//~ using boost::spirit::x3::hold;

		std::vector<char> v;
		_ASSERT(test_attr("abc", char_ >> *(char_ >> char_), v));
		_ASSERT(v.size() == 3);
		_ASSERT(v[0] == 'a');
		_ASSERT(v[1] == 'b');
		_ASSERT(v[2] == 'c');

		v.clear();
		_ASSERT(!test_attr("abcd", char_ >> *(char_ >> char_), v));

		// $$$ hold not yet implemented $$$
		//~ v.clear();
		//~ _ASSERT(test_attr("abcdef", char_ >> *hold[char_ >> char_] >> char_, v));
		//~ _ASSERT(v.size() == 6);
		//~ _ASSERT(v[0] == 'a');
		//~ _ASSERT(v[1] == 'b');
		//~ _ASSERT(v[2] == 'c');
		//~ _ASSERT(v[3] == 'd');
		//~ _ASSERT(v[4] == 'e');
		//~ _ASSERT(v[5] == 'f');

		v.clear();
		_ASSERT(test_attr("abc", char_ >> +(char_ >> char_), v));
		_ASSERT(v.size() == 3);
		_ASSERT(v[0] == 'a');
		_ASSERT(v[1] == 'b');
		_ASSERT(v[2] == 'c');
	}

	{ // alternative forms of attributes. Allow sequences to take in
	  // stl containers.

		std::vector<char> v;
		_ASSERT(test_attr("abc", char_ >> -(+char_), v));
		_ASSERT(v.size() == 3);
		_ASSERT(v[0] == 'a');
		_ASSERT(v[1] == 'b');
		_ASSERT(v[2] == 'c');
	}

	{ // alternative forms of attributes. Allow sequences to take in
	  // stl containers.

		std::string s;
		_ASSERT(test_attr("foobar", string("foo") >> string("bar"), s));
		_ASSERT(s == "foobar");

		s.clear();

		// $$$ hold not yet implemented $$$
		//~ using boost::spirit::x3::hold;

				//~ rule<char const*, std::string()> word = +char_("abc");
				//~ _ASSERT(test_attr("ab.bc.ca", *hold[word >> string(".")] >> word, s));
				//~ _ASSERT(s == "ab.bc.ca");
	}

	// Make sure get_sequence_types works for sequences of sequences.
	{
		std::vector<char> v;
		_ASSERT(test_attr(" a b", (lit(' ') >> char_) >> (lit(' ') >> char_), v));
		_ASSERT(v.size() == 2);
		_ASSERT(v[0] == 'a');
		_ASSERT(v[1] == 'b');
	}

	// alternative forms of attributes. Allow sequences to take in
	// stl containers of stl containers.
	{
		std::vector<std::string> v;
		_ASSERT(test_attr("abc1,abc2",
			*~char_(',') >> *(lit(',') >> *~char_(',')), v));
		_ASSERT(v.size() == 2 && v[0] == "abc1" && v[1] == "abc2");
	}

	{
		std::vector<std::string> v;

		auto e = rule<class e_id, std::string>()
			= *~char_(',');

		auto l = rule<class l_id, std::vector<std::string>>()
			= e >> *(lit(',') >> e);

		_ASSERT(test_attr("abc1,abc2,abc3", l, v));
		_ASSERT(v.size() == 3);
		_ASSERT(v[0] == "abc1");
		_ASSERT(v[1] == "abc2");
		_ASSERT(v[2] == "abc3");
	}

	// do the same with a plain string object
	{
		std::string s;
		_ASSERT(test_attr("abc1,abc2",
			*~char_(',') >> *(lit(',') >> *~char_(',')), s));
		_ASSERT(s == "abc1abc2");
	}

	{
		std::string s;
		auto e = rule<class e_id, std::string>()
			= *~char_(',');

		auto l = rule<class l_id, std::string>()
			= e >> *(lit(',') >> e);

		_ASSERT(test_attr("abc1,abc2,abc3", l, s));
		_ASSERT(s == "abc1abc2abc3");
	}

	{
		std::vector<char> v;
		_ASSERT(test_attr("ab", char_ >> -char_, v));
		_ASSERT(v.size() == 2 && v[0] == 'a' && v[1] == 'b');

		v.clear();
		_ASSERT(test_attr("a", char_ >> -char_, v));
		_ASSERT(v.size() == 1 && v[0] == 'a');

		// $$$ should this be allowed? I don't think so... $$$
		//~ v.clear();
		//~ _ASSERT(test_attr("a", char_, v));
		//~ _ASSERT(v.size() == 1 && v[0] == 'a');
	}

	{
		std::vector<std::optional<char>> v;
		_ASSERT(test_attr("ab", char_ >> -char_, v));
		_ASSERT(v.size() == 2 && v[0] == 'a' && v[1] == 'b');

		v.clear();
		_ASSERT(test_attr("a", char_ >> -char_, v));
		_ASSERT(v.size() == 2 && v[0] == 'a' && !v[1]);

		// $$$ should this be allowed? I don't think so... $$$
		//~ v.clear();
		//~ _ASSERT(test_attr("a", char_, v));
		//~ _ASSERT(v.size() == 1 && v[0] == 'a');
	}

	// test from spirit mailing list
	// "Error with container within sequence"
	{
		typedef vector<std::string> attr_type;
		attr_type vecstr;

		auto r = *alnum;

		_ASSERT(test_attr("abcdef", r, vecstr));
		_ASSERT(at_c<0>(vecstr) == "abcdef");
	}

	// test from spirit mailing list (variation of above)
	// "Error with container within sequence"
	{
		typedef vector<std::vector<int>> attr_type;
		attr_type vecvecn;

		auto r = *int_;

		_ASSERT(test_attr("123 456", r, vecvecn, space));
		_ASSERT(at_c<0>(vecvecn).size() == 2);
		_ASSERT(at_c<0>(vecvecn)[0] == 123);
		_ASSERT(at_c<0>(vecvecn)[1] == 456);
	}

	{ // non-flat optional
		vector<int, std::optional<vector<int, int>>> v;
		auto const p = int_ >> -(lit(':') >> int_ >> lit('-') >> int_);
		_ASSERT(test_attr("1:2-3", p, v));
		_ASSERT(at_c<1>(v));
		_ASSERTEQUAL(at_c<0>(*at_c<1>(v)), 2);
	}

	{ // optional with container attribute
		vector<char, std::optional<std::string>> v;
		auto const p = char_ >> -(lit(':') >> +char_);
		_ASSERT(test_attr("x", p, v));
		_ASSERT(!at_c<1>(v));
		v = {};
		_ASSERT(test_attr("x:abc", p, v));
		_ASSERT(at_c<1>(v));
		_ASSERT(*at_c<1>(v) == "abc");
	}

	{
		using Attr = boost::variant<int, float>;
		Attr varnf;
		auto const term = rule<class term_id, Attr>("term") = int_ | float_;
		auto const expr = rule<class expr_id, Attr>("expr") = term | (lit('(') > term > lit(')'));
		_ASSERT((test_attr("(1)", expr, varnf, space)));
	}

	// test that failing sequence leaves attribute consistent
	{
	std::string str;
	//no need to use omit[], but lit() is buggy ATM
	_ASSERT(test_attr("A\nB\nC", *(char_ >> omit[lit("\n")]), str, false));
	_ASSERT(str == "AB");
	}

	// test that sequence with only one parser producing attribute
	// makes it unwrapped
	{
	_ASSERT((boost::is_same<
		    typename attribute_of<decltype(lit("abc") >> attr(long())), unused_type>::type,
		    long>() ));
	}

	{   // test action
		using boost::fusion::at_c;

		char c = 0;
		int n = 0;
		auto f = [&](auto& ctx)
			{
				c = at_c<0>(_attr(ctx));
				n = at_c<1>(_attr(ctx));
			};

		_ASSERT(test("x123\"a string\"", (char_ >> int_ >> lit("\"a string\""))[f]));
		_ASSERT(c == 'x');
		_ASSERT(n == 123);
	}

	{   // test action
		char c = 0;
		int n = 0;
		auto f = [&](auto& ctx)
			{
				c = at_c<0>(_attr(ctx));
				n = at_c<1>(_attr(ctx));
			};

		_ASSERT(test("x 123 \"a string\"", (char_ >> int_ >> lit("\"a string\""))[f], space));
		_ASSERT(c == 'x');
		_ASSERT(n == 123);
	}

	{
#ifdef SPIRIT_NO_COMPILE_CHECK
		char const* const s = "";
		int i;
		parse(s, s, int_ >> int_, i);
#endif
	}

	{ // test move only types
		using boost::spirit::x3::eps;
		std::vector<move_only> v;
		_ASSERT(test_attr("ssszs", *synth_move_only >> lit('z') >> synth_move_only, v));
		_ASSERTEQUAL(v.size(), 4);
	}
}
