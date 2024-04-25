/*=============================================================================
	Copyright (c) 2001-2013 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include "../x3/binary.hpp"
#include "../x3/directive/with.hpp"
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/at.hpp>

#include <string>
#include <iostream>

UNITTESTDEF(x3_test_expect)
{
	using namespace boost::spirit;
	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::expect;
	using boost::spirit::x3::lexeme;
	using boost::spirit::x3::no_case;
	using boost::spirit::x3::no_skip;
	using boost::spirit::x3::omit;
	using boost::spirit::x3::raw;
	using boost::spirit::x3::skip;
	using boost::spirit::x3::seek;
	using boost::spirit::x3::repeat;
	using boost::spirit::x3::matches;
	using boost::spirit::x3::eps;
	using boost::spirit::x3::eoi;
	using boost::spirit::x3::eol;
	using boost::spirit::x3::attr;
	using boost::spirit::x3::dword;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::symbols;
	using boost::spirit::x3::confix;
	using boost::spirit::x3::with;
	using spirit_test::test;
	using spirit_test::test_attr;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(expect[lit('x')]);
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(char_ > char_);

	{
		_ASSERT((test("aa", char_ >> expect[char_])));
		_ASSERT((test("aaa", char_ >> expect[char_ >> char_('a')])));
		_ASSERT((test("xi", char_('x') >> expect[char_('i')])));
		_ASSERT((!test("xi", char_('y') >> expect[char_('o')]))); // should not throw!
		_ASSERT((test("xin", char_('x') >> expect[char_('i') >> char_('n')])));
		_ASSERT((!test("xi", char_('x') >> expect[char_('o')])));
	}

	{
		_ASSERT((test("aa", char_ > char_)));
		_ASSERT((test("aaa", char_ > char_ > char_('a'))));
		_ASSERT((test("xi", char_('x') > char_('i'))));
		_ASSERT((!test("xi", char_('y') > char_('o')))); // should not throw!
		_ASSERT((test("xin", char_('x') > char_('i') > char_('n'))));
		_ASSERT((!test("xi", char_('x') > char_('o'))));
	}

	{
		_ASSERT((!test("ay:a", char_ > char_('x') >> lit(':') > lit('a'))));
	}

#if defined(BOOST_CLANG)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Woverloaded-shift-op-parentheses"
#endif
	{ // Test that attributes with > (sequences) work just like >> (sequences)

		using boost::fusion::vector;
		using boost::fusion::at_c;

		{
			vector<char, char, char> vecAttr;
			_ASSERT((test_attr(" a\n  b\n  c",
				char_ > char_ > char_, vecAttr, space)));
			_ASSERT((at_c<0>(vecAttr) == 'a'));
			_ASSERT((at_c<1>(vecAttr) == 'b'));
			_ASSERT((at_c<2>(vecAttr) == 'c'));
		}

		{
			vector<char, char, char> vecAttr;
			_ASSERT((test_attr(" a\n  b\n  c",
				char_ > char_ >> char_, vecAttr, space)));
			_ASSERT((at_c<0>(vecAttr) == 'a'));
			_ASSERT((at_c<1>(vecAttr) == 'b'));
			_ASSERT((at_c<2>(vecAttr) == 'c'));
		}

		{
			vector<char, char, char> vecAttr;
			_ASSERT((test_attr(" a, b, c",
				char_ >> lit(',') > char_ >> lit(',') > char_, vecAttr, space)));
			_ASSERT((at_c<0>(vecAttr) == 'a'));
			_ASSERT((at_c<1>(vecAttr) == 'b'));
			_ASSERT((at_c<2>(vecAttr) == 'c'));
		}

		{
			std::string strAttr;
			_ASSERT((test_attr("'azaaz'",
				lit("'") > *(char_("a") | char_("z")) > lit("'"), strAttr, space)));
			_ASSERT(strAttr == "azaaz");
		}
	}
#if defined(BOOST_CLANG)
#pragma clang diagnostic pop
#endif

	{
		_ASSERT((test(" a a", char_ > char_, space)));
		_ASSERT((test(" x i", char_('x') > char_('i'), space)));
		_ASSERT((!test(" x i", char_('x') > char_('o'), space)));
	}

	{
		_ASSERT((!test("bar", expect[lit("foo")])));
	}

	{ // Test expect in skipper
		_ASSERT((test("accbeabfcdg", repeat(7)[alpha], lit('a') > lit('b') | lit('c') > lit('d'))));
		std::string strAttr;
		_ASSERT((test_attr("accbeabfcdg", repeat(7)[alpha], strAttr, lit('a') > lit('b') | lit('c') > lit('d'))));
		_ASSERT((strAttr == "accbefg"));
	}

	{ // Test expect in auxilary parsers
		_ASSERT((test("a12", lit('a') > eps > +digit)));
		_ASSERT((!test("a12", lit('a') > eps(false) > +digit)));
		_ASSERT((test("a12", lit('a') > +digit > eoi)));
		_ASSERT((!test("a12", lit('a') > eoi > +digit)));
		_ASSERT((test("a12\n", lit('a') > +digit > eol)));
		_ASSERT((!test("a12\n", lit('a') > eol > +digit)));
		int n = 0;
		_ASSERT((test_attr("abc", lit("abc") > attr(12) > eoi, n)));
		_ASSERT((12 == n));
	}

	{ // Test expect in binary, numeric, char, string parsers
		_ASSERT((test("12abcd", +digit > dword)));
		_ASSERT((!test("12abc", +digit > dword)));
		_ASSERT((test("abc12", +alpha > int_)));
		_ASSERT((!test("abc", +alpha > int_)));
		_ASSERT((test("12a", +digit > lit('a'))));
		_ASSERT((!test("12a", +digit > lit('b'))));
		symbols<> s;
		s.add("cat");
		_ASSERT((test("12cat", +digit > s)));
		_ASSERT((!test("12dog", +digit > s)));
	}

	{ // Test expect in confix
		_ASSERT((test("[12cat]", confix(lit('['), lit(']'))[+digit > lit("cat")])));
		_ASSERT((!test("[12dog]", confix(lit('['), lit(']'))[+digit > lit("cat")])));
	}

	{ // Test expect in expect
		_ASSERT((test("abc", lit('a') >> expect[lit('b') >> lit('c')])));
		_ASSERT((!test("abc", lit('a') >> expect[lit('b') >> lit('d')])));
		_ASSERT((!test("abc", lit('a') >> expect[lit('b') > lit('d')])));
	}

	{ // Test expect in lexeme
		_ASSERT((test("12 ab", int_ >> lexeme[lit('a') > lit('b')], space)));
		_ASSERT((!test("12 a b", int_ >> lexeme[lit('a') > lit('b')], space)));
	}

	{ // Test expect in matches
		_ASSERT((test("ab", matches[lit('a') >> lit('b')])));
		_ASSERT((test("ac", matches[lit('a') >> lit('b')] >> lit("ac"))));
		_ASSERT((test("ab", matches[lit('a') > lit('b')])));
		_ASSERT((!test("ac", matches[lit('a') > lit('b')] >> lit("ac"))));
		bool bAttr = false;
		_ASSERT((test_attr("ab", matches[lit('a') > lit('b')], bAttr)));
		_ASSERT((true == bAttr));
	}

	{ // Test expect in no_case
		_ASSERT((test("12 aB", int_ >> no_case[lit('a') > lit('b')], space)));
		_ASSERT((!test("12 aB", int_ >> no_case[lit('a') > lit('c')], space)));
	}

	{ // Test expect in no_skip
		_ASSERT((test("12 3ab", int_ >> int_ >> no_skip[lit('a') > lit('b')], space)));
		_ASSERT((!test("12 3ab", int_ >> int_ >> no_skip[lit('a') > lit('c')], space)));
	}

	{ // Test expect in omit
		_ASSERT((test("ab", omit[lit('a') > lit('b')])));
		_ASSERT((!test("ab", omit[lit('a') > lit('c')])));
	}

	{ // Test expect in raw
		_ASSERT((test("ab", raw[lit('a') > lit('b')])));
		_ASSERT((!test("ab", raw[lit('a') > lit('c')])));
	}

	{ // Test expect in repeat
		_ASSERT((test("ababac", repeat(1, 3)[lit('a') >> lit('b')] >> lit("ac") | +alpha)));
		_ASSERT((!test("ababac", repeat(1, 3)[lit('a') > lit('b')] | +alpha)));
		_ASSERT((!test("acab", repeat(2, 3)[lit('a') > lit('b')] | +alpha)));
		_ASSERT((test("bcab", repeat(2, 3)[lit('a') > lit('b')] | +alpha)));
	}

	{ // Test expect in seek
		_ASSERT((test("a1b1c1", seek[lit('c') > lit('1')])));
		_ASSERT((!test("a1b1c2c1", seek[lit('c') > lit('1')])));
	}

	{ // Test expect in skip
		_ASSERT((test("ab[]c[]d", skip(lit('[') > lit(']'))[+alpha])));
		_ASSERT((!test("ab[]c[5]d", skip(lit('[') > lit(']'))[+alpha])));
		_ASSERT((test("a1[]b2c3[]d4", skip(lit('[') > lit(']'))[+(alpha > digit)])));
		_ASSERT((!test("a1[]b2c3[]d", skip(lit('[') > lit(']'))[+(alpha > digit)])));
	}

	{ // Test expect in alternative
		_ASSERT((test("ac", lit('a') >> lit('b') | lit("ac"))));
		_ASSERT((!test("ac", (lit('a') > lit('b')) | lit("ac"))));
		_ASSERT((test("ac", lit('a') >> lit('b') | lit('a') >> lit('d') | lit("ac"))));
		_ASSERT((!test("ac", lit('a') >> lit('b') | (lit('a') > lit('d')) | lit("ac"))));
	}

	{ // Test expect in and predicate
		_ASSERT((test("abc", lit('a') >> &(lit('b') > lit('c')) >> lit("bc"))));
		_ASSERT((!test("abc", lit('a') >> &(lit('b') > lit('d')) >> lit("bc"))));
	}

	{ // Test expect in difference
		_ASSERT((test("bcac", *(char_ - (lit('a') >> lit('b'))))));
		_ASSERT((test("bcab", *(char_ - (lit('a') > lit('b'))) >> lit("ab"))));
		_ASSERT((!test("bcac", *(char_ - (lit('a') > lit('b'))) >> lit("ab"))));
	}

	{ // Test expect in kleene
		_ASSERT((test("abac", *(lit('a') >> lit('b')) >> lit("ac"))));
		_ASSERT((!test("abac", *(lit('a') > lit('b')) >> lit("ac"))));
		_ASSERT((test("abbc", *(lit('a') > lit('b')) >> lit("bc"))));
	}

	{ // Test expect in list
		_ASSERT((test("ab::ab::ac", (lit('a') >> lit('b')) % (lit(':') >> lit(':')) >> lit("::ac"))));
		_ASSERT((!test("ab::ab::ac", (lit('a') > lit('b')) % (lit(':') >> lit(':')) >> lit("::ac"))));
		_ASSERT((test("ab::ab:ac", (lit('a') > lit('b')) % (lit(':') >> lit(':')) >> lit(":ac"))));
		_ASSERT((!test("ab::ab:ab", (lit('a') >> lit('b')) % (lit(':') > lit(':')) >> lit(":ab"))));
	}

	{ // Test expect in not predicate
		_ASSERT((test("[ac]", lit('[') >> !(lit('a') >> lit('b')) >> +alpha >> lit(']'))));
		_ASSERT((test("[bc]", lit('[') >> !(lit('a') > lit('b')) >> +alpha >> lit(']'))));
		_ASSERT((!test("[ac]", lit('[') >> !(lit('a') > lit('b')) >> +alpha >> lit(']'))));
	}

	{ // Test expect in optional
		_ASSERT((test("ac", -(lit('a') >> lit('b')) >> lit("ac"))));
		_ASSERT((test("ab", -(lit('a') > lit('b')))));
		_ASSERT((!test("ac", -(lit('a') > lit('b')) >> lit("ac"))));
	}

	{ // Test expect in plus
		_ASSERT((test("abac", +(lit('a') >> lit('b')) >> lit("ac"))));
		_ASSERT((test("abbc", +(lit('a') > lit('b')) >> lit("bc"))));
		_ASSERT((!test("abac", +(lit('a') > lit('b')) >> lit("ac"))));
	}

	{ // Test fast expect (with bool injection)
		using boost::spirit::x3::expectation_failure_tag;
		_ASSERT((test("ade", with<expectation_failure_tag>(false)[lit('a') >> (lit('b') >> lit('c') | lit('d') >> lit('e'))])));
		_ASSERT((test("abc", with<expectation_failure_tag>(false)[lit('a') >> (lit('b') > lit('c') | lit('d') > lit('e'))])));
		_ASSERT((test("ade", with<expectation_failure_tag>(false)[lit('a') >> (lit('b') > lit('c') | lit('d') > lit('e'))])));
		_ASSERT((!test("abd", with<expectation_failure_tag>(false)[lit('a') >> (lit('b') > lit('c') | lit('d') > lit('e'))])));
		_ASSERT((!test("adc", with<expectation_failure_tag>(false)[lit('a') >> (lit('b') > lit('c') | lit('d') > lit('e'))])));
	}
}
