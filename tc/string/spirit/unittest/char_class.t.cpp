/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2010 Hartmut Kaiser

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#define BOOST_SPIRIT_X3_UNICODE
#include "test.hpp"
#include "../x3.hpp"
#include "../x3/char/unicode.hpp"
#include <boost/type_traits/is_same.hpp>

#include <iostream>

UNITTESTDEF(x3_test_char_class)
{
	using spirit_test::test;
	using spirit_test::test_failure;
	using spirit_test::test_attr;

	using boost::spirit::x3::unused_type;

	{
		using namespace boost::spirit::x3::ascii;
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alnum);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alpha);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(digit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(xdigit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(cntrl);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(graph);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(lower);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(print);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(punct);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(space);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(blank);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(upper);
		_ASSERT(test("1", alnum));
		_ASSERT(!test(" ", alnum));
		_ASSERT(!test("1", alpha));
		_ASSERT(test("x", alpha));
		_ASSERT(test(" ", blank));
		_ASSERT(!test("x", blank));
		_ASSERT(test("1", digit));
		_ASSERT(!test("x", digit));
		_ASSERT(test("a", lower));
		_ASSERT(!test("A", lower));
		_ASSERT(test("!", punct));
		_ASSERT(!test("x", punct));
		_ASSERT(test(" ", space));
		_ASSERT(test("\n", space));
		_ASSERT(test("\r", space));
		_ASSERT(test("\t", space));
		_ASSERT(test("A", upper));
		_ASSERT(!test("a", upper));
		_ASSERT(test("A", xdigit));
		_ASSERT(test("0", xdigit));
		_ASSERT(test("f", xdigit));
		_ASSERT(!test("g", xdigit));
	}

	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT(!test("1", ~alnum));
		_ASSERT(test(" ", ~alnum));
		_ASSERT(test("1", ~alpha));
		_ASSERT(!test("x", ~alpha));
		_ASSERT(!test(" ", ~blank));
		_ASSERT(test("x", ~blank));
		_ASSERT(!test("1", ~digit));
		_ASSERT(test("x", ~digit));
		_ASSERT(!test("a", ~lower));
		_ASSERT(test("A", ~lower));
		_ASSERT(!test("!", ~punct));
		_ASSERT(test("x", ~punct));
		_ASSERT(!test(" ", ~space));
		_ASSERT(!test("\n", ~space));
		_ASSERT(!test("\r", ~space));
		_ASSERT(!test("\t", ~space));
		_ASSERT(!test("A", ~upper));
		_ASSERT(test("a", ~upper));
		_ASSERT(!test("A", ~xdigit));
		_ASSERT(!test("0", ~xdigit));
		_ASSERT(!test("f", ~xdigit));
		_ASSERT(test("g", ~xdigit));
	}

	{
		using namespace boost::spirit::x3::iso8859_1;
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alnum);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alpha);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(digit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(xdigit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(cntrl);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(graph);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(lower);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(print);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(punct);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(space);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(blank);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(upper);
		_ASSERT(test("1", alnum));
		_ASSERT(!test(" ", alnum));
		_ASSERT(!test("1", alpha));
		_ASSERT(test("x", alpha));
		_ASSERT(test(" ", blank));
		_ASSERT(!test("x", blank));
		_ASSERT(test("1", digit));
		_ASSERT(!test("x", digit));
		_ASSERT(test("a", lower));
		_ASSERT(!test("A", lower));
		_ASSERT(test("!", punct));
		_ASSERT(!test("x", punct));
		_ASSERT(test(" ", space));
		_ASSERT(test("\n", space));
		_ASSERT(test("\r", space));
		_ASSERT(test("\t", space));
		_ASSERT(test("A", upper));
		_ASSERT(!test("a", upper));
		_ASSERT(test("A", xdigit));
		_ASSERT(test("0", xdigit));
		_ASSERT(test("f", xdigit));
		_ASSERT(!test("g", xdigit));

		// test extended ASCII characters
		_ASSERT(test("\xE9", alpha));
		_ASSERT(test("\xE9", lower));
		_ASSERT(!test("\xE9", upper));
	}

	{
		using namespace boost::spirit::x3::standard;
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alnum);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alpha);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(digit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(xdigit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(cntrl);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(graph);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(lower);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(print);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(punct);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(space);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(blank);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(upper);
		_ASSERT(test("1", alnum));
		_ASSERT(!test(" ", alnum));
		_ASSERT(!test("1", alpha));
		_ASSERT(test("x", alpha));
		_ASSERT(test(" ", blank));
		_ASSERT(!test("x", blank));
		_ASSERT(test("1", digit));
		_ASSERT(!test("x", digit));
		_ASSERT(test("a", lower));
		_ASSERT(!test("A", lower));
		_ASSERT(test("!", punct));
		_ASSERT(!test("x", punct));
		_ASSERT(test(" ", space));
		_ASSERT(test("\n", space));
		_ASSERT(test("\r", space));
		_ASSERT(test("\t", space));
		_ASSERT(test("A", upper));
		_ASSERT(!test("a", upper));
		_ASSERT(test("A", xdigit));
		_ASSERT(test("0", xdigit));
		_ASSERT(test("f", xdigit));
		_ASSERT(!test("g", xdigit));
		_ASSERT(!test("\xF1", print));
	}

	{
		using namespace boost::spirit::x3::standard_wide;
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alnum);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alpha);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(digit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(xdigit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(cntrl);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(graph);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(lower);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(print);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(punct);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(space);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(blank);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(upper);
		_ASSERT(test(L"1", alnum));
		_ASSERT(!test(L" ", alnum));
		_ASSERT(!test(L"1", alpha));
		_ASSERT(test(L"x", alpha));
		_ASSERT(test(L" ", blank));
		_ASSERT(!test(L"x", blank));
		_ASSERT(test(L"1", digit));
		_ASSERT(!test(L"x", digit));
		_ASSERT(test(L"a", lower));
		_ASSERT(!test(L"A", lower));
		_ASSERT(test(L"!", punct));
		_ASSERT(!test(L"x", punct));
		_ASSERT(test(L" ", space));
		_ASSERT(test(L"\n", space));
		_ASSERT(test(L"\r", space));
		_ASSERT(test(L"\t", space));
		_ASSERT(test(L"A", upper));
		_ASSERT(!test(L"a", upper));
		_ASSERT(test(L"A", xdigit));
		_ASSERT(test(L"0", xdigit));
		_ASSERT(test(L"f", xdigit));
		_ASSERT(!test(L"g", xdigit));
	}

	{
		using namespace boost::spirit::x3::unicode;
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alnum);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(alpha);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(digit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(xdigit);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(cntrl);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(graph);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(lower);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(print);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(punct);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(space);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(blank);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(upper);
		_ASSERT(test(L"1", alnum));
		_ASSERT(!test(L" ", alnum));
		_ASSERT(!test(L"1", alpha));
		_ASSERT(test(L"x", alpha));
		_ASSERT(test(L" ", blank));
		_ASSERT(!test(L"x", blank));
		_ASSERT(test(L"1", digit));
		_ASSERT(!test(L"x", digit));
		_ASSERT(test(L"a", lower));
		_ASSERT(!test(L"A", lower));
		_ASSERT(test(L"!", punct));
		_ASSERT(!test(L"x", punct));
		_ASSERT(test(L" ", space));
		_ASSERT(test(L"\n", space));
		_ASSERT(test(L"\r", space));
		_ASSERT(test(L"\t", space));
		_ASSERT(test(L"A", upper));
		_ASSERT(!test(L"a", upper));
		_ASSERT(test(L"A", xdigit));
		_ASSERT(test(L"0", xdigit));
		_ASSERT(test(L"f", xdigit));
		_ASSERT(!test(L"g", xdigit));

		_ASSERT(test(L"A", alphabetic));
		_ASSERT(test(L"9", decimal_number));
		_ASSERT(test(L"\u2800", braille));
		_ASSERT(!test(L" ", braille));
		_ASSERT(test(L" ", ~braille));
		// $$$ TODO $$$ Add more unicode tests
	}

	{   // test invalid unicode literals
		using namespace boost::spirit::x3::unicode;

		auto const invalid_unicode = char32_t{0x7FFFFFFF};
		auto const input           = boost::u32string_view(&invalid_unicode, 1);

		_ASSERT(test_failure(input, char_));

		// force unicode category lookup
		// related issue: https://github.com/boostorg/spirit/issues/524
		_ASSERT(test_failure(input, alpha));
		_ASSERT(test_failure(input, upper));
		_ASSERT(test_failure(input, lower));
	}

	{   // test attribute extraction
		using boost::spirit::x3::traits::attribute_of;
		using boost::spirit::x3::iso8859_1::alpha;
		using boost::spirit::x3::iso8859_1::alpha_type;

		static_assert(
			boost::is_same<
				attribute_of<alpha_type, unused_type>::type
			  , unsigned char>::value
		  , "Wrong attribute type!"
		);

		int attr = 0;
		_ASSERT(test_attr("a", alpha, attr));
		_ASSERT(attr == 'a');
	}

	{   // test attribute extraction
		using boost::spirit::x3::iso8859_1::alpha;
		using boost::spirit::x3::iso8859_1::space;
		char attr = 0;
		_ASSERT(test_attr("     a", alpha, attr, space));
		_ASSERT(attr == 'a');
	}

	{   // test action
		using namespace boost::spirit::x3::ascii;
		using boost::spirit::x3::_attr;
		char ch;
		auto f = [&](auto& ctx){ ch = _attr(ctx); };

		_ASSERT(test("x", alnum[f]));
		_ASSERT(ch == 'x');
		_ASSERT(test("   A", alnum[f], space));
		_ASSERT(ch == 'A');
	}
}
