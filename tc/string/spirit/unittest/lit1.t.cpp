/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	http://spirit.sourceforge.net/

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/vector.hpp>

#include <string>

UNITTESTDEF(x3_test_lit1)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::string;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(string("x"));

	{
		_ASSERT((test("kimpo", lit("kimpo"))));
		_ASSERT((test("kimpo", string("kimpo"))));

		_ASSERT((test("x", string("x"))));
		_ASSERT((test(L"x", string(L"x"))));

		std::basic_string<char> s("kimpo");
		std::basic_string<wchar_t> ws(L"kimpo");
		_ASSERT((test("kimpo", lit(s))));
		_ASSERT((test(L"kimpo", lit(ws))));
		_ASSERT((test("kimpo", string(s))));
		_ASSERT((test(L"kimpo", string(ws))));
	}

	{
		_ASSERT((test(L"kimpo", lit(L"kimpo"))));
		_ASSERT((test(L"kimpo", string(L"kimpo"))));
		_ASSERT((test(L"x", string(L"x"))));
	}

	{
		std::basic_string<char> s("kimpo");
		_ASSERT((test("kimpo", string(s))));

		std::basic_string<wchar_t> ws(L"kimpo");
		_ASSERT((test(L"kimpo", string(ws))));
	}

	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT((test("    kimpo", string("kimpo"), space)));
		_ASSERT((test(L"    kimpo", string(L"kimpo"), space)));
		_ASSERT((test("    x", string("x"), space)));
	}

	{
		using namespace boost::spirit::x3::ascii;
		_ASSERT((test("    kimpo", string("kimpo"), space)));
		_ASSERT((test(L"    kimpo", string(L"kimpo"), space)));
		_ASSERT((test("    x", string("x"), space)));
	}

	{
		using namespace boost::spirit::x3::ascii;
		std::string s;
		_ASSERT((test_attr("kimpo", string("kimpo"), s)));
		_ASSERT(s == "kimpo");
		s.clear();
		_ASSERT((test_attr("kimpo", string("kim") >> string("po"), s)));
		_ASSERT(s == "kimpo");
		s.clear();
		_ASSERT((test_attr("x", string("x"), s)));
		_ASSERT(s == "x");
	}

	{ // single-element fusion vector tests
		boost::fusion::vector<std::string> s;
		_ASSERT(test_attr("kimpo", string("kimpo"), s));
		_ASSERT(boost::fusion::at_c<0>(s) == "kimpo");
	}
}
