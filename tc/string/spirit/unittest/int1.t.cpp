/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser
	Copyright (c) 2011      Bryce Lelbach

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "int.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/at.hpp>

UNITTESTDEF(x3_test_int1)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	///////////////////////////////////////////////////////////////////////////
	//  signed integer tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::int_;
		int i=0;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(int_);

		_ASSERT(test("123456", int_));
		_ASSERT(test_attr("123456", int_, i));
		_ASSERT(i == 123456);

		_ASSERT(test("+123456", int_));
		_ASSERT(test_attr("+123456", int_, i));
		_ASSERT(i == 123456);

		_ASSERT(test("-123456", int_));
		_ASSERT(test_attr("-123456", int_, i));
		_ASSERT(i == -123456);

		_ASSERT(test(max_int, int_));
		_ASSERT(test_attr(max_int, int_, i));
		_ASSERT(i == INT_MAX);

		_ASSERT(test(min_int, int_));
		_ASSERT(test_attr(min_int, int_, i));
		_ASSERT(i == INT_MIN);

		_ASSERT(!test(int_overflow, int_));
		_ASSERT(!test_attr(int_overflow, int_, i));
		_ASSERT(!test(int_underflow, int_));
		_ASSERT(!test_attr(int_underflow, int_, i));

		_ASSERT(!test("-", int_));
		_ASSERT(!test_attr("-", int_, i));

		_ASSERT(!test("+", int_));
		_ASSERT(!test_attr("+", int_, i));

		// Bug report from Steve Nutt
		_ASSERT(!test_attr("5368709120", int_, i));

		// with leading zeros
		_ASSERT(test("0000000000123456", int_));
		_ASSERT(test_attr("0000000000123456", int_, i));
		_ASSERT(i == 123456);
	}

	///////////////////////////////////////////////////////////////////////////
	//  long long tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::long_long;
		boost::long_long_type ll=0;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(long_long);

		_ASSERT(test("1234567890123456789", long_long));
		_ASSERT(test_attr("1234567890123456789", long_long, ll));
		_ASSERT(ll == 1234567890123456789LL);

		_ASSERT(test("-1234567890123456789", long_long));
		_ASSERT(test_attr("-1234567890123456789", long_long, ll));
		_ASSERT(ll == -1234567890123456789LL);

		_ASSERT(test(max_long_long, long_long));
		_ASSERT(test_attr(max_long_long, long_long, ll));
		_ASSERT(ll == LLONG_MAX);

		_ASSERT(test(min_long_long, long_long));
		_ASSERT(test_attr(min_long_long, long_long, ll));
		_ASSERT(ll == LLONG_MIN);

		_ASSERT(!test(long_long_overflow, long_long));
		_ASSERT(!test_attr(long_long_overflow, long_long, ll));
		_ASSERT(!test(long_long_underflow, long_long));
		_ASSERT(!test_attr(long_long_underflow, long_long, ll));
	}

	///////////////////////////////////////////////////////////////////////////
	//  short_ and long_ tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::short_;
		using boost::spirit::x3::long_;
		int i=0;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(short_);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(long_);

		_ASSERT(test("12345", short_));
		_ASSERT(test_attr("12345", short_, i));
		_ASSERT(i == 12345);

		_ASSERT(test("1234567890", long_));
		_ASSERT(test_attr("1234567890", long_, i));
		_ASSERT(i == 1234567890);
	}

	///////////////////////////////////////////////////////////////////////////
	// Check overflow is parse error
	///////////////////////////////////////////////////////////////////////////
	{
		constexpr boost::spirit::x3::int_parser<boost::int8_t> int8_{};
		char c=char();

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(int8_);

		_ASSERT(!test_attr("999", int8_, c));

		int i;
		using boost::spirit::x3::short_;
		_ASSERT(!test_attr("32769", short_, i, false));
		_ASSERT(!test_attr("41234", short_, i, false));
	}

	///////////////////////////////////////////////////////////////////////////
	//  int_parser<unused_type> tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::int_parser;
		using boost::spirit::x3::unused_type;
		constexpr int_parser<unused_type> any_int{};

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(any_int);

		_ASSERT(test("123456", any_int));
		_ASSERT(test("-123456", any_int));
		_ASSERT(test("-1234567890123456789", any_int));
	}

	///////////////////////////////////////////////////////////////////////////
	//  action tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::_attr;
		using boost::spirit::x3::ascii::space;
		using boost::spirit::x3::int_;
		int n = 0, m = 0;

		auto f = [&](auto& ctx){ n = _attr(ctx); };

		_ASSERT(test("123", int_[f]));
		_ASSERT(n == 123);
		_ASSERT(test_attr("789", int_[f], m));
		_ASSERT(n == 789 && m == 789);
		_ASSERT(test("   456", int_[f], space));
		_ASSERT(n == 456);
	}

	///////////////////////////////////////////////////////////////////////////
	//  custom int tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::int_;
		using boost::spirit::x3::int_parser;
		custom_int i;

		_ASSERT(test_attr("-123456", int_, i));
		int_parser<custom_int, 10, 1, 2> int2;
		_ASSERT(test_attr("-12", int2, i));
	}

	///////////////////////////////////////////////////////////////////////////
	//  single-element fusion vector tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::int_;
		using boost::spirit::x3::int_parser;
		boost::fusion::vector<int> i;

		_ASSERT(test_attr("-123456", int_, i));
		_ASSERT(boost::fusion::at_c<0>(i) == -123456);
	}
}
