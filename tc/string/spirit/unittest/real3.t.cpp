/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2010 Hartmut Kaiser

	Use, modification and distribution is subject to the Boost Software
	License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
	http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include "real.hpp"

UNITTESTDEF(x3_test_real3)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	///////////////////////////////////////////////////////////////////////////
	//  strict real number tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::real_parser;
		using boost::spirit::x3::parse;
		using boost::spirit::x3::strict_ureal_policies;
		using boost::spirit::x3::strict_real_policies;

		constexpr real_parser<double, strict_ureal_policies<double> > strict_udouble;
		constexpr real_parser<double, strict_real_policies<double> > strict_double;
		double  d;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(strict_udouble);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(strict_double);

		_ASSERT(!test("1234", strict_udouble));
		_ASSERT(!test_attr("1234", strict_udouble, d));

		_ASSERT(test("1.2", strict_udouble));
		_ASSERT(test_attr("1.2", strict_udouble, d) && compare(d, 1.2));

		_ASSERT(!test("-1234", strict_double));
		_ASSERT(!test_attr("-1234", strict_double, d));

		_ASSERT(test("123.", strict_double));
		_ASSERT(test_attr("123.", strict_double, d) && compare(d, 123));

		_ASSERT(test("3.E6", strict_double));
		_ASSERT(test_attr("3.E6", strict_double, d) && compare(d, 3e6));

		constexpr real_parser<double, no_trailing_dot_policy<double> > notrdot_real;
		constexpr real_parser<double, no_leading_dot_policy<double> > nolddot_real;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(notrdot_real);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(nolddot_real);

		_ASSERT(!test("1234.", notrdot_real));          //  Bad trailing dot
		_ASSERT(!test(".1234", nolddot_real));          //  Bad leading dot
	}

	///////////////////////////////////////////////////////////////////////////
	//  Special thousands separated numbers
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::real_parser;
		using boost::spirit::x3::parse;
		constexpr real_parser<double, ts_real_policies<double> > ts_real;
		double  d;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(ts_real);

		_ASSERT(test("123.01", ts_real));
		_ASSERT(test_attr("123.01", ts_real, d)
				&& compare(d, 123.01));

		_ASSERT(test("123,456,789.01", ts_real));
		_ASSERT(test_attr("123,456,789.01", ts_real, d)
				&& compare(d, 123456789.01));

		_ASSERT(test("12,345,678.90", ts_real));
		_ASSERT(test_attr("12,345,678.90", ts_real, d)
				&& compare(d, 12345678.90));

		_ASSERT(test("1,234,567.89", ts_real));
		_ASSERT(test_attr("1,234,567.89", ts_real, d)
				&& compare(d, 1234567.89));

		_ASSERT(!test("1234,567,890", ts_real));
		_ASSERT(!test("1,234,5678,9", ts_real));
		_ASSERT(!test("1,234,567.89e6", ts_real));
		_ASSERT(!test("1,66", ts_real));
	}
}
