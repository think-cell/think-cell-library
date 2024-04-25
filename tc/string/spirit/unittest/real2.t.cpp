/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser
	Copyright (c) 2011      Bryce Lelbach

	Use, modification and distribution is subject to the Boost Software
	License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
	http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include "real.hpp"

template <typename T, typename P>
void basic_real_parser_test(P parser)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	T attr;

	_ASSERT(test("-1234", parser));
	_ASSERT(test_attr("-1234", parser, attr) && compare(attr, T(-1234l)));

	_ASSERT(test("-1.2e3", parser));
	_ASSERT(test_attr("-1.2e3", parser, attr) && compare(attr, T(-1.2e3l)));

	_ASSERT(test("+1.2e3", parser));
	_ASSERT(test_attr("+1.2e3", parser, attr) && compare(attr, T(1.2e3l)));

	_ASSERT(test("-0.1", parser));
	_ASSERT(test_attr("-0.1", parser, attr) && compare(attr, T(-0.1l)));

	_ASSERT(test("-1.2e-3", parser));
	_ASSERT(test_attr("-1.2e-3", parser, attr) && compare(attr, T(-1.2e-3l)));

	_ASSERT(test("-1.e2", parser));
	_ASSERT(test_attr("-1.e2", parser, attr) && compare(attr, T(-1.e2l)));

	_ASSERT(test("-.2e3", parser));
	_ASSERT(test_attr("-.2e3", parser, attr) && compare(attr, T(-.2e3l)));

	_ASSERT(test("-2e3", parser));
	_ASSERT(test_attr("-2e3", parser, attr) && compare(attr, T(-2e3l)));

	_ASSERT(!test("-e3", parser));
	_ASSERT(!test_attr("-e3", parser, attr));

	_ASSERT(!test("-1.2e", parser));
	_ASSERT(!test_attr("-1.2e", parser, attr));
}

UNITTESTDEF(x3_test_real2)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(boost::spirit::x3::float_);
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(boost::spirit::x3::double_);
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(boost::spirit::x3::long_double);

	///////////////////////////////////////////////////////////////////////////
	//  signed real number tests
	///////////////////////////////////////////////////////////////////////////
	{
		basic_real_parser_test<float>(boost::spirit::x3::float_);
		basic_real_parser_test<double>(boost::spirit::x3::double_);
		basic_real_parser_test<long double>(boost::spirit::x3::long_double);
	}

	{
		using boost::spirit::x3::double_;
		double  d;

#if defined(BOOST_SPIRIT_TEST_REAL_PRECISION)
		_ASSERT(test_attr("-5.7222349715140557e+307", double_, d));
		_ASSERT(d == -5.7222349715140557e+307); // exact!

		_ASSERT(test_attr("2.0332938517515416e-308", double_, d));
		_ASSERT(d == 2.0332938517515416e-308); // exact!

		_ASSERT(test_attr("20332938517515416e291", double_, d));
		_ASSERT(d == 20332938517515416e291); // exact!

		_ASSERT(test_attr("2.0332938517515416e307", double_, d));
		_ASSERT(d == 2.0332938517515416e307); // exact!
#endif

		_ASSERT(test("-inf", double_));
		_ASSERT(test("-infinity", double_));
		_ASSERT(test_attr("-inf", double_, d) &&
			std::isinf(d) && std::signbit(d));
		_ASSERT(test_attr("-infinity", double_, d) &&
			std::isinf(d) && std::signbit(d));
		_ASSERT(test("-INF", double_));
		_ASSERT(test("-INFINITY", double_));
		_ASSERT(test_attr("-INF", double_, d) &&
			std::isinf(d) && std::signbit(d));
		_ASSERT(test_attr("-INFINITY", double_, d) &&
			std::isinf(d) && std::signbit(d));

		_ASSERT(test("-nan", double_));
		_ASSERT(test_attr("-nan", double_, d) &&
			std::isnan(d) && std::signbit(d));
		_ASSERT(test("-NAN", double_));
		_ASSERT(test_attr("-NAN", double_, d) &&
			std::isnan(d) && std::signbit(d));

		_ASSERT(test("-nan(...)", double_));
		_ASSERT(test_attr("-nan(...)", double_, d) &&
			std::isnan(d) && std::signbit(d));
		_ASSERT(test("-NAN(...)", double_));
		_ASSERT(test_attr("-NAN(...)", double_, d) &&
			std::isnan(d) && std::signbit(d));

		_ASSERT(!test("1e999", double_));
		_ASSERT(!test("1e-999", double_));
		_ASSERT(test_attr("2.1111111e-303", double_, d) &&
			compare(d, 2.1111111e-303));
		_ASSERT(!test_attr("1.1234e", double_, d) && compare(d, 1.1234));

		// https://svn.boost.org/trac10/ticket/11608
		_ASSERT(test_attr("1267650600228229401496703205376", double_, d) &&
			compare(d, 1267650600228229401496703205376.));    // Note Qi has better precision

		_ASSERT(test_attr("12676506.00228229401496703205376", double_, d) &&
			compare(d, 12676506.00228229401496703205376));    // Note Qi has better precision

		_ASSERT(test_attr("12676506.00228229401496703205376E6", double_, d) &&
			compare(d, 12676506.00228229401496703205376E6));  // Note Qi has better precision
	}
}
