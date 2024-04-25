/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser
	Copyright (c) 2011      Bryce Lelbach

	Use, modification and distribution is subject to the Boost Software
	License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at
	http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include "real.hpp"

#include <boost/math/concepts/real_concept.hpp>

UNITTESTDEF(x3_test_real4)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	///////////////////////////////////////////////////////////////////////////
	//  Custom data type
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::math::concepts::real_concept;
		using boost::spirit::x3::real_parser;
		using boost::spirit::x3::real_policies;
		using boost::spirit::x3::parse;

		constexpr real_parser<real_concept, real_policies<real_concept> > custom_real;
		real_concept d;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(custom_real);

		_ASSERT(test("-1234", custom_real));
		_ASSERT(test_attr("-1234", custom_real, d) && compare(d, -1234));

		_ASSERT(test("-1.2e3", custom_real));
		_ASSERT(test_attr("-1.2e3", custom_real, d) && compare(d, -1.2e3));

		_ASSERT(test("+1.2e3", custom_real));
		_ASSERT(test_attr("+1.2e3", custom_real, d) && compare(d, 1.2e3));

		_ASSERT(test("-0.1", custom_real));
		_ASSERT(test_attr("-0.1", custom_real, d) && compare(d, -0.1));

		_ASSERT(test("-1.2e-3", custom_real));
		_ASSERT(test_attr("-1.2e-3", custom_real, d) && compare(d, -1.2e-3));

		_ASSERT(test("-1.e2", custom_real));
		_ASSERT(test_attr("-1.e2", custom_real, d) && compare(d, -1.e2));

		_ASSERT(test("-.2e3", custom_real));
		_ASSERT(test_attr("-.2e3", custom_real, d) && compare(d, -.2e3));

		_ASSERT(test("-2e3", custom_real));
		_ASSERT(test_attr("-2e3", custom_real, d) && compare(d, -2e3));

		_ASSERT(!test("-e3", custom_real));
		_ASSERT(!test_attr("-e3", custom_real, d));

		_ASSERT(!test("-1.2e", custom_real));
		_ASSERT(!test_attr("-1.2e", custom_real, d));
	}

	///////////////////////////////////////////////////////////////////////////
	//  custom real tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::double_;
		custom_real n;

		_ASSERT(test_attr("-123456e6", double_, n));
	}
}
