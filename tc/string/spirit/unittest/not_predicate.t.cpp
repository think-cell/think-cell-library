/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

#include <iostream>

UNITTESTDEF(x3_test_not_predicate)
{
	using spirit_test::test;
	using boost::spirit::x3::int_;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(!int_);

	{
		_ASSERT((!test("1234", !int_)));
		_ASSERT((test("abcd", !int_, false)));
		_ASSERT((!test("abcd", !!int_, false)));
	}
}
