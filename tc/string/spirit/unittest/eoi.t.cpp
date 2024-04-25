/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

#include <iostream>

UNITTESTDEF(x3_test_eoi)
{
	using spirit_test::test;
	using boost::spirit::x3::eoi;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(eoi);

	{
		_ASSERT((test("", eoi)));
		_ASSERT(!(test("x", eoi)));
	}

	{
		_ASSERT(what(eoi) == "eoi");
	}
}
