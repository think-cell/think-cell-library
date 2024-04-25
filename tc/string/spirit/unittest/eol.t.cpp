/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

#include <iostream>

UNITTESTDEF(x3_test_eol)
{
	using spirit_test::test;
	using boost::spirit::x3::eol;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(eol);

	{
		_ASSERT((test("\r\n", eol)));
		_ASSERT((test("\r", eol)));
		_ASSERT((test("\n", eol)));
		_ASSERT((!test("\n\r", eol)));
		_ASSERT((!test("", eol)));
	}

	{
		_ASSERT(what(eol) == "eol");
	}
}
