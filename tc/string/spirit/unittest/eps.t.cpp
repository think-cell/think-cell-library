/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

#include <iostream>

UNITTESTDEF(x3_test_eps)
{
	using spirit_test::test;
	using boost::spirit::x3::eps;
	using boost::spirit::x3::unused_type;

	{
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(eps);
		_ASSERT((test("", eps)));
		_ASSERT((test("xxx", eps, false)));
		//~ _ASSERT((!test("", !eps))); // not predicate $$$ Implement me! $$$
	}

	{   // test non-lazy semantic predicate

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(eps(true));
		_ASSERT((test("", eps(true))));
		_ASSERT((!test("", eps(false))));
		_ASSERT((test("", !eps(false))));
	}

	{   // test lazy semantic predicate

		auto true_ = [](unused_type) { return true; };
		auto false_ = [](unused_type) { return false; };

		// cannot use lambda in constant expression before C++17
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(eps(std::true_type{}));
		_ASSERT((test("", eps(true_))));
		_ASSERT((!test("", eps(false_))));
		_ASSERT((test("", !eps(false_))));
	}
}
