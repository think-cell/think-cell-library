/*=============================================================================
	Copyright (c) 2001-2016 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include "../x3/support/ast/variant.hpp"
#include <string>
#include <iostream>

namespace x3 = boost::spirit::x3;

struct none {};

using variant = x3::variant<
		none
	  , bool
	  , std::string
	  , int
	  , double
	>;

struct ast : variant
{
	using variant::variant;
	using variant::operator=;

	ast(char const* s)
	  : variant(std::string{s})
	{}

	ast& operator=(char const* s)
	{
		variant::operator=(std::string{s});
		return *this;
	}
};

UNITTESTDEF(x3_test_x3_variant)
{
	{
		ast v{123};
		_ASSERT(boost::get<int>(v) == 123);

		v = "test";
		_ASSERT(boost::get<std::string>(v) == "test");

		v = true;
		_ASSERT(boost::get<bool>(v) == true);
	}
}
