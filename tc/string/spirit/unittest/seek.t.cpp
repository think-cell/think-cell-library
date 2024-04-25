/*//////////////////////////////////////////////////////////////////////////////
	Copyright (c) 2011 Jamboree
	Copyright (c) 2014 Lee Clagett

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
//////////////////////////////////////////////////////////////////////////////*/

#include "test.hpp"
#include "../x3/auxiliary/eoi.hpp"
#include "../x3/core.hpp"
#include "../x3/char.hpp"
#include "../x3/string.hpp"
#include "../x3/numeric.hpp"
#include "../x3/operator/plus.hpp"
#include "../x3/operator/sequence.hpp"
#include "../x3/directive/seek.hpp"
#include <vector>

///////////////////////////////////////////////////////////////////////////////
UNITTESTDEF(x3_test_seek)
{
	using namespace spirit_test;
	namespace x3 = boost::spirit::x3;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(x3::seek[lit('x')]);

	// test eoi
	{
		_ASSERT(test("", x3::seek[x3::eoi]));
		_ASSERT(test(" ", x3::seek[x3::eoi], x3::space));
		_ASSERT(test("a", x3::seek[x3::eoi]));
		_ASSERT(test(" a", x3::seek[x3::eoi], x3::space));
	}

	// test literal finding
	{
		int i = 0;

		_ASSERT(
			test_attr("!@#$%^&*KEY:123", x3::seek[lit("KEY:")] >> x3::int_, i)
			&& i == 123
		);
	}
	// test sequence finding
	{
		int i = 0;

		_ASSERT(
			test_attr("!@#$%^&* KEY : 123", x3::seek[x3::lit("KEY") >> lit(':')] >> x3::int_, i, x3::space)
			&& i == 123
		);
	}

	// test attr finding
	{
		std::vector<int> v;

		_ASSERT( // expect partial match
			test_attr("a06b78c3d", +x3::seek[x3::int_], v, false)
			&& v.size() == 3 && v[0] == 6 && v[1] == 78 && v[2] == 3
		);
	}

	// test action
	{

	   bool b = false;
	   auto const action = [&b]() { b = true; };

	   _ASSERT( // expect partial match
		   test("abcdefg", x3::seek[lit("def")][action], false)
		   && b
	   );
	}

	// test container
	{
		std::vector<int> v;

		_ASSERT(
			test_attr("abcInt:100Int:95Int:44", x3::seek[+(lit("Int:") >> x3::int_)], v)
			&& v.size() == 3 && v[0] == 100 && v[1] == 95 && v[2] == 44
		);
	}

	// test failure rollback
	{
		_ASSERT(test_failure("abcdefg", x3::seek[x3::int_]));
	}

	// past the end regression GH#658
	_ASSERT(!test(" ", x3::seek[lit('x')], x3::space));
}
