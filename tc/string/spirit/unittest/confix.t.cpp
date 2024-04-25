/*=============================================================================
	Copyright (c) 2009 Chris Hoeppler
	Copyright (c) 2014 Lee Clagett

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3/char.hpp"
#include "../x3/core.hpp"
#include "../x3/numeric.hpp"
#include "../x3/operator.hpp"
#include "../x3/string.hpp"
#include "../x3/directive/confix.hpp"

UNITTESTDEF(x3_test_confix)
{
	namespace x3 = boost::spirit::x3;
	using namespace spirit_test;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(x3::confix(lit('('), lit(')')));
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(x3::confix(lit("["), lit("]")));
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(x3::confix(lit("/*"), lit("*/")));

	{
		const auto comment = x3::confix(lit("/*"), lit("*/"));

		_ASSERT(test_failure("/abcdef*/", comment[lit("abcdef")]));
		_ASSERT(test_failure("/* abcdef*/", comment[lit("abcdef")]));
		_ASSERT(test_failure("/*abcdef */", comment[lit("abcdef")]));
		_ASSERT(test("/*abcdef*/", comment[lit("abcdef")]));

		{
			unsigned value = 0;
			_ASSERT(
				test_attr(" /* 123 */ ", comment[x3::uint_], value, x3::space));
			_ASSERT(value == 123);

			using x3::_attr;
			value = 0;
			const auto lambda = [&value](auto& ctx ){ value = _attr(ctx) + 1; };
			_ASSERT(test_attr("/*123*/", comment[x3::uint_][lambda], value));
			_ASSERT(value == 124);
		}
	}
	{
		const auto array = x3::confix(lit('['), lit(']'));

		{
			std::vector<unsigned> values;

			_ASSERT(test("[0,2,4,6,8]", array[x3::uint_ % lit(',')]));
			_ASSERT(test_attr("[0,2,4,6,8]", array[x3::uint_ % lit(',')], values));
			_ASSERT(
				values.size() == 5 &&
				values[0] == 0 &&
				values[1] == 2 &&
				values[2] == 4 &&
				values[3] == 6 &&
				values[4] == 8);
		}
		{
			std::vector<std::vector<unsigned>> values;
			_ASSERT(
				test("[[1,3,5],[0,2,4]]", array[array[x3::uint_ % lit(',')] % lit(',')]));
			_ASSERT(
				test_attr(
					"[[1,3,5],[0,2,4]]",
					array[array[x3::uint_ % lit(',')] % lit(',')],
					values));
			_ASSERT(
				values.size() == 2 &&
				values[0].size() == 3 &&
				values[0][0] == 1 &&
				values[0][1] == 3 &&
				values[0][2] == 5 &&
				values[1].size() == 3 &&
				values[1][0] == 0 &&
				values[1][1] == 2 &&
				values[1][2] == 4);
		}
	}
}
