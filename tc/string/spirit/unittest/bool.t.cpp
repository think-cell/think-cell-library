/*=============================================================================
	Copyright (c) 2001-2011 Hartmut Kaiser
	Copyright (c) 2011      Bryce Lelbach

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#include "bool.hpp"

UNITTESTDEF(x3_test_bool)
{
	using spirit_test::test_attr;
	using spirit_test::test;
	using boost::spirit::x3::bool_;

	{
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(bool_);

		_ASSERT(test("true", bool_));
		_ASSERT(test("false", bool_));
		_ASSERT(!test("fasle", bool_));
	}

	{
		using boost::spirit::x3::true_;
		using boost::spirit::x3::false_;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(true_);
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(false_);

		_ASSERT(test("true", true_));
		_ASSERT(!test("true", false_));
		_ASSERT(test("false", false_));
		_ASSERT(!test("false", true_));
	}

	{
		using boost::spirit::x3::true_;
		using boost::spirit::x3::false_;
		using boost::spirit::x3::no_case;

		_ASSERT(test("True", no_case[bool_]));
		_ASSERT(test("False", no_case[bool_]));
		_ASSERT(test("True", no_case[true_]));
		_ASSERT(test("False", no_case[false_]));
	}

	{
		bool b = false;
		_ASSERT(test_attr("true", bool_, b) && b);
		_ASSERT(test_attr("false", bool_, b) && !b);
		_ASSERT(!test_attr("fasle", bool_, b));
	}

	{
		typedef boost::spirit::x3::bool_parser<bool, boost::spirit::char_encoding::standard, backwards_bool_policies>
			backwards_bool_type;
		constexpr backwards_bool_type backwards_bool{};

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(backwards_bool);

		_ASSERT(test("true", backwards_bool));
		_ASSERT(test("eurt", backwards_bool));
		_ASSERT(!test("false", backwards_bool));
		_ASSERT(!test("fasle", backwards_bool));

		bool b = false;
		_ASSERT(test_attr("true", backwards_bool, b) && b);
		_ASSERT(test_attr("eurt", backwards_bool, b) && !b);
		_ASSERT(!test_attr("false", backwards_bool, b));
		_ASSERT(!test_attr("fasle", backwards_bool, b));
	}

	{
		typedef boost::spirit::x3::bool_parser<test_bool_type, boost::spirit::char_encoding::standard>
			bool_test_type;
		constexpr bool_test_type test_bool{};

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(test_bool);

		_ASSERT(test("true", test_bool));
		_ASSERT(test("false", test_bool));
		_ASSERT(!test("fasle", test_bool));

		test_bool_type b = false;
		_ASSERT(test_attr("true", test_bool, b) && b.b);
		_ASSERT(test_attr("false", test_bool, b) && !b.b);
		_ASSERT(!test_attr("fasle", test_bool, b));
	}
}
