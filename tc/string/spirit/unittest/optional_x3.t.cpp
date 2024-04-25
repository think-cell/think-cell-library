/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "utils.hpp"
#include "../x3.hpp"
#include <boost/fusion/adapted/struct.hpp>
#include <boost/fusion/include/vector.hpp>
#include <iostream>

#ifdef _MSC_VER
// bogus https://developercommunity.visualstudio.com/t/buggy-warning-c4709/471956
# pragma warning(disable: 4709) // comma operator within array index expression
#endif

struct adata
{
	int a;
	std::optional<int> b;
};

BOOST_FUSION_ADAPT_STRUCT(adata,
	a, b
)

struct test_attribute_type
{
	template <typename Context>
	void operator()(Context& ctx) const
	{
		_ASSERT(typeid(decltype(_attr(ctx))).name() == typeid(std::optional<int>).name());
	}
};

UNITTESTDEF(x3_test_optional)
{
	using boost::spirit::x3::traits::is_optional;

	static_assert(is_optional<std::optional<int>>(), "is_optional problem");

	using spirit_test::test;
	using spirit_test::test_attr;

	using boost::spirit::x3::int_;
	using boost::spirit::x3::omit;
	using boost::spirit::x3::ascii::char_;
	using boost::spirit::x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(-int_);

	{
		_ASSERT((test("1234", -int_)));
		_ASSERT((test("abcd", -int_, false)));

		std::optional<int> n;
		_ASSERT(test_attr("", -int_, n));
		_ASSERT(!n);
		_ASSERT(test_attr("123", -int_, n));
		_ASSERT(n);
		_ASSERTEQUAL(*n, 123);

		std::optional<std::string> s;
		_ASSERT(test_attr("", -+char_, s));
		_ASSERT(!s);
		_ASSERT(test_attr("abc", -+char_, s));
		_ASSERT(s);
		_ASSERTEQUAL(*s, "abc");
	}

	{   // test propagation of unused
		using boost::fusion::at_c;
		using boost::fusion::vector;

		vector<char, char> v;
		_ASSERT((test_attr("a1234c", char_ >> -omit[int_] >> char_, v)));
		_ASSERT((at_c<0>(v) == 'a'));
		_ASSERT((at_c<1>(v) == 'c'));

		v = boost::fusion::vector<char, char>();
		_ASSERT((test_attr("a1234c", char_ >> omit[-int_] >> char_, v)));
		_ASSERT((at_c<0>(v) == 'a'));
		_ASSERT((at_c<1>(v) == 'c'));

		char ch=0;
		_ASSERT((test_attr(",c", -(lit(',') >> char_), ch)));
		_ASSERT((ch == 'c'));
	}

	{   // test action
		std::optional<int> n = 0;
		_ASSERT((test_attr("1234", (-int_)[test_attribute_type()], n)));
		_ASSERT((n == 1234));
	}

	{
		std::string s;
		_ASSERT((test_attr("abc", char_ >> -(char_ >> char_), s)));
		_ASSERT(s == "abc");
	}

	{
		std::optional<int> n = 0;
		auto f = [&](auto& ctx){ n = _attr(ctx); };

		_ASSERT((test("1234", (-int_)[f])));
		_ASSERT(n == 1234);

		n = std::optional<int>();
		_ASSERT((test("abcd", (-int_)[f], false)));
		_ASSERT(!n);
	}

	{
		std::vector<adata> v;
		_ASSERT((test_attr("a 1 2 a 2", *(lit('a') >> int_ >> -int_), v
		  , char_(' '))));
		_ASSERT(2 == v.size() &&
			1 == v[0].a && v[0].b && 2 == *(v[0].b) &&
			2 == v[1].a && !v[1].b);
	}

	{ // test move only types
		std::optional<move_only> o;
		_ASSERT(test_attr("s", -synth_move_only, o));
		_ASSERT(o);
	}
}
