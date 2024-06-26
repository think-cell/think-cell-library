/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <cstring>
#include <functional>

#ifdef _MSC_VER
// bogus https://developercommunity.visualstudio.com/t/buggy-warning-c4709/471956
# pragma warning(disable: 4709) // comma operator within array index expression
#endif

namespace x3 = boost::spirit::x3;

int g_nTestAction = 0;

auto fun1 =
	[](auto& ctx)
	{
		g_nTestAction += x3::_attr(ctx);
	}
;

struct fun_action
{
	template <typename Context>
	void operator()(Context const& ctx) const
	{
		g_nTestAction += x3::_attr(ctx);
	}
};

auto fail =
	[](auto& ctx)
	{
		x3::_pass(ctx) = false;
	}
;

struct setnext
{
	setnext(char& next) : next(next) {}

	template <typename Context>
	void operator()(Context const& ctx) const
	{
		next = x3::_attr(ctx);
	}

	char& next;
};


struct stationary : boost::noncopyable
{
	explicit stationary(int i) : val{i} {}
	stationary& operator=(int i) { val = i; return *this; }

	int val;
};


UNITTESTDEF(x3_test_actions)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	using x3::int_;
	using x3::lit;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(x3::int_type{}[std::true_type{}]);

	{
		char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
		x3::parse(s1, e1, lit('{') >> int_[fun1] >> lit('}'));
	}


	{
		char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
		x3::parse(s1, e1, lit('{') >> int_[fun_action()] >> lit('}'));
	}

	{
		using namespace std::placeholders;
		char const *s1 = "{42}", *e1 = s1 + std::strlen(s1);
		x3::parse(s1, e1, lit('{') >> int_[std::bind(fun_action(), _1)] >> lit('}'));
	}

	_ASSERT(g_nTestAction == (42*3));

	{
	   std::string input("1234 6543");
	   char next = '\0';
	   _ASSERT(x3::phrase_parse(input.begin(), input.end(),
		  x3::int_[fail] | x3::digit[setnext(next)], x3::space));
	   _ASSERT(next == '1');
	}

	{ // ensure no unneeded synthesization, copying and moving occurred
		auto p = lit('{') >> int_ >> lit('}');

		stationary st { 0 };
		_ASSERT(test_attr("{42}", p[([]{})], st));
		_ASSERTEQUAL(st.val, 42);
	}
}
