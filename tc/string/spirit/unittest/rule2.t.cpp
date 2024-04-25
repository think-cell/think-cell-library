/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <string>
#include <cstring>
#include <iostream>

namespace x3 = boost::spirit::x3;

namespace {
	template<typename Id>
	struct check_no_rule_injection_parser
		: x3::parser<check_no_rule_injection_parser<Id>>
	{
		typedef x3::unused_type attribute_type;
		static bool const has_attribute = false;

		template <typename Iterator, typename Context
		  , typename RuleContext, typename Attribute>
		bool parse(Iterator&, Iterator const&, Context const&,
			RuleContext&, Attribute&) const
		{
			static_assert(std::is_same<std::remove_cvref_t<decltype(x3::get<Id>(std::declval<Context const&>()))>, x3::unused_type>::value,
				"no rule definition injection should occur");
			return true;
		}
	};

	template<typename Id>
	[[maybe_unused]] auto const check_no_rule_injection = check_no_rule_injection_parser<Id>{};
}

UNITTESTDEF(x3_test_rule2)
{
	using spirit_test::test_attr;
	using spirit_test::test;

	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::rule;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::unused_type;
	using boost::spirit::x3::_attr;

	{ // context tests

		char ch;
		auto a = rule<class a_id, char>() = alpha;

		// this semantic action requires the context
		auto f = [&](auto& ctx){ ch = _attr(ctx); };
		_ASSERT(test("x", a[f]));
		_ASSERT(ch == 'x');

		// this semantic action requires the (unused) context
		auto f2 = [&](auto&){ ch = 'y'; };
		_ASSERT(test("x", a[f2]));
		_ASSERT(ch == 'y');

		// the semantic action may optionally not have any arguments at all
		auto f3 = [&]{ ch = 'z'; };
		_ASSERT(test("x", a[f3]));
		_ASSERT(ch == 'z');

		_ASSERT(test_attr("z", a, ch)); // attribute is given.
		_ASSERT(ch == 'z');
	}

	{ // auto rules tests

		char ch = '\0';
		auto a = rule<class a_id, char>() = alpha;
		auto f = [&](auto& ctx){ ch = _attr(ctx); };

		_ASSERT(test("x", a[f]));
		_ASSERT(ch == 'x');
		ch = '\0';
		_ASSERT(test_attr("z", a, ch)); // attribute is given.
		_ASSERT(ch == 'z');

		ch = '\0';
		_ASSERT(test("x", a[f]));
		_ASSERT(ch == 'x');
		ch = '\0';
		_ASSERT(test_attr("z", a, ch)); // attribute is given.
		_ASSERT(ch == 'z');
	}

	{ // auto rules tests: allow stl containers as attributes to
	  // sequences (in cases where attributes of the elements
	  // are convertible to the value_type of the container or if
	  // the element itself is an stl container with value_type
	  // that is convertible to the value_type of the attribute).

		std::string s;
		auto f = [&](auto& ctx){ s = _attr(ctx); };

		{
			auto r = rule<class r_id, std::string>()
				= char_ >> *(lit(',') >> char_)
				;

			_ASSERT(test("a,b,c,d,e,f", r[f]));
			_ASSERT(s == "abcdef");
		}

		{
			auto r = rule<class r_id, std::string>()
				= char_ >> *(lit(',') >> char_);
			s.clear();
			_ASSERT(test("a,b,c,d,e,f", r[f]));
			_ASSERT(s == "abcdef");
		}

		{
			auto r = rule<class r_id, std::string>()
				= char_ >> char_ >> char_ >> char_ >> char_ >> char_;
			s.clear();
			_ASSERT(test("abcdef", r[f]));
			_ASSERT(s == "abcdef");
		}
	}

	{
		struct a;
		_ASSERT(test("", rule<a>{} = check_no_rule_injection<a>));
		_ASSERT(test("", rule<a>{} %= check_no_rule_injection<a>));
	}
}
