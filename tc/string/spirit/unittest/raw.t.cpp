/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/std_pair.hpp>
#include <iostream>
#include <string>

using boost::spirit::x3::rule;

namespace {
	rule<class direct_rule, int> direct_rule = "direct_rule";
	rule<class indirect_rule, int> indirect_rule = "indirect_rule";

	auto const direct_rule_def = boost::spirit::x3::int_;
	auto const indirect_rule_def = direct_rule;

	BOOST_SPIRIT_DEFINE(direct_rule, indirect_rule)
}

UNITTESTDEF(x3_test_raw)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::raw;
	using boost::spirit::x3::eps;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::_attr;
	using boost::spirit::x3::parse;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::char_;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(raw[lit('x')]);

	{
		boost::iterator_range<char const*> range;
		std::string str;
		_ASSERT((test_attr("spirit_test_123", raw[alpha >> *(alnum | lit('_'))], range)));
		_ASSERT((std::string(range.begin(), range.end()) == "spirit_test_123"));
		_ASSERT((test_attr("  spirit", raw[*alpha], range, space)));
		_ASSERT((range.size() == 6));
	}

	{
		std::string str;
		_ASSERT((test_attr("spirit_test_123", raw[alpha >> *(alnum | lit('_'))], str)));
		_ASSERT((str == "spirit_test_123"));

		str.clear();
		_ASSERT((test_attr("x123", alpha >> raw[+alnum], str)));
		_ASSERTEQUAL(str, "x123");
	}

	{
		boost::iterator_range<char const*> range;
		_ASSERT((test("x", raw[alpha])));
		_ASSERT((test_attr("x", raw[alpha], range)));
		_ASSERT((test_attr("x", raw[alpha] >> eps, range)));
	}

	{
		boost::iterator_range<char const*> range;
		_ASSERT((test("x", raw[alpha][ ([&](auto& ctx){ range = _attr(ctx); }) ])));
		_ASSERT(range.size() == 1 && *range.begin() == 'x');
	}

	{
		boost::iterator_range<char const*> range;
		_ASSERT((test("x123x", lit('x') >> raw[+digit] >> lit('x'))));
		_ASSERT((test_attr("x123x", lit('x') >> raw[+digit] >> lit('x'), range)));
		_ASSERT((std::string(range.begin(), range.end()) == "123"));
	}

	{
		using range = boost::iterator_range<std::string::iterator>;
		boost::variant<int, range> attr;

		std::string str("test");
		parse(str.begin(), str.end(),  (int_ | raw[*char_]), attr);

		auto rng = boost::get<range>(attr);
		_ASSERT(std::string(rng.begin(), rng.end()) == "test");
	}

	{
		std::vector<boost::iterator_range<std::string::iterator>> attr;
		std::string str("123abcd");
		parse(str.begin(), str.end()
		  , (raw[int_] >> raw[*char_])
		  , attr
		);
		_ASSERT(attr.size() == 2);
		_ASSERT(std::string(attr[0].begin(), attr[0].end()) == "123");
		_ASSERT(std::string(attr[1].begin(), attr[1].end()) == "abcd");
	}

	{
		std::pair<int, boost::iterator_range<std::string::iterator>> attr;
		std::string str("123abcd");
		parse(str.begin(), str.end()
		  , (int_ >> raw[*char_])
		  , attr
		);
		_ASSERT(attr.first == 123);
		_ASSERT(std::string(attr.second.begin(), attr.second.end()) == "abcd");
	}

	{
		// test with simple rule
		boost::iterator_range<char const*> range;
		_ASSERT((test_attr("123", raw[direct_rule], range)));
		_ASSERT((std::string(range.begin(), range.end()) == "123"));
	}

	{
		// test with complex rule
		boost::iterator_range<char const*> range;
		_ASSERT((test_attr("123", raw[indirect_rule], range)));
		_ASSERT((std::string(range.begin(), range.end()) == "123"));
	}
}
