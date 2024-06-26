/*=============================================================================
	Copyright (c) 2001-2012 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/fusion/include/std_pair.hpp>
#include <boost/variant.hpp>
#include <string>
#include <vector>
#include <cstring>
#include <iostream>

#ifdef _MSC_VER
// bogus https://developercommunity.visualstudio.com/t/buggy-warning-c4709/471956
# pragma warning(disable: 4709) // comma operator within array index expression
#endif

using boost::spirit::x3::_val;
namespace x3 = boost::spirit::x3;
using boost::spirit::x3::lit;

struct f
{
	template <typename Context>
	void operator()(Context const& ctx) const
	{
		_val(ctx) += _attr(ctx);
	}
};


struct stationary : boost::noncopyable
{
	explicit stationary(int i) : val{i} {}
	stationary& operator=(int i) { val = i; return *this; }

	int val;
};


namespace check_stationary {

boost::spirit::x3::rule<class a_r, stationary> const a;
boost::spirit::x3::rule<class b_r, stationary> const b;

auto const a_def = lit('{') >> boost::spirit::x3::int_ >> lit('}');
auto const b_def = a;

BOOST_SPIRIT_DEFINE(a, b)

}

namespace check_recursive {

using node_t = boost::make_recursive_variant<
				   int,
				   std::vector<boost::recursive_variant_>
			   >::type;

boost::spirit::x3::rule<class grammar_r, node_t> const grammar;

auto const grammar_def = lit('[') >> grammar % lit(',') >> lit(']') | boost::spirit::x3::int_;

BOOST_SPIRIT_DEFINE(grammar)

}

namespace check_recursive_scoped {

using check_recursive::node_t;

x3::rule<class intvec_r, node_t> const intvec;
auto const grammar = intvec = lit('[') >> intvec % lit(',') >> lit(']') | x3::int_;

}

struct recursive_tuple
{
	int value;
	std::vector<recursive_tuple> children;
};
BOOST_FUSION_ADAPT_STRUCT(recursive_tuple,
	value, children)

// regression test for #461
namespace check_recursive_tuple {

x3::rule<class grammar_r, recursive_tuple> const grammar;
auto const grammar_def = x3::int_ >> (lit('{') >> grammar % lit(',') >> lit('}') | x3::eps);
BOOST_SPIRIT_DEFINE(grammar)

BOOST_SPIRIT_INSTANTIATE(decltype(grammar), char const*, TC_FWD(
	x3::context<
		x3::expectation_failure_tag,
		std::optional<x3::expectation_failure<char const*>>,
		x3::unused_type
	>
))

}


UNITTESTDEF(x3_test_rule3)
{
	using spirit_test::test_attr;
	using spirit_test::test;

	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::rule;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::eps;
	using boost::spirit::x3::unused_type;


	{ // synth attribute value-init

		std::string s;
		typedef rule<class r, std::string> rule_type;

		auto rdef = rule_type()
			= alpha                 [f()]
			;

		_ASSERT(test_attr("abcdef", +rdef, s));
		_ASSERT(s == "abcdef");
	}

	{ // synth attribute value-init

		std::string s;
		typedef rule<class r, std::string> rule_type;

		auto rdef = rule_type() =
			alpha /
			   [](auto& ctx)
			   {
				  _val(ctx) += _attr(ctx);
			   }
			;

		_ASSERT(test_attr("abcdef", +rdef, s));
		_ASSERT(s == "abcdef");
	}

	{
		auto r = rule<class r_id, int>{} = eps[([] (auto& ctx) {
			using boost::spirit::x3::_val;
			static_assert(std::is_same<std::decay_t<decltype(_val(ctx))>, unused_type>::value,
				"Attribute must not be synthesized");
		})];
		_ASSERT(test("", r));
	}

	{ // ensure no unneeded synthesization, copying and moving occurred
		stationary st { 0 };
		_ASSERT(test_attr("{42}", check_stationary::b, st));
		_ASSERTEQUAL(st.val, 42);
	}

	{
		using namespace check_recursive;
		node_t v;
		_ASSERT(test_attr("[4,2]", grammar, v));
		_ASSERT((node_t{std::vector<node_t>{{4}, {2}}} == v));
	}
	{
		using namespace check_recursive_scoped;
		node_t v;
		_ASSERT(test_attr("[4,2]", grammar, v));
		_ASSERT((node_t{std::vector<node_t>{{4}, {2}}} == v));
	}
}
