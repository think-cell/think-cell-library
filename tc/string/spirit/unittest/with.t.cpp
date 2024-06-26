/*=============================================================================
	Copyright (c) 2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"

namespace x3 = boost::spirit::x3;

struct my_tag;

struct my_rule_class
{
	template <typename Iterator, typename Exception, typename Context>
	x3::error_handler_result
	on_error(Iterator&, Iterator const&, Exception const&, Context const& context)
	{
		x3::get<my_tag>(context)++;
		return x3::error_handler_result::fail;
	}

	template <typename Iterator, typename Attribute, typename Context>
	inline void
	on_success(Iterator const&, Iterator const&, Attribute&, Context const& context)
	{
		x3::get<my_tag>(context)++;
	}
};

UNITTESTDEF(x3_test_with)
{
	using spirit_test::test_attr;
	using spirit_test::test;

	using boost::spirit::x3::rule;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::with;
	using boost::spirit::x3::lit;

// read from a mutable field is not allowed on these compilers
#if (!defined(_MSC_VER) || _MSC_VER >= 1910) && \
	(!defined(__clang__) || __clang_major__ >= 7)
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(with<my_tag>(0)[lit('x')]);
#endif
	{
		constexpr int i = 0;
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(with<my_tag>(i)[lit('x')]);
	}

	{ // injecting data into the context in the grammar

		int val = 0;
		auto r = rule<my_rule_class, char const*>() =
			lit('(') > int_ > lit(',') > int_ > lit(')')
			;

		auto start =
			with<my_tag>(std::ref(val)) [ r ]
			;

		_ASSERT(test("(123,456)", start));
		_ASSERT(!test("(abc,def)", start));
		_ASSERT(val == 2);
	}

	{ // injecting non-const lvalue into the context
		int val = 0;
		auto const r  = int_[([](auto& ctx){
			x3::get<my_tag>(ctx) += x3::_attr(ctx);
		})];
		_ASSERT(test("123,456", with<my_tag>(val)[r % lit(',')]));
		_ASSERT(579 == val);
	}

	{ // injecting rvalue into the context
		auto const r1 = int_[([](auto& ctx){
			x3::get<my_tag>(ctx) += x3::_attr(ctx);
		})];
		auto const r2 = rule<struct my_rvalue_rule_class, int>() =
			x3::lit('(') >> (r1 % lit(',')) >> x3::lit(')')[([](auto& ctx){
				x3::_val(ctx) = x3::get<my_tag>(ctx);
			})];
		int attr = 0;
		_ASSERT(test_attr("(1,2,3)", with<my_tag>(100)[r2], attr));
		_ASSERT(106 == attr);
	}

	{ // injecting const/non-const lvalue and rvalue into the context
		struct functor {
			int operator()(int& val) {
				return val * 10; // non-const ref returns 10 * injected val
			}
			int operator()(int const& val) {
				return val; // const ref returns injected val
			}
		};

		auto f = [](auto& ctx){
			x3::_val(ctx) = x3::_attr(ctx) + functor()(x3::get<my_tag>(ctx));
		};
		auto const r = rule<struct my_rule_class2, int>() = int_[f];

		int attr = 0;
		int const cval = 10;
		_ASSERT(test_attr("5", with<my_tag>(cval)[r], attr));
		_ASSERT(15 == attr); // x3::get returns const ref to cval

		attr = 0;
		int val = 10;
		_ASSERT(test_attr("5", with<my_tag>(val)[r], attr));
		_ASSERT(105 == attr); // x3::get returns ref to val

		attr = 0;

		_ASSERT(test_attr("5", with<my_tag>(10)[r], attr));
		// x3::get returns ref to member variable of with_directive
		_ASSERT(105 == attr);
	}
}
