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

UNITTESTDEF(x3_test_rule1)
{
	using spirit_test::test_attr;
	using spirit_test::test;

	using namespace boost::spirit::x3::ascii;
	using boost::spirit::x3::rule;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::unused_type;
	using boost::spirit::x3::phrase_parse;
	using boost::spirit::x3::skip_flag;
	using boost::spirit::x3::traits::has_attribute;

#ifdef BOOST_SPIRIT_X3_NO_RTTI
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(rule<class r>{});
#endif
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(rule<class r>{"r"});
	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(rule<class r>{"r"} = lit('x'));

	// check attribute advertising
	static_assert( has_attribute<rule<class r, int>, /*Context=*/unused_type>::value, "");
	static_assert(!has_attribute<rule<class r     >, /*Context=*/unused_type>::value, "");
	static_assert( has_attribute<decltype(rule<class r, int>{} = int_), /*Context=*/unused_type>::value, "");
	static_assert(!has_attribute<decltype(rule<class r     >{} = int_), /*Context=*/unused_type>::value, "");


	{ // basic tests

		auto a = lit('a');
		auto b = lit('b');
		auto c = lit('c');
		rule<class r> r;

		{
			auto start =
				r = *(a | b | c);

			_ASSERT(test("abcabcacb", start));
		}

		{
			auto start =
				r = (a | b) >> (r | b);

			_ASSERT(test("aaaabababaaabbb", start));
			_ASSERT(test("aaaabababaaabba", start, false));

			// ignore the skipper!
			_ASSERT(test("aaaabababaaabba", start, space, false));
		}
	}

	{ // basic tests w/ skipper

		auto a = lit('a');
		auto b = lit('b');
		auto c = lit('c');
		rule<class r> r;

		{
			auto start =
				r = *(a | b | c);

			_ASSERT(test(" a b c a b c a c b ", start, space));
		}

		{
			auto start =
				r = (a | b) >> (r | b);

			_ASSERT(test(" a a a a b a b a b a a a b b b ", start, space));
			_ASSERT(test(" a a a a b a b a b a a a b b a ", start, space, false));
		}
	}

	{ // basic tests w/ skipper but no final post-skip

		auto a = rule<class a_id>()
			= lit('a');

		auto b = rule<class b_id>()
			= lit('b');

		auto c = rule<class c_id>()
			= lit('c');

		{
			auto start = rule<class start_id>() = *(a | b) >> c;

			char const *s1 = " a b a a b b a c ... "
			  , *const e1 = s1 + std::strlen(s1);
			_ASSERT(phrase_parse(s1, e1, start, space, skip_flag::dont_post_skip)
			  && s1 == e1 - 5);

		}

		{
			rule<class start> start;

			auto p =
				start = (a | b) >> (start | c);
			{
				char const *s1 = " a a a a b a b a b a a a b b b c "
				  , *const e1 = s1 + std::strlen(s1);
				_ASSERT(phrase_parse(s1, e1, p, space, skip_flag::post_skip)
				  && s1 == e1);
			}
			{
				char const *s1 = " a a a a b a b a b a a a b b b c "
				  , *const e1 = s1 + std::strlen(s1);
				_ASSERT(phrase_parse(s1, e1, p, space, skip_flag::dont_post_skip)
				  && s1 == e1 - 1);
			}
		}
	}
}
