/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/variant.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/at.hpp>

#include <string>
#include <iostream>
#include <vector>

struct di_ignore
{
	std::string text;
};

struct di_include
{
	std::string FileName;
};

BOOST_FUSION_ADAPT_STRUCT(di_ignore,
	text
)

BOOST_FUSION_ADAPT_STRUCT(di_include,
	FileName
)

struct undefined {};


struct stationary : boost::noncopyable
{
	explicit stationary(int i) : val{i} {}
	stationary& operator=(int i) { val = i; return *this; }

	int val;
};


UNITTESTDEF(x3_test_alternative)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	using boost::spirit::x3::attr;
	using boost::spirit::x3::char_;
	using boost::spirit::x3::int_;
	using boost::spirit::x3::lit;
	using boost::spirit::x3::unused_type;
	using boost::spirit::x3::unused;
	using boost::spirit::x3::omit;
	using boost::spirit::x3::eps;

	BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(char_ | char_);

	{
		_ASSERT((test("a", char_ | char_)));
		_ASSERT((test("x", lit('x') | lit('i'))));
		_ASSERT((test("i", lit('x') | lit('i'))));
		_ASSERT((!test("z", lit('x') | lit('o'))));
		_ASSERT((test("rock", lit("rock") | lit("roll"))));
		_ASSERT((test("roll", lit("rock") | lit("roll"))));
		_ASSERT((test("rock", lit("rock") | int_)));
		_ASSERT((test("12345", lit("rock") | int_)));
	}

	{
		typedef boost::variant<undefined, int, char> attr_type;
		attr_type v;

		_ASSERT((test_attr("12345", int_ | char_, v)));
		_ASSERT(boost::get<int>(v) == 12345);

		_ASSERT((test_attr("12345", lit("rock") | int_ | char_, v)));
		_ASSERT(boost::get<int>(v) == 12345);

		v = attr_type();
		_ASSERT((test_attr("rock", lit("rock") | int_ | char_, v)));
		_ASSERT(v.which() == 0);

		_ASSERT((test_attr("x", lit("rock") | int_ | char_, v)));
		_ASSERT(boost::get<char>(v) == 'x');
	}

	{   // Make sure that we are using the actual supplied attribute types
		// from the variant and not the expected type.
		boost::variant<int, std::string> v;
		_ASSERT((test_attr("12345", int_ | +char_, v)));
		_ASSERT(boost::get<int>(v) == 12345);

		_ASSERT((test_attr("abc", int_ | +char_, v)));
		_ASSERT(boost::get<std::string>(v) == "abc");

		_ASSERT((test_attr("12345", +char_ | int_, v)));
		_ASSERT(boost::get<std::string>(v) == "12345");
	}

	{
		unused_type x;
		_ASSERT((test_attr("rock", lit("rock") | lit('x'), x)));
	}

	{
		// test if alternatives with all components having unused
		// attributes have an unused attribute

		using boost::fusion::vector;
		using boost::fusion::at_c;

		vector<char, char> v;
		_ASSERT((test_attr("abc",
			char_ >> (omit[char_] | omit[char_]) >> char_, v)));
		_ASSERT((at_c<0>(v) == 'a'));
		_ASSERT((at_c<1>(v) == 'c'));
	}

	{
		// Test that we can still pass a "compatible" attribute to
		// an alternate even if its "expected" attribute is unused type.

		std::string s;
		_ASSERT((test_attr("...", *(char_('.') | char_(',')), s)));
		_ASSERT(s == "...");
	}

	{   // make sure collapsing eps works as expected
		// (compile check only)

		using boost::spirit::x3::rule;
		using boost::spirit::x3::eps;
		using boost::spirit::x3::_attr;
		using boost::spirit::x3::_val;

		rule<class r1, wchar_t> r1;
		rule<class r2, wchar_t> r2;
		rule<class r3, wchar_t> r3;

		auto f = [&](auto& ctx){ _val(ctx) = _attr(ctx); };

		r3  = ((eps >> r1))[f];
		r3  = ((r1) | r2)[f];
		r3 = ((eps >> r1) | r2);
	}

	{
		std::string s;
		using boost::spirit::x3::eps;

		// test having a variant<container, ...>
		_ASSERT( (test_attr("a,b", (char_ % lit(',')) | eps, s )) );
		_ASSERT(s == "ab");
	}

	{
		using boost::spirit::x3::eps;

		// testing a sequence taking a container as attribute
		std::string s;
		_ASSERT( (test_attr("abc,a,b,c",
			char_ >> char_ >> (char_ % lit(',')), s )) );
		_ASSERT(s == "abcabc");

		// test having an optional<container> inside a sequence
		s.erase();
		_ASSERT( (test_attr("ab",
			char_ >> char_ >> -(char_ % lit(',')), s )) );
		_ASSERT(s == "ab");

		// test having a variant<container, ...> inside a sequence
		s.erase();
		_ASSERT( (test_attr("ab",
			char_ >> char_ >> ((char_ % lit(',')) | eps), s )) );
		_ASSERT(s == "ab");
		s.erase();
		_ASSERT( (test_attr("abc",
			char_ >> char_ >> ((char_ % lit(',')) | eps), s )) );
		_ASSERT(s == "abc");
	}

	{
		//compile test only (bug_march_10_2011_8_35_am)
		typedef boost::variant<double, std::string> value_type;

		using boost::spirit::x3::rule;
		using boost::spirit::x3::eps;

		rule<class r1, value_type> r1;
		auto r1_ = r1 = r1 | eps; // left recursive!

		unused = r1_; // silence unused local warning
	}

	{
		using boost::spirit::x3::rule;
		typedef boost::variant<di_ignore, di_include> d_line;

		rule<class ignore, di_ignore> ignore;
		rule<class include, di_include> include;
		rule<class line, d_line> line;

		auto start =
			line = include | ignore;

		unused = start; // silence unused local warning
	}

	// single-element fusion vector tests
	{
		boost::fusion::vector<boost::variant<int, std::string>> fv;
		_ASSERT((test_attr("12345", int_ | +char_, fv)));
		_ASSERT(boost::get<int>(boost::fusion::at_c<0>(fv)) == 12345);

		boost::fusion::vector<boost::variant<int, std::string>> fvi;
		_ASSERT((test_attr("12345", int_ | int_, fvi)));
		_ASSERT(boost::get<int>(boost::fusion::at_c<0>(fvi)) == 12345);
	}

	// alternative over single element sequences as part of another sequence
	{
		auto  key1 = lit("long") >> attr(long());
		auto  key2 = lit("char") >> attr(char());
		auto  keys = key1 | key2;
		auto pair = keys >> lit("=") >> +char_;

		boost::fusion::deque<boost::variant<long, char>, std::string> attr_;

		_ASSERT(test_attr("long=ABC", pair, attr_));
		_ASSERT(boost::get<long>(&boost::fusion::front(attr_)) != nullptr);
		_ASSERT(boost::get<char>(&boost::fusion::front(attr_)) == nullptr);
	}

	{ // ensure no unneeded synthesization, copying and moving occurred
		auto p = lit('{') >> int_ >> lit('}');

		stationary st { 0 };
		_ASSERT(test_attr("{42}", p | eps | p, st));
		_ASSERTEQUAL(st.val, 42);
	}

	{ // attributeless parsers must not insert values
		std::vector<int> v;
		_ASSERT(test_attr("1 2 3 - 5 - - 7 -", (int_ | lit('-')) % lit(' '), v));
		_ASSERTEQUAL(v.size(), 5);
		_ASSERTEQUAL(v[0], 1);
		_ASSERTEQUAL(v[1], 2);
		_ASSERTEQUAL(v[2], 3);
		_ASSERTEQUAL(v[3], 5);
		_ASSERTEQUAL(v[4], 7);
	}

	{ // regressing test for #603
		using boost::spirit::x3::attr;
		struct X {};
		std::vector<boost::variant<std::string, int, X>> v;
		_ASSERT(test_attr("xx42x9y", *(int_ | +char_('x') | lit('y') >> attr(X{})), v));
		_ASSERTEQUAL(v.size(), 5);
	}

	{ // sequence parser in alternative into container
		std::string s;
		_ASSERT(test_attr("abcbbcd",
			*(char_('a') >> *(*char_('b') >> char_('c')) | char_('d')), s));
		_ASSERTEQUAL(s, "abcbbcd");
	}

	{ // conversion between alternatives
		struct X {};
		struct Y {};
		struct Z {};
		boost::variant<X, Y, Z> v;
		boost::variant<Y, X> x{X{}};
		v = x; // boost::variant supports that convertion
		auto const p = lit('x') >> attr(x) | lit('z') >> attr(Z{});
		_ASSERT(test_attr("z", p, v));
		_ASSERT(boost::get<Z>(&v) != nullptr);
		_ASSERT(test_attr("x", p, v));
		_ASSERT(boost::get<X>(&v) != nullptr);
	}

	{ // regression test for #679
		using Qaz = std::vector<boost::variant<int>>;
		using Foo = std::vector<boost::variant<Qaz, int>>;
		using Bar = std::vector<boost::variant<Foo, int>>;
		Bar x;
		_ASSERT(test_attr("abaabb", +(lit('a') >> attr(Foo{}) | lit('b') >> attr(int{})), x));
	}
}
