/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "test.hpp"
#include "../x3.hpp"
#include <iostream>

// Custom string type with a C-style string conversion.
struct custom_string_c
{
	custom_string_c(char c) { str[0] = c; str[1] = '\0'; }

	operator char*() { return str; }
	operator char const*() const { return str; }

private:
	char str[2];
};

UNITTESTDEF(x3_test_symbols1)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::symbols;
	using boost::spirit::x3::no_case;
	using boost::spirit::x3::lit;

	{ // basics
		symbols<int> sym;

		sym.add
			("Joel")
			("Ruby")
			("Tenji")
			("Tutit")
			("Kim")
			("Joey")
			("Joeyboy")
		;

		_ASSERT((test("Joel", sym)));
		_ASSERT((test("Ruby", sym)));
		_ASSERT((test("Tenji", sym)));
		_ASSERT((test("Tutit", sym)));
		_ASSERT((test("Kim", sym)));
		_ASSERT((test("Joey", sym)));
		_ASSERT((test("Joeyboy", sym)));
		_ASSERT((!test("XXX", sym)));

		// test copy
		symbols<int> sym2;
		sym2 = sym;
		_ASSERT((test("Joel", sym2)));
		_ASSERT((test("Ruby", sym2)));
		_ASSERT((test("Tenji", sym2)));
		_ASSERT((test("Tutit", sym2)));
		_ASSERT((test("Kim", sym2)));
		_ASSERT((test("Joey", sym2)));
		_ASSERT((!test("XXX", sym2)));

		// make sure it plays well with other parsers
		_ASSERT((test("Joelyo", sym >> lit("yo"))));

		sym.remove
			("Joel")
			("Ruby")
		;

		_ASSERT((!test("Joel", sym)));
		_ASSERT((!test("Ruby", sym)));
	}

	{ // comma syntax
		symbols<int> sym;
		sym += "Joel", "Ruby", "Tenji", "Tutit", "Kim", "Joey";

		_ASSERT((test("Joel", sym)));
		_ASSERT((test("Ruby", sym)));
		_ASSERT((test("Tenji", sym)));
		_ASSERT((test("Tutit", sym)));
		_ASSERT((test("Kim", sym)));
		_ASSERT((test("Joey", sym)));
		_ASSERT((!test("XXX", sym)));

		sym -= "Joel", "Ruby";

		_ASSERT((!test("Joel", sym)));
		_ASSERT((!test("Ruby", sym)));
	}

	{ // no-case handling
		using namespace boost::spirit::x3::ascii;

		symbols<int> sym;
		// NOTE: make sure all entries are in lower-case!!!
		sym = "joel", "ruby", "tenji", "tutit", "kim", "joey";

		_ASSERT((test("joel", no_case[sym])));
		_ASSERT((test("ruby", no_case[sym])));
		_ASSERT((test("tenji", no_case[sym])));
		_ASSERT((test("tutit", no_case[sym])));
		_ASSERT((test("kim", no_case[sym])));
		_ASSERT((test("joey", no_case[sym])));

		_ASSERT((test("JOEL", no_case[sym])));
		_ASSERT((test("RUBY", no_case[sym])));
		_ASSERT((test("TENJI", no_case[sym])));
		_ASSERT((test("TUTIT", no_case[sym])));
		_ASSERT((test("KIM", no_case[sym])));
		_ASSERT((test("JOEY", no_case[sym])));

		// make sure it plays well with other parsers
		_ASSERT((test("Joelyo", no_case[sym] >> lit("yo"))));
	}

	{ // attributes
		symbols<int> sym;

		sym.add
			("Joel", 1)
			("Ruby", 2)
			("Tenji", 3)
			("Tutit", 4)
			("Kim", 5)
			("Joey", 6)
		;

		int i=0;
		_ASSERT((test_attr("Joel", sym, i)));
		_ASSERT(i == 1);
		_ASSERT((test_attr("Ruby", sym, i)));
		_ASSERT(i == 2);
		_ASSERT((test_attr("Tenji", sym, i)));
		_ASSERT(i == 3);
		_ASSERT((test_attr("Tutit", sym, i)));
		_ASSERT(i == 4);
		_ASSERT((test_attr("Kim", sym, i)));
		_ASSERT(i == 5);
		_ASSERT((test_attr("Joey", sym, i)));
		_ASSERT(i == 6);
		_ASSERT((!test_attr("XXX", sym, i)));

		// double add:

		sym.add("Joel", 265);
		_ASSERT((test_attr("Joel", sym, i)));
		_ASSERT(i == 1);
	}

	{ // actions
		using boost::spirit::x3::_attr;

		symbols<int> sym;
		sym.add
			("Joel", 1)
			("Ruby", 2)
			("Tenji", 3)
			("Tutit", 4)
			("Kim", 5)
			("Joey", 6)
		;

		int i;
		auto f = [&](auto& ctx){ i = _attr(ctx); };

		_ASSERT((test("Joel", sym[f])));
		_ASSERT(i == 1);
		_ASSERT((test("Ruby", sym[f])));
		_ASSERT(i == 2);
		_ASSERT((test("Tenji", sym[f])));
		_ASSERT(i == 3);
		_ASSERT((test("Tutit", sym[f])));
		_ASSERT(i == 4);
		_ASSERT((test("Kim", sym[f])));
		_ASSERT(i == 5);
		_ASSERT((test("Joey", sym[f])));
		_ASSERT(i == 6);
		_ASSERT((!test("XXX", sym[f])));
	}
}
