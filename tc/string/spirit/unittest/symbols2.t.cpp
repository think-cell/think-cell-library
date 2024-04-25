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

std::string get_str(char const* str)
{
	return std::string(str);
}

UNITTESTDEF(x3_test_symbols2)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	using boost::spirit::x3::symbols;
	using boost::spirit::x3::rule;

	{ // construction from symbol array
		char const* syms[] = {"Joel","Ruby","Tenji","Tutit","Kim","Joey"};
		symbols<int> sym(syms);

		_ASSERT((test("Joel", sym)));
		_ASSERT((test("Ruby", sym)));
		_ASSERT((test("Tenji", sym)));
		_ASSERT((test("Tutit", sym)));
		_ASSERT((test("Kim", sym)));
		_ASSERT((test("Joey", sym)));
		_ASSERT((!test("XXX", sym)));
	}

	{ // construction from 2 arrays

		char const* syms[] = {"Joel","Ruby","Tenji","Tutit","Kim","Joey"};
		int data[] = {1,2,3,4,5,6};
		symbols<int> sym(syms, data);

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
	}

	{ // allow std::string and other string types
		symbols<> sym;

		// const and non-const std::string
		std::string a("abc");
		std::string const b("def");
		sym += a;
		sym += b;
		_ASSERT((test("abc", sym)));
		_ASSERT((test("def", sym)));
		sym = a;
		_ASSERT((test("abc", sym)));
		_ASSERT((!test("def", sym)));

		// non-const C-style string
		char arr[2]; arr[0] = 'a'; arr[1] = '\0';
		sym = arr;
		_ASSERT((test("a", sym)));
		_ASSERT((!test("b", sym)));

		// const and non-const custom string type
		custom_string_c c('x');
		custom_string_c const cc('y');
		sym = c, cc;
		_ASSERT((test("x", sym)));
		_ASSERT((test("y", sym)));
		_ASSERT((!test("z", sym)));
	}

	{ // find

		symbols<int> sym;
		sym.add("a", 1)("b", 2);

		_ASSERT(!sym.find("c"));

		_ASSERT(sym.find("a") && *sym.find("a") == 1);
		_ASSERT(sym.find("b") && *sym.find("b") == 2);

		_ASSERT(sym.at("a") == 1);
		_ASSERT(sym.at("b") == 2);
		_ASSERT(sym.at("c") == 0);

		_ASSERT(sym.find("a") && *sym.find("a") == 1);
		_ASSERT(sym.find("b") && *sym.find("b") == 2);
		_ASSERT(sym.find("c") && *sym.find("c") == 0);

		symbols<int> const_sym(sym);

		_ASSERT(const_sym.find("a") && *const_sym.find("a") == 1);
		_ASSERT(const_sym.find("b") && *const_sym.find("b") == 2);
		_ASSERT(const_sym.find("c") && *const_sym.find("c") == 0);
		_ASSERT(!const_sym.find("d"));

		char const *str1 = "all";
		char const *first = str1, *last = str1 + 3;
		_ASSERT(*sym.prefix_find(first, last) == 1 && first == str1 + 1);

		char const *str2 = "dart";
		first = str2; last = str2 + 4;
		_ASSERT(!sym.prefix_find(first, last) && first == str2);
	}

	{ // name
		symbols <int> sym,sym2;
		sym.name("test");
		_ASSERT(sym.name()=="test");
		sym2 = sym;
		_ASSERT(sym2.name()=="test");

		symbols <int> sym3(sym);
		_ASSERT(sym3.name()=="test");
	}

	{ // Substrings

		symbols<int> sym;
		_ASSERT(sym.at("foo") == 0);
		sym.at("foo") = 1;
		_ASSERT(sym.at("foo") == 1);
		_ASSERT(sym.at("fool") == 0);
		sym.at("fool") = 2;
		_ASSERT(sym.find("foo") && *sym.find("foo") == 1);
		_ASSERT(sym.find("fool") && *sym.find("fool") == 2);
		_ASSERT(!sym.find("foolish"));
		_ASSERT(!sym.find("foot"));
		_ASSERT(!sym.find("afoot"));

		char const *str, *first, *last;
		str = "foolish"; first = str; last = str + 7;
		_ASSERT(*sym.prefix_find(first, last) == 2 && first == str + 4);

		first = str; last = str + 4;
		_ASSERT(*sym.prefix_find(first, last) == 2 && first == str + 4);

		str = "food"; first = str; last = str + 4;
		_ASSERT(*sym.prefix_find(first, last) == 1 && first == str + 3);

		first = str; last = str + 3;
		_ASSERT(*sym.prefix_find(first, last) == 1 && first == str + 3);

		first = str; last = str + 2;
		_ASSERT(!sym.prefix_find(first, last) && first == str);
	}

	{
		// remove bug

		std::string s;
		symbols<double> vars;

		vars.add("l1", 12.0);
		vars.add("l2", 0.0);
		vars.remove("l2");
		vars.find("l1");
		double* d = vars.find("l1");
		_ASSERT(d != 0);
	}

	{ // test for proto problem with rvalue references (10-11-2011)
		symbols<int> sym;
		sym += get_str("Joel");
		sym += get_str("Ruby");

		_ASSERT((test("Joel", sym)));
		_ASSERT((test("Ruby", sym)));
	}
}
