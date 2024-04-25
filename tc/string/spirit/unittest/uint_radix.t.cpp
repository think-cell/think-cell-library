/*=============================================================================
	Copyright (c) 2011 Jan Frederick Eick

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/

#include "test.hpp"
#include "uint_radix.hpp"
#include "../x3.hpp"
#include <climits>
#include <cstring>

UNITTESTDEF(x3_test_uint_radix)
{
	using spirit_test::test;
	using spirit_test::test_attr;

	using boost::spirit::x3::uint_parser;

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 3)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 3, 1, -1>             base3_parser;

		_ASSERT(test("210112221200",                 base3_parser));
		_ASSERT(test_attr("210112221200",            base3_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("1231",                        base3_parser));
		_ASSERT(!test_attr("1231",                   base3_parser, u));

		_ASSERT(test(max_unsigned_base3,             base3_parser));
		_ASSERT(test_attr(max_unsigned_base3,        base3_parser, u));

		_ASSERT(!test(unsigned_overflow_base3,       base3_parser));
		_ASSERT(!test_attr(unsigned_overflow_base3,  base3_parser, u));
		_ASSERT(!test(digit_overflow_base3,          base3_parser));
		_ASSERT(!test_attr(digit_overflow_base3,     base3_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 4)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;

		uint_parser<unsigned int, 4, 1, -1>             base4_parser;

		_ASSERT(test("1213210302",                   base4_parser));
		_ASSERT(test_attr("1213210302",              base4_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("1234",                        base4_parser));
		_ASSERT(!test_attr("1234",                   base4_parser, u));

		_ASSERT(test(max_unsigned_base4,             base4_parser));
		_ASSERT(test_attr(max_unsigned_base4,        base4_parser, u));
		_ASSERT(!test(digit_overflow_base4,          base4_parser));
		_ASSERT(!test_attr(digit_overflow_base4,     base4_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 5)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;

		uint_parser<unsigned int, 5, 1, -1>             base5_parser;

		_ASSERT(test("102033432",                    base5_parser));
		_ASSERT(test_attr("102033432",               base5_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("2345",                        base5_parser));
		_ASSERT(!test_attr("2345",                   base5_parser, u));

		_ASSERT(test(max_unsigned_base5,             base5_parser));
		_ASSERT(test_attr(max_unsigned_base5,        base5_parser, u));

		_ASSERT(!test(unsigned_overflow_base5,       base5_parser));
		_ASSERT(!test_attr(unsigned_overflow_base5,  base5_parser, u));
		_ASSERT(!test(digit_overflow_base5,          base5_parser));
		_ASSERT(!test_attr(digit_overflow_base5,     base5_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 6)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;

		uint_parser<unsigned int, 6, 1, -1>             base6_parser;

		_ASSERT(test("13032030",                     base6_parser));
		_ASSERT(test_attr("13032030",                base6_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("3456",                        base6_parser));
		_ASSERT(!test_attr("3456",                   base6_parser, u));

		_ASSERT(test(max_unsigned_base6,             base6_parser));
		_ASSERT(test_attr(max_unsigned_base6,        base6_parser, u));

		_ASSERT(!test(unsigned_overflow_base6,       base6_parser));
		_ASSERT(!test_attr(unsigned_overflow_base6,  base6_parser, u));
		_ASSERT(!test(digit_overflow_base6,          base6_parser));
		_ASSERT(!test_attr(digit_overflow_base6,     base6_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 7)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;

		uint_parser<unsigned int, 7, 1, -1>             base7_parser;

		_ASSERT(test("3414600",                      base7_parser));
		_ASSERT(test_attr("3414600",                 base7_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("4567",                        base7_parser));
		_ASSERT(!test_attr("4567",                   base7_parser, u));

		_ASSERT(test(max_unsigned_base7,             base7_parser));
		_ASSERT(test_attr(max_unsigned_base7,        base7_parser, u));

		_ASSERT(!test(unsigned_overflow_base7,       base7_parser));
		_ASSERT(!test_attr(unsigned_overflow_base7,  base7_parser, u));
		_ASSERT(!test(digit_overflow_base7,          base7_parser));
		_ASSERT(!test_attr(digit_overflow_base7,     base7_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 9)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;

		uint_parser<unsigned int, 9, 1, -1>             base9_parser;

		_ASSERT(test("715850",                       base9_parser));
		_ASSERT(test_attr("715850",                  base9_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("6789",                        base9_parser));
		_ASSERT(!test_attr("6789",                   base9_parser, u));

		_ASSERT(test(max_unsigned_base9,             base9_parser));
		_ASSERT(test_attr(max_unsigned_base9,        base9_parser, u));

		_ASSERT(!test(unsigned_overflow_base9,       base9_parser));
		_ASSERT(!test_attr(unsigned_overflow_base9,  base9_parser, u));
		_ASSERT(!test(digit_overflow_base9,          base9_parser));
		_ASSERT(!test_attr(digit_overflow_base9,     base9_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 11)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;

		uint_parser<unsigned int, 11, 1, -1>            base11_parser;

		_ASSERT(test("26a815",                       base11_parser));
		_ASSERT(test_attr("26a815",                  base11_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("90ab",                        base11_parser));
		_ASSERT(!test_attr("90AB",                   base11_parser, u));

		_ASSERT(test(max_unsigned_base11,            base11_parser));
		_ASSERT(test_attr(max_unsigned_base11,       base11_parser, u));

		_ASSERT(!test(unsigned_overflow_base11,      base11_parser));
		_ASSERT(!test_attr(unsigned_overflow_base11, base11_parser, u));
		_ASSERT(!test(digit_overflow_base11,         base11_parser));
		_ASSERT(!test_attr(digit_overflow_base11,    base11_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 12)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 12, 1, -1>            base12_parser;

		_ASSERT(test("185616",                       base12_parser));
		_ASSERT(test_attr("185616",                  base12_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("9abc",                        base12_parser));
		_ASSERT(!test_attr("9ABC",                   base12_parser, u));

		_ASSERT(test(max_unsigned_base12,            base12_parser));
		_ASSERT(test_attr(max_unsigned_base12,       base12_parser, u));

		_ASSERT(!test(unsigned_overflow_base12,      base12_parser));
		_ASSERT(!test_attr(unsigned_overflow_base12, base12_parser, u));
		_ASSERT(!test(digit_overflow_base12,         base12_parser));
		_ASSERT(!test_attr(digit_overflow_base12,    base12_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 13)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 13, 1, -1>            base13_parser;

		_ASSERT(test("11b140",                       base13_parser));
		_ASSERT(test_attr("11b140",                  base13_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("abcd",                        base13_parser));
		_ASSERT(!test_attr("ABCD",                   base13_parser, u));

		_ASSERT(test(max_unsigned_base13,            base13_parser));
		_ASSERT(test_attr(max_unsigned_base13,       base13_parser, u));

		_ASSERT(!test(unsigned_overflow_base13,      base13_parser));
		_ASSERT(!test_attr(unsigned_overflow_base13, base13_parser, u));
		_ASSERT(!test(digit_overflow_base13,         base13_parser));
		_ASSERT(!test_attr(digit_overflow_base13,    base13_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 14)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 14, 1, -1>            base14_parser;

		_ASSERT(test("b0870",                        base14_parser));
		_ASSERT(test_attr("b0870",                   base14_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("bcde",                        base14_parser));
		_ASSERT(!test_attr("BCDE",                   base14_parser, u));

		_ASSERT(test(max_unsigned_base14,            base14_parser));
		_ASSERT(test_attr(max_unsigned_base14,       base14_parser, u));

		_ASSERT(!test(unsigned_overflow_base14,      base14_parser));
		_ASSERT(!test_attr(unsigned_overflow_base14, base14_parser, u));
		_ASSERT(!test(digit_overflow_base14,         base14_parser));
		_ASSERT(!test_attr(digit_overflow_base14,    base14_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 15)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 15, 1, -1>            base15_parser;

		_ASSERT(test("85a7c",                        base15_parser));
		_ASSERT(test_attr("85a7c",                   base15_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("cdef",                        base15_parser));
		_ASSERT(!test_attr("CDEF",                   base15_parser, u));

		_ASSERT(test(max_unsigned_base15,            base15_parser));
		_ASSERT(test_attr(max_unsigned_base15,       base15_parser, u));

		_ASSERT(!test(unsigned_overflow_base15,      base15_parser));
		_ASSERT(!test_attr(unsigned_overflow_base15, base15_parser, u));
		_ASSERT(!test(digit_overflow_base15,         base15_parser));
		_ASSERT(!test_attr(digit_overflow_base15,    base15_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 17)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 17, 1, -1>            base17_parser;

		_ASSERT(test("515g7",                        base17_parser));
		_ASSERT(test_attr("515g7",                   base17_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("efgh",                        base17_parser));
		_ASSERT(!test_attr("EFGH",                   base17_parser, u));

		_ASSERT(test(max_unsigned_base17,            base17_parser));
		_ASSERT(test_attr(max_unsigned_base17,       base17_parser, u));

		_ASSERT(!test(unsigned_overflow_base17,      base17_parser));
		_ASSERT(!test_attr(unsigned_overflow_base17, base17_parser, u));
		_ASSERT(!test(digit_overflow_base17,         base17_parser));
		_ASSERT(!test_attr(digit_overflow_base17,    base17_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 18)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 18, 1, -1>            base18_parser;

		_ASSERT(test("40d70",                        base18_parser));
		_ASSERT(test_attr("40d70",                   base18_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("fghi",                        base18_parser));
		_ASSERT(!test_attr("FGHI",                   base18_parser, u));

		_ASSERT(test(max_unsigned_base18,            base18_parser));
		_ASSERT(test_attr(max_unsigned_base18,       base18_parser, u));

		_ASSERT(!test(unsigned_overflow_base18,      base18_parser));
		_ASSERT(!test_attr(unsigned_overflow_base18, base18_parser, u));
		_ASSERT(!test(digit_overflow_base18,         base18_parser));
		_ASSERT(!test_attr(digit_overflow_base18,    base18_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 19)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 19, 1, -1>            base19_parser;

		_ASSERT(test("34g3a",                        base19_parser));
		_ASSERT(test_attr("34g3a",                   base19_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("ghij",                        base19_parser));
		_ASSERT(!test_attr("GHIJ",                   base19_parser, u));

		_ASSERT(test(max_unsigned_base19,            base19_parser));
		_ASSERT(test_attr(max_unsigned_base19,       base19_parser, u));

		_ASSERT(!test(unsigned_overflow_base19,      base19_parser));
		_ASSERT(!test_attr(unsigned_overflow_base19, base19_parser, u));
		_ASSERT(!test(digit_overflow_base19,         base19_parser));
		_ASSERT(!test_attr(digit_overflow_base19,    base19_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 20)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 20, 1, -1>            base20_parser;

		_ASSERT(test("2d0c2",                        base20_parser));
		_ASSERT(test_attr("2d0c2",                   base20_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("hijk",                        base20_parser));
		_ASSERT(!test_attr("HIJK",                   base20_parser, u));

		_ASSERT(test(max_unsigned_base20,            base20_parser));
		_ASSERT(test_attr(max_unsigned_base20,       base20_parser, u));

		_ASSERT(!test(unsigned_overflow_base20,      base20_parser));
		_ASSERT(!test_attr(unsigned_overflow_base20, base20_parser, u));
		_ASSERT(!test(digit_overflow_base20,         base20_parser));
		_ASSERT(!test_attr(digit_overflow_base20,    base20_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 21)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 21, 1, -1>            base21_parser;

		_ASSERT(test("23h00",                        base21_parser));
		_ASSERT(test_attr("23h00",                   base21_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("ijkl",                        base21_parser));
		_ASSERT(!test_attr("IJKL",                   base21_parser, u));

		_ASSERT(test(max_unsigned_base21,            base21_parser));
		_ASSERT(test_attr(max_unsigned_base21,       base21_parser, u));

		_ASSERT(!test(unsigned_overflow_base21,      base21_parser));
		_ASSERT(!test_attr(unsigned_overflow_base21, base21_parser, u));
		_ASSERT(!test(digit_overflow_base21,         base21_parser));
		_ASSERT(!test_attr(digit_overflow_base21,    base21_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 22)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 22, 1, -1>            base22_parser;

		_ASSERT(test("1hibg",                        base22_parser));
		_ASSERT(test_attr("1hibg",                   base22_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("jklm",                        base22_parser));
		_ASSERT(!test_attr("JKLM",                   base22_parser, u));

		_ASSERT(test(max_unsigned_base22,            base22_parser));
		_ASSERT(test_attr(max_unsigned_base22,       base22_parser, u));

		_ASSERT(!test(unsigned_overflow_base22,      base22_parser));
		_ASSERT(!test_attr(unsigned_overflow_base22, base22_parser, u));
		_ASSERT(!test(digit_overflow_base22,         base22_parser));
		_ASSERT(!test_attr(digit_overflow_base22,    base22_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 23)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 23, 1, -1>            base23_parser;

		_ASSERT(test("1bjm7",                        base23_parser));
		_ASSERT(test_attr("1bjm7",                   base23_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("klmn",                        base23_parser));
		_ASSERT(!test_attr("KLMN",                   base23_parser, u));

		_ASSERT(test(max_unsigned_base23,            base23_parser));
		_ASSERT(test_attr(max_unsigned_base23,       base23_parser, u));

		_ASSERT(!test(unsigned_overflow_base23,      base23_parser));
		_ASSERT(!test_attr(unsigned_overflow_base23, base23_parser, u));
		_ASSERT(!test(digit_overflow_base23,         base23_parser));
		_ASSERT(!test_attr(digit_overflow_base23,    base23_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 24)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 24, 1, -1>            base24_parser;

		_ASSERT(test("16gci",                        base24_parser));
		_ASSERT(test_attr("16gci",                   base24_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("lmno",                        base24_parser));
		_ASSERT(!test_attr("LMNO",                   base24_parser, u));

		_ASSERT(test(max_unsigned_base24,            base24_parser));
		_ASSERT(test_attr(max_unsigned_base24,       base24_parser, u));

		_ASSERT(!test(unsigned_overflow_base24,      base24_parser));
		_ASSERT(!test_attr(unsigned_overflow_base24, base24_parser, u));
		_ASSERT(!test(digit_overflow_base24,         base24_parser));
		_ASSERT(!test_attr(digit_overflow_base24,    base24_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 25)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 25, 1, -1>            base25_parser;

		_ASSERT(test("123jh",                        base25_parser));
		_ASSERT(test_attr("123jh",                   base25_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("mnop",                        base25_parser));
		_ASSERT(!test_attr("MNOP",                   base25_parser, u));

		_ASSERT(test(max_unsigned_base25,            base25_parser));
		_ASSERT(test_attr(max_unsigned_base25,       base25_parser, u));

		_ASSERT(!test(unsigned_overflow_base25,      base25_parser));
		_ASSERT(!test_attr(unsigned_overflow_base25, base25_parser, u));
		_ASSERT(!test(digit_overflow_base25,         base25_parser));
		_ASSERT(!test_attr(digit_overflow_base25,    base25_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 26)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 26, 1, -1>            base26_parser;

		_ASSERT(test("o3f0",                         base26_parser));
		_ASSERT(test_attr("o3f0",                    base26_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("nopq",                        base26_parser));
		_ASSERT(!test_attr("NOPQ",                   base26_parser, u));

		_ASSERT(test(max_unsigned_base26,            base26_parser));
		_ASSERT(test_attr(max_unsigned_base26,       base26_parser, u));

		_ASSERT(!test(unsigned_overflow_base26,      base26_parser));
		_ASSERT(!test_attr(unsigned_overflow_base26, base26_parser, u));
		_ASSERT(!test(digit_overflow_base26,         base26_parser));
		_ASSERT(!test_attr(digit_overflow_base26,    base26_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 27)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 27, 1, -1>            base27_parser;

		_ASSERT(test("lepi",                         base27_parser));
		_ASSERT(test_attr("lepi",                    base27_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("opqr",                        base27_parser));
		_ASSERT(!test_attr("OPQR",                   base27_parser, u));

		_ASSERT(test(max_unsigned_base27,            base27_parser));
		_ASSERT(test_attr(max_unsigned_base27,       base27_parser, u));

		_ASSERT(!test(unsigned_overflow_base27,      base27_parser));
		_ASSERT(!test_attr(unsigned_overflow_base27, base27_parser, u));
		_ASSERT(!test(digit_overflow_base27,         base27_parser));
		_ASSERT(!test_attr(digit_overflow_base27,    base27_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 28)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 28, 1, -1>            base28_parser;

		_ASSERT(test("j93e",                         base28_parser));
		_ASSERT(test_attr("j93e",                    base28_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("pqrs",                        base28_parser));
		_ASSERT(!test_attr("PQRS",                   base28_parser, u));

		_ASSERT(test(max_unsigned_base28,            base28_parser));
		_ASSERT(test_attr(max_unsigned_base28,       base28_parser, u));

		_ASSERT(!test(unsigned_overflow_base28,      base28_parser));
		_ASSERT(!test_attr(unsigned_overflow_base28, base28_parser, u));
		_ASSERT(!test(digit_overflow_base28,         base28_parser));
		_ASSERT(!test_attr(digit_overflow_base28,    base28_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 29)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 29, 1, -1>            base29_parser;

		_ASSERT(test("hbd1",                         base29_parser));
		_ASSERT(test_attr("hbd1",                    base29_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("qrst",                        base29_parser));
		_ASSERT(!test_attr("QRST",                   base29_parser, u));

		_ASSERT(test(max_unsigned_base29,            base29_parser));
		_ASSERT(test_attr(max_unsigned_base29,       base29_parser, u));

		_ASSERT(!test(unsigned_overflow_base29,      base29_parser));
		_ASSERT(!test_attr(unsigned_overflow_base29, base29_parser, u));
		_ASSERT(!test(digit_overflow_base29,         base29_parser));
		_ASSERT(!test_attr(digit_overflow_base29,    base29_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 30)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 30, 1, -1>            base30_parser;

		_ASSERT(test("flbc",                         base30_parser));
		_ASSERT(test_attr("flbc",                    base30_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("rstu",                        base30_parser));
		_ASSERT(!test_attr("RSTU",                   base30_parser, u));

		_ASSERT(test(max_unsigned_base30,            base30_parser));
		_ASSERT(test_attr(max_unsigned_base30,       base30_parser, u));

		_ASSERT(!test(unsigned_overflow_base30,      base30_parser));
		_ASSERT(!test_attr(unsigned_overflow_base30, base30_parser, u));
		_ASSERT(!test(digit_overflow_base30,         base30_parser));
		_ASSERT(!test_attr(digit_overflow_base30,    base30_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 31)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 31, 1, -1>            base31_parser;

		_ASSERT(test("e7e7",                         base31_parser));
		_ASSERT(test_attr("e7e7",                    base31_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("stuv",                        base31_parser));
		_ASSERT(!test_attr("STUV",                   base31_parser, u));

		_ASSERT(test(max_unsigned_base31,            base31_parser));
		_ASSERT(test_attr(max_unsigned_base31,       base31_parser, u));

		_ASSERT(!test(unsigned_overflow_base31,      base31_parser));
		_ASSERT(!test_attr(unsigned_overflow_base31, base31_parser, u));
		_ASSERT(!test(digit_overflow_base31,         base31_parser));
		_ASSERT(!test_attr(digit_overflow_base31,    base31_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 32)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 32, 1, -1>            base32_parser;

		_ASSERT(test("cu9i",                         base32_parser));
		_ASSERT(test_attr("cu9i",                    base32_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("tuvw",                        base32_parser));
		_ASSERT(!test_attr("TUVW",                   base32_parser, u));

		_ASSERT(test(max_unsigned_base32,            base32_parser));
		_ASSERT(test_attr(max_unsigned_base32,       base32_parser, u));

		_ASSERT(!test(unsigned_overflow_base32,      base32_parser));
		_ASSERT(!test_attr(unsigned_overflow_base32, base32_parser, u));
		_ASSERT(!test(digit_overflow_base32,         base32_parser));
		_ASSERT(!test_attr(digit_overflow_base32,    base32_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 33)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 33, 1, -1>            base33_parser;

		_ASSERT(test("bqir",                         base33_parser));
		_ASSERT(test_attr("bqir",                    base33_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("uvwx",                        base33_parser));
		_ASSERT(!test_attr("UVWX",                   base33_parser, u));

		_ASSERT(test(max_unsigned_base33,            base33_parser));
		_ASSERT(test_attr(max_unsigned_base33,       base33_parser, u));

		_ASSERT(!test(unsigned_overflow_base33,      base33_parser));
		_ASSERT(!test_attr(unsigned_overflow_base33, base33_parser, u));
		_ASSERT(!test(digit_overflow_base33,         base33_parser));
		_ASSERT(!test_attr(digit_overflow_base33,    base33_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 34)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 34, 1, -1>            base34_parser;

		_ASSERT(test("aqxo",                         base34_parser));
		_ASSERT(test_attr("aqxo",                    base34_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("vwxy",                        base34_parser));
		_ASSERT(!test_attr("VWXY",                   base34_parser, u));

		_ASSERT(test(max_unsigned_base34,            base34_parser));
		_ASSERT(test_attr(max_unsigned_base34,       base34_parser, u));

		_ASSERT(!test(unsigned_overflow_base34,      base34_parser));
		_ASSERT(!test_attr(unsigned_overflow_base34, base34_parser, u));
		_ASSERT(!test(digit_overflow_base34,         base34_parser));
		_ASSERT(!test_attr(digit_overflow_base34,    base34_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 35)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 35, 1, -1>            base35_parser;

		_ASSERT(test("9vb7",                         base35_parser));
		_ASSERT(test_attr("9vb7",                    base35_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(!test("wxyz",                        base35_parser));
		_ASSERT(!test_attr("WXYZ",                   base35_parser, u));

		_ASSERT(test(max_unsigned_base35,            base35_parser));
		_ASSERT(test_attr(max_unsigned_base35,       base35_parser, u));

		_ASSERT(!test(unsigned_overflow_base35,      base35_parser));
		_ASSERT(!test_attr(unsigned_overflow_base35, base35_parser, u));
		_ASSERT(!test(digit_overflow_base35,         base35_parser));
		_ASSERT(!test_attr(digit_overflow_base35,    base35_parser, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  arbitrary radix test (base 36)
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned int u=0;
		uint_parser<unsigned int, 36, 1, -1>            base36_parser;

		_ASSERT(test("93ci",                         base36_parser));
		_ASSERT(test_attr("93ci",                    base36_parser, u));
		_ASSERT(424242 == u);

		_ASSERT(test(max_unsigned_base36,            base36_parser));
		_ASSERT(test_attr(max_unsigned_base36,       base36_parser, u));

		_ASSERT(!test(unsigned_overflow_base36,      base36_parser));
		_ASSERT(!test_attr(unsigned_overflow_base36, base36_parser, u));
		_ASSERT(!test(digit_overflow_base36,         base36_parser));
		_ASSERT(!test_attr(digit_overflow_base36,    base36_parser, u));
	}
}
