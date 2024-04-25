/*=============================================================================
	Copyright (c) 2001-2015 Joel de Guzman
	Copyright (c) 2001-2011 Hartmut Kaiser
	Copyright (c) 2011      Bryce Lelbach

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
=============================================================================*/
#include "uint.hpp"
#include "../x3.hpp"

UNITTESTDEF(x3_test_uint1)
{
	using spirit_test::test;
	using spirit_test::test_attr;
	///////////////////////////////////////////////////////////////////////////
	//  unsigned tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::uint_;
		unsigned u=0;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(uint_);

		_ASSERT(test("123456", uint_));
		_ASSERT(test_attr("123456", uint_, u));
		_ASSERT(u == 123456);

		_ASSERT(test(max_unsigned, uint_));
		_ASSERT(test_attr(max_unsigned, uint_, u));
		_ASSERT(u == UINT_MAX);

		_ASSERT(!test(unsigned_overflow, uint_));
		_ASSERT(!test_attr(unsigned_overflow, uint_, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  binary tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::bin;
		unsigned u=0;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(bin);

		_ASSERT(test("11111110", bin));
		_ASSERT(test_attr("11111110", bin, u));
		_ASSERT(u == 0xFE);

		_ASSERT(test(max_binary, bin));
		_ASSERT(test_attr(max_binary, bin, u));
		_ASSERT(u == UINT_MAX);

		_ASSERT(!test(binary_overflow, bin));
		_ASSERT(!test_attr(binary_overflow, bin, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  octal tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::oct;
		unsigned u=0;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(oct);

		_ASSERT(test("12545674515", oct));
		_ASSERT(test_attr("12545674515", oct, u));
		_ASSERT(u == 012545674515);

		_ASSERT(test(max_octal, oct));
		_ASSERT(test_attr(max_octal, oct, u));
		_ASSERT(u == UINT_MAX);

		_ASSERT(!test(octal_overflow, oct));
		_ASSERT(!test_attr(octal_overflow, oct, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  hex tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::hex;
		unsigned u=0;

		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(hex);

		_ASSERT(test("95BC8DF", hex));
		_ASSERT(test_attr("95BC8DF", hex, u));
		_ASSERT(u == 0x95BC8DF);

		_ASSERT(test("abcdef12", hex));
		_ASSERT(test_attr("abcdef12", hex, u));
		_ASSERT(u == 0xabcdef12);

		_ASSERT(test(max_hex, hex));
		_ASSERT(test_attr(max_hex, hex, u));
		_ASSERT(u == UINT_MAX);

		_ASSERT(!test(hex_overflow, hex));
		_ASSERT(!test_attr(hex_overflow, hex, u));
	}

	///////////////////////////////////////////////////////////////////////////
	//  limited fieldwidth
	///////////////////////////////////////////////////////////////////////////
	{
		unsigned u=0;
		using boost::spirit::x3::uint_parser;

		constexpr uint_parser<unsigned, 10, 1, 3> uint3{};
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(uint3);
		_ASSERT(test("123456", uint3, false));
		_ASSERT(test_attr("123456", uint3, u, false));
		_ASSERT(u == 123);

		constexpr uint_parser<unsigned, 10, 2, 4> uint4{};
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(uint4);
		_ASSERT(test("123456", uint4, false));
		_ASSERT(test_attr("123456", uint4, u, false));
		_ASSERT(u == 1234);

		char const * first = "0000000";
		char const * last  = first + std::strlen(first);
		constexpr uint_parser<unsigned, 10, 4, 4> uint_exact4{};
		BOOST_SPIRIT_ASSERT_CONSTEXPR_CTORS(uint_exact4);
		_ASSERT(boost::spirit::x3::parse(first, last, uint_exact4, u)
			&& first != last && (last-first == 3) && u == 0);

		first = "0001400";
		last  = first + std::strlen(first);
		_ASSERT(boost::spirit::x3::parse(first, last, uint_exact4, u)
			&& first != last && (last-first == 3) && u == 1);

		_ASSERT(!test("1", uint4));
		_ASSERT(!test_attr("1", uint4, u));
		_ASSERT(test_attr("014567", uint4, u, false) && u == 145);
	}

	///////////////////////////////////////////////////////////////////////////
	//  action tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::_attr;
		using boost::spirit::x3::uint_;
		using boost::spirit::x3::ascii::space;
		int n;

		auto f = [&](auto& ctx){ n = _attr(ctx); };

		_ASSERT(test("123", uint_[f]));
		_ASSERT(n == 123);
		_ASSERT(test("   456", uint_[f], space));
		_ASSERT(n == 456);
	}

	///////////////////////////////////////////////////////////////////////////
	// Check overflow is parse error
	///////////////////////////////////////////////////////////////////////////
	{
		boost::spirit::x3::uint_parser<boost::uint8_t> uint8_;
		boost::uint8_t u8;

		_ASSERT(!test_attr("999", uint8_, u8));
		_ASSERT(!test_attr("256", uint8_, u8));
		_ASSERT(test_attr("255", uint8_, u8));

		boost::spirit::x3::uint_parser<boost::uint16_t> uint16_;
		boost::uint16_t u16;

		_ASSERT(!test_attr("99999", uint16_, u16));
		_ASSERT(!test_attr("65536", uint16_, u16));
		_ASSERT(test_attr("65535", uint16_, u16));

		boost::spirit::x3::uint_parser<boost::uint32_t> uint32_;
		boost::uint32_t u32;

		_ASSERT(!test_attr("9999999999", uint32_, u32));
		_ASSERT(!test_attr("4294967296", uint32_, u32));
		_ASSERT(test_attr("4294967295", uint32_, u32));

		boost::spirit::x3::uint_parser<boost::int8_t> u_int8_;

		_ASSERT(!test_attr("999", u_int8_, u8));
		_ASSERT(!test_attr("-1", u_int8_, u8));
		_ASSERT(!test_attr("128", u_int8_, u8));
		_ASSERT(test_attr("127", u_int8_, u8));
		_ASSERT(test_attr("0", u_int8_, u8));

		boost::spirit::x3::uint_parser<boost::int16_t> u_int16_;

		_ASSERT(!test_attr("99999", u_int16_, u16));
		_ASSERT(!test_attr("-1", u_int16_, u16));
		_ASSERT(!test_attr("32768", u_int16_, u16));
		_ASSERT(test_attr("32767", u_int16_, u16));
		_ASSERT(test_attr("0", u_int16_, u16));

		boost::spirit::x3::uint_parser<boost::int32_t> u_int32_;

		_ASSERT(!test_attr("9999999999", u_int32_, u32));
		_ASSERT(!test_attr("-1", u_int32_, u32));
		_ASSERT(!test_attr("2147483648", u_int32_, u32));
		_ASSERT(test_attr("2147483647", u_int32_, u32));
		_ASSERT(test_attr("0", u_int32_, u32));
	}

	///////////////////////////////////////////////////////////////////////////
	//  custom uint tests
	///////////////////////////////////////////////////////////////////////////
	{
		using boost::spirit::x3::uint_;
		using boost::spirit::x3::uint_parser;
		custom_uint u;

		_ASSERT(test_attr("123456", uint_, u));
		uint_parser<custom_uint, 10, 1, 2> uint2;
		_ASSERT(test_attr("12", uint2, u));
	}
}
