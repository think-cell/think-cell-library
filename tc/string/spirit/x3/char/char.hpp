/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#if !defined(BOOST_SPIRIT_X3_CHAR_APRIL_16_2006_1051AM)
#define BOOST_SPIRIT_X3_CHAR_APRIL_16_2006_1051AM

#include "any_char.hpp"
#include "../../support/char_encoding/ascii.hpp"
#include "../../support/char_encoding/iso8859_1.hpp"
#include "../../support/char_encoding/standard.hpp"
#include "../../support/char_encoding/standard_wide.hpp"

namespace boost { namespace spirit { namespace x3
{
	namespace standard
	{
		typedef any_char<char_encoding::standard> char_type;
		constexpr auto char_ = char_type{};

		constexpr literal_char<char_encoding::standard, unused_type>
		lit(char ch)
		{
			return { ch };
		}

		constexpr literal_char<char_encoding::standard, unused_type>
		lit(wchar_t ch)
		{
			return { ch };
		}

	}

	using standard::char_type;
	using standard::char_;
	using standard::lit;

#ifndef BOOST_SPIRIT_NO_STANDARD_WIDE
	namespace standard_wide
	{
		typedef any_char<char_encoding::standard_wide> char_type;
		constexpr auto char_ = char_type{};

		constexpr literal_char<char_encoding::standard_wide, unused_type>
		lit(wchar_t ch)
		{
			return { ch };
		}
	}
#endif

	namespace ascii
	{
		typedef any_char<char_encoding::ascii> char_type;
		constexpr auto char_ = char_type{};

		constexpr literal_char<char_encoding::ascii, unused_type>
		lit(char ch)
		{
			return { ch };
		}

		constexpr literal_char<char_encoding::ascii, unused_type>
		lit(wchar_t ch)
		{
			return { ch };
		}
	}

	namespace iso8859_1
	{
		typedef any_char<char_encoding::iso8859_1> char_type;
		constexpr auto char_ = char_type{};

		constexpr literal_char<char_encoding::iso8859_1, unused_type>
		lit(char ch)
		{
			return { ch };
		}

		constexpr literal_char<char_encoding::iso8859_1, unused_type>
		lit(wchar_t ch)
		{
			return { ch };
		}
	}
}}}

#endif
