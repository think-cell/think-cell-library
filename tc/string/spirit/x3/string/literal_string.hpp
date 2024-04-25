/*=============================================================================
	Copyright (c) 2001-2014 Joel de Guzman

	Distributed under the Boost Software License, Version 1.0. (See accompanying
	file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
==============================================================================*/
#if !defined(BOOST_SPIRIT_X3_LITERAL_STRING_APR_18_2006_1125PM)
#define BOOST_SPIRIT_X3_LITERAL_STRING_APR_18_2006_1125PM

#include "../core/parser.hpp"
#include "../core/skip_over.hpp"
#include "detail/string_parse.hpp"
#include "../support/no_case.hpp"
#include "../support/utility/utf8.hpp"
#include "../../support/char_encoding/ascii.hpp"
#include "../../support/char_encoding/standard.hpp"
#include "../../support/char_encoding/standard_wide.hpp"

#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/add_reference.hpp>
#include <string>

namespace boost { namespace spirit { namespace x3
{
	template <typename String, typename Encoding,
		typename Attribute = std::basic_string<typename Encoding::char_type>>
	struct literal_string : parser<literal_string<String, Encoding, Attribute>>
	{
		typedef typename Encoding::char_type char_type;
		typedef Encoding encoding;
		typedef Attribute attribute_type;
		static bool const has_attribute =
			!is_same<unused_type, attribute_type>::value;
		static bool const handles_container = has_attribute;

		constexpr literal_string(typename add_reference< typename add_const<String>::type >::type str)
		  : str(str)
		{}

		template <typename Iterator, typename Context, typename Attribute_>
		bool parse(Iterator& first, Iterator const& last
		  , Context const& context, unused_type, Attribute_& attr) const
		{
			x3::skip_over(first, last, context);
			return detail::string_parse(str, first, last, attr, get_case_compare<encoding>(context));
		}

		String str;
	};

	namespace standard
	{
		constexpr literal_string<char const*, char_encoding::standard>
		string(char const* s)
		{
			return { s };
		}

		inline literal_string<std::basic_string<char>, char_encoding::standard>
		string(std::basic_string<char> const& s)
		{
			return { s };
		}

		inline constexpr literal_string<char const*, char_encoding::standard, unused_type>
		lit(char const* s)
		{
			return { s };
		}

		template <typename Char>
		literal_string<std::basic_string<Char>, char_encoding::standard, unused_type>
		lit(std::basic_string<Char> const& s)
		{
			return { s };
		}
	}

#ifndef BOOST_SPIRIT_NO_STANDARD_WIDE
	namespace standard_wide
	{
		constexpr literal_string<wchar_t const*, char_encoding::standard_wide>
		string(wchar_t const* s)
		{
			return { s };
		}

		inline literal_string<std::basic_string<wchar_t>, char_encoding::standard_wide>
		string(std::basic_string<wchar_t> const& s)
		{
			return { s };
		}

		constexpr literal_string<wchar_t const*, char_encoding::standard_wide, unused_type>
		lit(wchar_t const* s)
		{
			return { s };
		}

		inline literal_string<std::basic_string<wchar_t>, char_encoding::standard_wide, unused_type>
		lit(std::basic_string<wchar_t> const& s)
		{
			return { s };
		}
	}
#endif

#if defined(BOOST_SPIRIT_X3_UNICODE)
	namespace unicode
	{
		constexpr literal_string<char32_t const*, char_encoding::unicode>
		string(char32_t const* s)
		{
			return { s };
		}

		inline literal_string<std::basic_string<char32_t>, char_encoding::unicode>
		string(std::basic_string<char32_t> const& s)
		{
			return { s };
		}

		constexpr literal_string<char32_t const*, char_encoding::unicode, unused_type>
		lit(char32_t const* s)
		{
			return { s };
		}

		inline literal_string<std::basic_string<char32_t>, char_encoding::unicode, unused_type>
		lit(std::basic_string<char32_t> const& s)
		{
			return { s };
		}
	}
#endif

	namespace ascii
	{
		constexpr literal_string<wchar_t const*, char_encoding::ascii>
		string(wchar_t const* s)
		{
			return { s };
		}

		inline literal_string<std::basic_string<wchar_t>, char_encoding::ascii>
		string(std::basic_string<wchar_t> const& s)
		{
			return { s };
		}

		constexpr literal_string<char const*, char_encoding::ascii, unused_type>
		lit(char const* s)
		{
			return { s };
		}

		template <typename Char>
		literal_string<std::basic_string<Char>, char_encoding::ascii, unused_type>
		lit(std::basic_string<Char> const& s)
		{
			return { s };
		}
	}

	namespace iso8859_1
	{
		constexpr literal_string<wchar_t const*, char_encoding::iso8859_1>
		string(wchar_t const* s)
		{
			return { s };
		}

		inline literal_string<std::basic_string<wchar_t>, char_encoding::iso8859_1>
		string(std::basic_string<wchar_t> const& s)
		{
			return { s };
		}

		constexpr literal_string<char const*, char_encoding::iso8859_1, unused_type>
		lit(char const* s)
		{
			return { s };
		}

		template <typename Char>
		literal_string<std::basic_string<Char>, char_encoding::iso8859_1, unused_type>
		lit(std::basic_string<Char> const& s)
		{
			return { s };
		}
	}

	using standard::string;
	using standard::lit;
#ifndef BOOST_SPIRIT_NO_STANDARD_WIDE
	using standard_wide::string;
	using standard_wide::lit;
#endif

	template <typename String, typename Encoding, typename Attribute>
	struct get_info<literal_string<String, Encoding, Attribute>>
	{
		typedef std::string result_type;
		std::string operator()(literal_string<String, Encoding, Attribute> const& p) const
		{
			return '"' + to_utf8(p.str) + '"';
		}
	};
}}}

#endif
