
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "value_restrictive.h"
#include "../base/assert_defs.h"
#include "../base/explicit_cast.h"
#include "../range/meta.h"
#include "../range/transform_adaptor.h"

#include <cstdint>

namespace tc {

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	[[nodiscard]] constexpr bool isasciidigit( T ch ) noexcept {
		return tc::char_ascii('0')<=ch && ch<=tc::char_ascii('9');
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciixdigit( T ch ) noexcept {
		return isasciidigit(ch)
			|| (tc::char_ascii('A')<=ch && ch<=tc::char_ascii('F'))
			|| (tc::char_ascii('a')<=ch && ch<=tc::char_ascii('f'));
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciiupper( T ch ) noexcept {
		return tc::char_ascii('A')<=ch && ch<=tc::char_ascii('Z');
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciilower( T ch ) noexcept {
		return tc::char_ascii('a')<=ch && ch<=tc::char_ascii('z');
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciicntrl( T ch ) noexcept {
		return (tc::char_ascii('\0')<=ch && ch<=tc::char_ascii('\x1f')) || tc::char_ascii('\x7f')==ch;
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciiblank( T ch ) noexcept {
		return tc::char_ascii('\t')==ch || tc::char_ascii(' ')==ch;
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciispace( T ch ) noexcept {
		return tc::isasciiblank(ch) ||
			(tc::char_ascii('\xa')<=ch && ch<=tc::char_ascii('\xd')); // \n, \v, \f, \r
	}

	template< typename T >
	[[nodiscard]] constexpr T toasciiupper( T ch ) noexcept {
		if( tc::isasciilower(ch) ) {
			return static_cast<T>( ch-('a'-'A') );
		} else {
			return ch;
		}
	}

	template< typename T >
	[[nodiscard]] constexpr T toasciilower( T ch ) noexcept {
		if( tc::isasciiupper(ch) ) {
			return static_cast<T>( ch+('a'-'A') );
		} else {
			return ch;
		}
	}

	template<typename Rng>
	decltype(auto) transform_asciiupper(Rng&& rng) noexcept { // return_decltype_noexcept in C++20
		return tc::transform( std::forward<Rng>(rng), tc_fn(tc::toasciiupper) );
	}

	template<typename Rng>
	decltype(auto) transform_asciilower(Rng&& rng) noexcept { // return_decltype_noexcept in C++20
		return tc::transform( std::forward<Rng>(rng), tc_fn(tc::toasciilower) );
	}

	namespace rfc3986 {
		// https://tools.ietf.org/html/rfc3986#appendix-A:

		template<typename T>
		bool is_unreserved(T ch) noexcept {
			// unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
			return tc::isasciilower(ch)
				|| tc::isasciiupper(ch)
				|| tc::isasciidigit(ch)
				|| tc::char_ascii('-')==ch
				|| tc::char_ascii('.')==ch
				|| tc::char_ascii('_')==ch
				|| tc::char_ascii('~')==ch;
		};

		template<typename T>
		bool is_subdelim(T ch) noexcept {
			// sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
			return tc::char_ascii('!')==ch
				|| tc::char_ascii('$')==ch
				|| tc::char_ascii('&')==ch
				|| tc::char_ascii('\'')==ch
				|| tc::char_ascii('(')==ch
				|| tc::char_ascii(')')==ch
				|| tc::char_ascii('*')==ch
				|| tc::char_ascii('+')==ch
				|| tc::char_ascii(',')==ch
				|| tc::char_ascii(';')==ch
				|| tc::char_ascii('=')==ch;
		};

		template<typename T>
		bool is_unencoded_pchar(T ch) noexcept {
			//pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
			return tc::rfc3986::is_unreserved(ch)
				|| tc::rfc3986::is_subdelim(ch)
				|| tc::char_ascii(':')==ch
				|| tc::char_ascii('@')==ch;
		};
	}

	namespace ascii_byte_literals {
		constexpr auto operator "" _asc(char ch) noexcept {
			// Don't use hex or octal escapes; use the char code directly. For example, use 0x80 instead of '\x80'_asc
			// Other ASCII control characters may be added here as exceptions.
			_ASSERTE((ch >= 0x20 && ch <= 0x7E) || ch == '\r' || ch == '\n' || ch == '\t');

			return tc::to_underlying(ch);
		}
	}
}
