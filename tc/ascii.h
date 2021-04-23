
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "meta.h"
#include "explicit_cast.h"
#include "transform.h"

#include <cstdint>

namespace tc {

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	[[nodiscard]] constexpr bool isasciidigit( T ch ) noexcept {
		return tc::explicit_cast<T>('0')<=ch && ch<=tc::explicit_cast<T>('9');
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciixdigit( T ch ) noexcept {
		return isasciidigit(ch)
			|| (tc::explicit_cast<T>('A')<=ch && ch<=tc::explicit_cast<T>('F'))
			|| (tc::explicit_cast<T>('a')<=ch && ch<=tc::explicit_cast<T>('f'));
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciiupper( T ch ) noexcept {
		return tc::explicit_cast<T>('A')<=ch && ch<=tc::explicit_cast<T>('Z');
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciilower( T ch ) noexcept {
		return tc::explicit_cast<T>('a')<=ch && ch<=tc::explicit_cast<T>('z');
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciicntrl( T ch ) noexcept {
		return (tc::explicit_cast<T>('\0')<=ch && ch<=tc::explicit_cast<T>('\x1f')) || tc::explicit_cast<T>('\x7f')==ch;
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciiblank( T ch ) noexcept {
		return tc::explicit_cast<T>('\t')==ch || tc::explicit_cast<T>(' ')==ch;
	}

	template< typename T >
	[[nodiscard]] constexpr bool isasciispace( T ch ) noexcept {
		return tc::isasciiblank(ch) ||
			(tc::explicit_cast<T>('\xa')<=ch && ch<=tc::explicit_cast<T>('\xd')); // \n, \v, \f, \r
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
		return tc::transform( std::forward<Rng>(rng), TC_FN(tc::toasciiupper) );
	}

	template<typename Rng>
	decltype(auto) transform_asciilower(Rng&& rng) noexcept { // return_decltype_noexcept in C++20
		return tc::transform( std::forward<Rng>(rng), TC_FN(tc::toasciilower) );
	}

	namespace rfc3986 {
		// https://tools.ietf.org/html/rfc3986#appendix-A:

		template<typename T>
		bool is_unreserved(T ch) noexcept {
			// unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
			return tc::isasciilower(ch)
				|| tc::isasciiupper(ch)
				|| tc::isasciidigit(ch)
				|| tc::explicit_cast<T>('-')==ch
				|| tc::explicit_cast<T>('.')==ch
				|| tc::explicit_cast<T>('_')==ch
				|| tc::explicit_cast<T>('~')==ch;
		};

		template<typename T>
		bool is_subdelim(T ch) noexcept {
			// sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
			return tc::explicit_cast<T>('!')==ch
				|| tc::explicit_cast<T>('$')==ch
				|| tc::explicit_cast<T>('&')==ch
				|| tc::explicit_cast<T>('\'')==ch
				|| tc::explicit_cast<T>('(')==ch
				|| tc::explicit_cast<T>(')')==ch
				|| tc::explicit_cast<T>('*')==ch
				|| tc::explicit_cast<T>('+')==ch
				|| tc::explicit_cast<T>(',')==ch
				|| tc::explicit_cast<T>(';')==ch
				|| tc::explicit_cast<T>('=')==ch;
		};

		template<typename T>
		bool is_unencoded_pchar(T ch) noexcept {
			//pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
			return tc::rfc3986::is_unreserved(ch)
				|| tc::rfc3986::is_subdelim(ch)
				|| tc::explicit_cast<T>(':')==ch
				|| tc::explicit_cast<T>('@')==ch;
		};
	}

	namespace ascii_byte_literals {
		constexpr auto operator "" _asc(char ch) noexcept {
			// Don't use hex or octal escapes; use the char code directly. For example, use 0x80 instead of '\x80'_asc
			// Other ASCII control characters may be added here as exceptions.
			_ASSERTE((ch >= 0x20 && ch <= 0x7E) || ch == '\r' || ch == '\n' || ch == '\t');

			return tc::underlying_cast(ch);
		}
	}
}
