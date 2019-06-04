
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "explicit_cast.h"
#include "transform.h"

#include <cstdint>

namespace tc {

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	bool isasciidigit( T ch ) noexcept {
		return tc::explicit_cast<T>('0')<=ch && ch<=tc::explicit_cast<T>('9');
	}
	DEFINE_FN( isasciidigit )

	template< typename T >
	bool isasciiupper( T ch ) noexcept {
		return tc::explicit_cast<T>('A')<=ch && ch<=tc::explicit_cast<T>('Z');
	}
	DEFINE_FN( isasciiupper )

	template< typename T >
	bool isasciilower( T ch ) noexcept {
		return tc::explicit_cast<T>('a')<=ch && ch<=tc::explicit_cast<T>('z');
	}
	DEFINE_FN( isasciilower )

	template< typename T >
	bool isasciicntrl( T ch ) noexcept {
		return (tc::explicit_cast<T>('\0')<=ch && ch<=tc::explicit_cast<T>('\x1f')) || tc::explicit_cast<T>('\x7f')==ch;
	}
	DEFINE_FN(isasciicntrl)

	template< typename T >
	bool isasciiblank( T ch ) noexcept {
		return tc::explicit_cast<T>('\t')==ch || tc::explicit_cast<T>(' ')==ch;
	}

	template< typename T >
	bool isasciispace( T ch ) noexcept {
		return tc::isasciiblank(ch) ||
			(tc::explicit_cast<T>('\xa')<=ch && ch<=tc::explicit_cast<T>('\xd')); // \n, \v, \f, \r
	}
	DEFINE_FN(isasciispace)

	template< typename T >
	T toasciiupper( T ch ) noexcept {
		if( isasciilower(ch) ) {
			return static_cast<T>( ch-('a'-'A') );
		} else {
			return ch;
		}
	}
	DEFINE_FN(toasciiupper)

	template< typename T >
	T toasciilower( T ch ) noexcept {
		if( isasciiupper(ch) ) {
			return static_cast<T>( ch+('a'-'A') );
		} else {
			return ch;
		}
	}
	DEFINE_FN(toasciilower)

	template<typename Rng>
	auto transform_asciiupper(Rng&& rng) noexcept
		return_decltype( transform( std::forward<Rng>(rng), tc::fn_toasciiupper() ))

	template<typename Rng>
	auto transform_asciilower(Rng&& rng) noexcept
		return_decltype( transform( std::forward<Rng>(rng), tc::fn_toasciilower() ))
}
