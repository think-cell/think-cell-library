
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "bool_cast.h"

#ifndef __clang__
#include <intrin.h> // _BitScanForward, _BitScanReverse
#endif

#include <climits>

namespace tc {
	[[nodiscard]] inline int bit_count(unsigned int x) noexcept {
		// http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
		// http://aggregate.org/MAGIC/#Population%20Count%20%28Ones%20Count%29
		x -= (x >> 1) & 0x55555555;
		x = ((x >> 2) & 0x33333333) + (x & 0x33333333);
		x = ((x >> 4) + x) & 0x0f0f0f0f;
		x += x >> 8;
		x += x >> 16;
		return x & 0x0000003f;
	}

	[[nodiscard]] inline unsigned long index_of_least_significant_bit(unsigned long x) noexcept {
	#ifdef __clang__
		_ASSERT(0!=x);
		return __builtin_ctzl(x);
	#else
		unsigned long nIndex;
		VERIFY(_BitScanForward(&nIndex, x));
		return nIndex;
	#endif
	}

	[[nodiscard]] inline unsigned long least_significant_bit(unsigned long x) noexcept {
		if( x ) {
			return 1ul << tc::index_of_least_significant_bit(x);
		} else {
			return 0;
		}
	}

	[[nodiscard]] inline unsigned long index_of_most_significant_bit(unsigned long x) noexcept {
	#ifdef __clang__
		_ASSERT(0!=x);
		return sizeof(unsigned long)*CHAR_BIT - 1 - __builtin_clzl(x);
	#else
		unsigned long nIndex;
		VERIFY(_BitScanReverse(&nIndex, x));
		return nIndex;
	#endif
	}

	[[nodiscard]] inline unsigned long most_significant_bit(unsigned long x) noexcept {
		if( x ) {
			return 1ul << tc::index_of_most_significant_bit(x);
		} else {
			return 0;
		}
	}
}
