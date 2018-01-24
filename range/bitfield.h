//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"

#ifndef __clang__
#include <intrin.h> // _BitScanForward, _BitScanReverse
#endif

namespace tc {
	inline int bit_count(unsigned int x) noexcept {
		// http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
		// http://aggregate.org/MAGIC/#Population%20Count%20%28Ones%20Count%29
		x -= (x >> 1) & 0x55555555;
		x = ((x >> 2) & 0x33333333) + (x & 0x33333333);
		x = ((x >> 4) + x) & 0x0f0f0f0f;
		x += x >> 8;
		x += x >> 16;
		return x & 0x0000003f;
	}

	inline unsigned long index_of_least_significant_bit(unsigned long x) noexcept {
	#ifdef __clang__
		_ASSERT(0!=x);
		return __builtin_ctzl(x);
	#else
		unsigned long nIndex;
		VERIFY(_BitScanForward(&nIndex, x));
		return nIndex;
	#endif
	}

	inline unsigned long least_significant_bit(unsigned long x) noexcept {
		return x ? 1ul << tc::index_of_least_significant_bit(x) : 0;
	}

	inline unsigned long index_of_most_significant_bit(unsigned long x) noexcept {
	#ifdef __clang__
		_ASSERT(0!=x);
		return sizeof(unsigned long)*CHAR_BIT - 1 - __builtin_clzl(x);
	#else
		unsigned long nIndex;
		VERIFY(_BitScanReverse(&nIndex, x));
		return nIndex;
	#endif
	}

	inline unsigned long most_significant_bit(unsigned long x) noexcept {
		return x ? 1ul << tc::index_of_most_significant_bit(x) : 0;
	}
}
