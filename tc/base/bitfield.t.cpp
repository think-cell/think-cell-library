
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt


#include "assert_defs.h"
#include "bitfield.h"
#include "../unittest.h"

namespace {
	auto constexpr nAllBitsSet = static_cast<unsigned int>(-1);
}

STATICASSERTEQUAL( tc::index_of_least_significant_bit(nAllBitsSet), 0 );
STATICASSERTEQUAL( tc::index_of_least_significant_bit(0x00000001u), 0 );
STATICASSERTEQUAL( tc::index_of_least_significant_bit(0x00018000u), 15 );
STATICASSERTEQUAL( tc::index_of_least_significant_bit(0x000ff000u), 12 );
STATICASSERTEQUAL( tc::index_of_least_significant_bit(0x80000000u), 31 );
STATICASSERTEQUAL( tc::index_of_least_significant_bit(0x80000001u), 0 );

STATICASSERTEQUAL( tc::least_significant_bit(0u), 0u );
STATICASSERTEQUAL( tc::least_significant_bit(nAllBitsSet), 1u );
STATICASSERTEQUAL( tc::least_significant_bit(0x00000001u), 0x00000001u );
STATICASSERTEQUAL( tc::least_significant_bit(0x00018000u), 0x00008000u );
STATICASSERTEQUAL( tc::least_significant_bit(0x000ff000u), 0x00001000u );
STATICASSERTEQUAL( tc::least_significant_bit(0x80000000u), 0x80000000u );
STATICASSERTEQUAL( tc::least_significant_bit(0x80000001u), 0x00000001u );

STATICASSERTEQUAL( tc::index_of_most_significant_bit(nAllBitsSet), std::numeric_limits<unsigned int>::digits - 1 );
STATICASSERTEQUAL( tc::index_of_most_significant_bit(0x00000001u), 0 );
STATICASSERTEQUAL( tc::index_of_most_significant_bit(0x00018000u), 16 );
STATICASSERTEQUAL( tc::index_of_most_significant_bit(0x000ff000u), 19 );
STATICASSERTEQUAL( tc::index_of_most_significant_bit(0x80000000u), 31 );
STATICASSERTEQUAL( tc::index_of_most_significant_bit(0x80000001u), 31 );

STATICASSERTEQUAL( tc::most_significant_bit(0u), 0u );
STATICASSERTEQUAL( tc::most_significant_bit(nAllBitsSet), 1u << ( sizeof(nAllBitsSet)*CHAR_BIT-1 ) );
STATICASSERTEQUAL( tc::most_significant_bit(0x00000001u), 0x00000001u );
STATICASSERTEQUAL( tc::most_significant_bit(0x00018000u), 0x00010000u );
STATICASSERTEQUAL( tc::most_significant_bit(0x000ff000u), 0x00080000u );
STATICASSERTEQUAL( tc::most_significant_bit(0x80000000u), 0x80000000u );
STATICASSERTEQUAL( tc::most_significant_bit(0x80000001u), 0x80000000u );
