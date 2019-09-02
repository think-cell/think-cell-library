
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"

#include "bitfield.h"

UNITTESTDEF( bitfield ) {
	auto const nAllBitsSet = static_cast<unsigned int>(-1);
	_ASSERTEQUAL( tc::bit_count(0), 0 );
	_ASSERTEQUAL( tc::bit_count(nAllBitsSet), sizeof(nAllBitsSet)*CHAR_BIT );
	_ASSERTEQUAL( tc::bit_count(0x00000001), 1 );
	_ASSERTEQUAL( tc::bit_count(0x00018000), 2 );
	_ASSERTEQUAL( tc::bit_count(0x000ff000), 8 );
	_ASSERTEQUAL( tc::bit_count(0x80000000), 1 );
	_ASSERTEQUAL( tc::bit_count(0x80000001), 2 );

	_ASSERTEQUAL( tc::least_significant_bit(0), 0u );
	_ASSERTEQUAL( tc::least_significant_bit(nAllBitsSet), 1u );
	_ASSERTEQUAL( tc::least_significant_bit(0x00000001), 0x00000001u );
	_ASSERTEQUAL( tc::least_significant_bit(0x00018000), 0x00008000u );
	_ASSERTEQUAL( tc::least_significant_bit(0x000ff000), 0x00001000u );
	_ASSERTEQUAL( tc::least_significant_bit(0x80000000), 0x80000000u );
	_ASSERTEQUAL( tc::least_significant_bit(0x80000001), 0x00000001u );

	_ASSERTEQUAL( tc::most_significant_bit(0), 0u );
	_ASSERTEQUAL( tc::most_significant_bit(nAllBitsSet), 1u << ( sizeof(nAllBitsSet)*CHAR_BIT-1 ) );
	_ASSERTEQUAL( tc::most_significant_bit(0x00000001), 0x00000001u );
	_ASSERTEQUAL( tc::most_significant_bit(0x00018000), 0x00010000u );
	_ASSERTEQUAL( tc::most_significant_bit(0x000ff000), 0x00080000u );
	_ASSERTEQUAL( tc::most_significant_bit(0x80000000), 0x80000000u );
	_ASSERTEQUAL( tc::most_significant_bit(0x80000001), 0x80000000u );
}
