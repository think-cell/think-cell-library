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