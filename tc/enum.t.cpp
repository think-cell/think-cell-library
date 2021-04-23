
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "enum.h"
#include "enumset.h"
#include "range.t.h"

#define TEST_ENUM(offset, constants, underlying) \
	namespace { namespace BOOST_PP_CAT(enum_test_, __COUNTER__) { \
		DEFINE_ENUM_WITH_OFFSET(Enum, en, offset, constants); \
		static_assert( std::is_signed<std::underlying_type_t<Enum>>::value ); \
		STATICASSERTEQUAL( sizeof(std::underlying_type_t<Enum>), sizeof(underlying) ); \
		STATICASSERTEQUAL( tc::underlying_cast(tc::contiguous_enum<Enum>::begin()), offset ); \
		STATICASSERTEQUAL( tc::underlying_cast(tc::contiguous_enum<Enum>::end()), offset + BOOST_PP_SEQ_SIZE(constants) ); \
	} }

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused" // unused functions/variables defined in unnamed namespace
#endif

TEST_ENUM(0, (a), std::int8_t);
TEST_ENUM(126, (a), std::int8_t);
TEST_ENUM(-128, (a), std::int8_t);
TEST_ENUM(127, (a), std::int16_t);
TEST_ENUM(-129, (a), std::int16_t);
TEST_ENUM(-0x8000, (a), std::int16_t);
TEST_ENUM(-0x10000, (a), std::int32_t);
TEST_ENUM(0x10000, (a), std::int32_t);
TEST_ENUM(-0x100000000, (a), std::int64_t);
TEST_ENUM(0x100000000, (a), std::int64_t);

#define CONSTANTS4(pre) (pre ## 0)(pre ## 1)(pre ## 2)(pre ## 3)
#define CONSTANTS16(pre) CONSTANTS4(pre ## 0)CONSTANTS4(pre ## 1)CONSTANTS4(pre ## 2)CONSTANTS4(pre ## 3)
#define CONSTANTS64(pre) CONSTANTS16(pre ## 0)CONSTANTS16(pre ## 1)CONSTANTS16(pre ## 2)CONSTANTS16(pre ## 3)
#define CONSTANTS192 CONSTANTS64(a ## 0)CONSTANTS64(a ## 1)CONSTANTS64(a ## 2)

TEST_ENUM(-128, CONSTANTS192, std::int8_t);
TEST_ENUM(-129, CONSTANTS192, std::int16_t);
TEST_ENUM(0, CONSTANTS192, std::int16_t);

namespace {
DEFINE_ENUM_WITH_OFFSET(ETest, etest, 1234, (A)(B)(C)(D)(E)(F));
constexpr auto etestEND = tc::contiguous_enum<ETest>::end();

UNITTESTDEF(enumset_index_inc_dec) {
	tc::enumset<ETest> setetest;
	setetest |= etestB;
	setetest |= etestC;
	setetest |= etestE;

	auto itetest = tc::begin(setetest);
	_ASSERTEQUAL(*itetest++, etestB);
	_ASSERTEQUAL(*itetest++, etestC);
	_ASSERTEQUAL(*itetest++, etestE);
	_ASSERTEQUAL(itetest, tc::end(setetest));
	_ASSERTEQUAL(*--itetest, etestE);
	_ASSERTEQUAL(*--itetest, etestC);
	_ASSERTEQUAL(*--itetest, etestB);
	_ASSERTEQUAL(itetest, tc::begin(setetest));
}

}
#ifdef __clang__
#pragma clang diagnostic pop
#endif
