
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "round.h"

UNITTESTDEF( rounding_cast ) {
	STATICASSERTSAME(decltype(tc::rounding_cast<int>(1, tc::roundFLOOR)), int&&, "tc::rounding_cast<T> doesn't return T");
	STATICASSERTSAME(decltype(tc::rounding_cast<int>(1.0, tc::roundFLOOR)), int, "tc::rounding_cast<T> doesn't return T");
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundCEIL), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.0, tc::roundNEAREST), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundCEIL), 2);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5, tc::roundNEAREST), 2);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundFLOOR), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundCEIL), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1, tc::roundNEAREST), 1);
	_ASSERTEQUAL(tc::rounding_cast<int>(1.5), 2);
}

STATICASSERTSAME(tc::int_value_least_t<0>, tc::int_least_t<7>);
STATICASSERTSAME(tc::uint_value_least_t<0>, tc::uint_least_t<8>);
STATICASSERTSAME(tc::int_value_least_t<0ull>, tc::int_least_t<7>);
STATICASSERTSAME(tc::uint_value_least_t<0ll>, tc::uint_least_t<8>);
STATICASSERTSAME(tc::int_value_least_t<127>, tc::int_least_t<7>);
STATICASSERTSAME(tc::int_value_least_t<-128>, tc::int_least_t<7>);
STATICASSERTSAME(tc::int_value_least_t<127ull>, tc::int_least_t<7>);
STATICASSERTSAME(tc::int_value_least_t<-128ll>, tc::int_least_t<7>);
STATICASSERTSAME(tc::uint_value_least_t<255>, tc::uint_least_t<8>);
STATICASSERTSAME(tc::uint_value_least_t<255ll>, tc::uint_least_t<8>);
STATICASSERTSAME(tc::int_value_least_t<128>, tc::int_least_t<15>);
STATICASSERTSAME(tc::int_value_least_t<-129>, tc::int_least_t<15>);
STATICASSERTSAME(tc::int_value_least_t<128ull>, tc::int_least_t<15>);
STATICASSERTSAME(tc::int_value_least_t<-129ll>, tc::int_least_t<15>);
STATICASSERTSAME(tc::uint_value_least_t<256>, tc::uint_least_t<16>);
STATICASSERTSAME(tc::uint_value_least_t<256ll>, tc::uint_least_t<16>);
STATICASSERTSAME(tc::int_value_least_t<std::numeric_limits<std::int64_t>::max()>, tc::int_least_t<63>);
STATICASSERTSAME(tc::int_value_least_t<std::numeric_limits<std::int64_t>::lowest()>, tc::int_least_t<63>);
STATICASSERTSAME(tc::uint_value_least_t<std::numeric_limits<std::uint64_t>::max()>, tc::uint_least_t<64>);
STATICASSERTSAME(tc::uint_value_least_t<std::numeric_limits<std::uint64_t>::lowest()>, tc::uint_least_t<8>);

namespace {
	template<typename TExpected>
	consteval auto test_mul(auto lhs, auto rhs) noexcept {
		auto nResult=tc::mul(lhs, rhs);
		STATICASSERTSAME(decltype(nResult), TExpected);
		return nResult;
	}
}

static_assert(15==test_mul<tc::int_least_t<15>>(tc::explicit_cast<std::int8_t>(-3), tc::explicit_cast<std::int8_t>(-5)));
static_assert(-15==test_mul<tc::int_least_t<15>>(tc::explicit_cast<std::int8_t>(-3), tc::explicit_cast<std::uint8_t>(5)));
static_assert(15==test_mul<tc::uint_least_t<16>>(tc::explicit_cast<std::uint8_t>(3), tc::explicit_cast<std::uint8_t>(5)));

static_assert(4.5==test_mul<double>(1.5, tc::constant<3>()));
static_assert(4.5==test_mul<double>(tc::constant<3>(), 1.5));
static_assert(5==test_mul<int>(5, tc::constant<1>()));
static_assert(5==test_mul<int>(tc::constant<1>(), 5));
static_assert(0==test_mul<tc::uint_least_t<8>>(1500, tc::constant<0>()));
static_assert(0==test_mul<tc::uint_least_t<8>>(tc::constant<0>(), -1500));
static_assert(15==test_mul<tc::uint_least_t<8>>(tc::constant<3>(), tc::constant<5>()));
static_assert(15==test_mul<tc::uint_least_t<8>>(tc::constant<-3>(), tc::constant<-5>()));
static_assert(-15==test_mul<tc::int_least_t<7>>(tc::constant<3>(), tc::constant<-5>()));
static_assert(1500==test_mul<tc::uint_least_t<16>>(tc::constant<30>(), tc::constant<50>()));
static_assert(-1500==test_mul<tc::int_least_t<15>>(tc::constant<-30>(), tc::constant<50>()));
static_assert(15==test_mul<tc::int_least_t<15>>(tc::explicit_cast<std::int8_t>(5), tc::constant<3>()));
static_assert(15==test_mul<tc::uint_least_t<16>>(tc::explicit_cast<std::uint8_t>(5), tc::constant<3>()));
static_assert(-15==test_mul<tc::int_least_t<15>>(tc::explicit_cast<std::uint8_t>(5), tc::constant<-3>()));
static_assert(15==test_mul<tc::int_least_t<15>>(tc::constant<-3>(), tc::explicit_cast<std::int8_t>(-5)));
