
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt


#include "assert_defs.h"
#include "trivial_functors.h"
#include "utility.h"

#include "../unittest.h"
#include "../array.h"
#include "../algorithm/break_or_continue.h"


namespace {
	using TypeBiggerThanPointer = std::array<int,sizeof(void*)/sizeof(int) + 1>;
	auto constexpr c_typebiggerthanpointer = TypeBiggerThanPointer();

	[[maybe_unused]] void static_test_make_constexpr_function() noexcept {
		// Fits in a pointer
		{
			constexpr auto fn = tc::constexpr_function<int{}>();
			tc_static_auto_constexpr_lambda(fnOracle) = []() constexpr noexcept {
				constexpr auto _ = int();
				return _;
			}; 
			STATICASSERTSAME(decltype(fnOracle()), int);
			STATICASSERTSAME(decltype(fn()), int);
		}
		{
			struct SFoo {
				int m_n;
			};
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(SFoo{});
			tc_static_auto_constexpr_lambda(fnOracle) = []() constexpr noexcept {
				constexpr auto _ = SFoo{};
				return _;
			}; 
			STATICASSERTSAME(decltype(fnOracle()), SFoo);
			STATICASSERTSAME(decltype(fn()), SFoo);
		}
		{
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(tc::constant<10>());
			tc_static_auto_constexpr_lambda(fnOracle) = []() constexpr noexcept {
				constexpr auto _ = tc::constant<10>();
				return _;
			}; 
			STATICASSERTSAME(decltype(fnOracle()), tc::constant<10>);
			STATICASSERTSAME(decltype(fn()), tc::constant<10>);
		}
		{
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(tc::constant<tc::break_>());
			tc_static_auto_constexpr_lambda(fnOracle) = []() constexpr noexcept {
				constexpr auto _ = tc::constant<tc::break_>();
				return _;
			}; 
			STATICASSERTSAME(decltype(fnOracle()), tc::constant<tc::break_>);
			STATICASSERTSAME(decltype(fn()), tc::constant<tc::break_>);
		}

		// Doesn't fit in a pointer
		{
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(c_typebiggerthanpointer);
			tc_static_auto_constexpr_lambda(fnOracle) = []() constexpr noexcept ->decltype(auto) {
				return (c_typebiggerthanpointer);
			}; 
			STATICASSERTSAME(decltype(fnOracle()), TypeBiggerThanPointer const&);
			STATICASSERTSAME(decltype(fn()), TypeBiggerThanPointer const&);
		}
	}
}
