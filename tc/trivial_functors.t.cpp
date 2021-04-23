
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "trivial_functors.h"
#include "array.h"
#include "break_or_continue.h"
#include "utility.h"
#ifdef TC_PRIVATE
#include "Library/ErrorReporting/_Assert.h"
#endif

namespace {
	using TypeBiggerThanPointer = tc::array<int,sizeof(void*)/sizeof(int) + 1>;
	static constexpr auto c_typebiggerthanpointer = TypeBiggerThanPointer();

	[[maybe_unused]] void static_test_make_constexpr_function() noexcept {
		// Fits in a pointer
		{
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(int{});
			auto fnOracle = []() constexpr noexcept {
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
			auto fnOracle = []() constexpr noexcept {
				constexpr auto _ = SFoo{};
				return _;
			}; 
			STATICASSERTSAME(decltype(fnOracle()), SFoo);
			STATICASSERTSAME(decltype(fn()), SFoo);
		}
		{
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(INTEGRAL_CONSTANT(10)());
			auto fnOracle = []() constexpr noexcept {
				constexpr auto _ = INTEGRAL_CONSTANT(10)();
				return _;
			}; 
			STATICASSERTSAME(decltype(fnOracle()), (INTEGRAL_CONSTANT(10)));
			STATICASSERTSAME(decltype(fn()), (INTEGRAL_CONSTANT(10)));
		}
		{
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(INTEGRAL_CONSTANT(tc::break_)());
			auto fnOracle = []() constexpr noexcept {
				constexpr auto _ = INTEGRAL_CONSTANT(tc::break_)();
				return _;
			}; 
			STATICASSERTSAME(decltype(fnOracle()), (INTEGRAL_CONSTANT(tc::break_)));
			STATICASSERTSAME(decltype(fn()), (INTEGRAL_CONSTANT(tc::break_)));
		}

		// Doesn't fit in a pointer
		{
			constexpr auto fn = MAKE_CONSTEXPR_FUNCTION(c_typebiggerthanpointer);
			auto fnOracle = []() constexpr noexcept ->decltype(auto) {
				return (c_typebiggerthanpointer);
			}; 
			STATICASSERTSAME(decltype(fnOracle()), TypeBiggerThanPointer const&);
			STATICASSERTSAME(decltype(fn()), TypeBiggerThanPointer const&);
		}
	}
}
