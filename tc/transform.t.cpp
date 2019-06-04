
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.h"
#include "range.t.h"
#include "size.h"

namespace {
	[[maybe_unused]] void static_tests() noexcept {
		auto rngSize = tc::transform(tc::vector<int>(), [](int) noexcept { return 0; });
		static_assert(tc::has_size<decltype(rngSize)>::value);

		auto rngNoSize = tc::transform(tc::filter(tc::vector<int>(), [](int) noexcept { return false; }), [](int) noexcept { return 0; });
		static_assert(!tc::has_size<decltype(rngNoSize)>::value);

		int anNative[] = {1,2,3,4,5};
		auto anTc = tc::make_array(anNative);
		auto anTc2 = tc::make_array(anTc);
		auto anNativeTrans = tc::make_array(tc::transform(anNative, tc::identity()));
		auto anTcTrans = tc::make_array(tc::transform(anTc, tc::identity()));

		static_assert(
			tc::constexpr_size<tc::remove_cvref_t<decltype(anTc)>>::value==
			tc::constexpr_size<tc::remove_cvref_t<decltype(anNative)>>::value
		);
		static_assert(
			tc::constexpr_size<tc::remove_cvref_t<decltype(anTc2)>>::value==
			tc::constexpr_size<tc::remove_cvref_t<decltype(anNative)>>::value
		);
		static_assert(
			tc::constexpr_size<tc::remove_cvref_t<decltype(anNativeTrans)>>::value==
			tc::constexpr_size<tc::remove_cvref_t<decltype(anNative)>>::value
		);
		static_assert(
			tc::constexpr_size<tc::remove_cvref_t<decltype(anTcTrans)>>::value==
			tc::constexpr_size<tc::remove_cvref_t<decltype(anNative)>>::value
		);
	}
}

UNITTESTDEF(vector_int_ref_need_sfinae_transform) {
	tc::vector<int> vecn{1,2,3};
	auto rgntrnsfn = tc::transform(vecn, [](int& n) noexcept {return n*n;});
	auto it = tc::begin(rgntrnsfn);
	_ASSERTEQUAL(*it++,1);
	_ASSERTEQUAL(*it++,4);
	_ASSERTEQUAL(*it++,9);
}

UNITTESTDEF(replace_if_prvalue_range_reference) {
	auto pred = []( auto const ch ) noexcept {
		return tc::find_first< tc::return_bool >( "aeiou", ch );
	};
	auto const rng1 = tc::replace_if("hello world!", pred, 'x');
	auto const rng2 = tc::replace_if(tc::transform("hello world!", [](auto const ch) noexcept { return ch; }), pred, 'x');

	STATICASSERTSAME(tc::range_reference_t<decltype(rng1)>, char const&);
	STATICASSERTSAME(tc::range_reference_t<decltype(rng2)>, char);

	TEST_RANGE_EQUAL(rng1, "hxllx wxrld!");
	TEST_RANGE_EQUAL(rng2, "hxllx wxrld!");
}
