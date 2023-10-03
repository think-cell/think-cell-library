
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../algorithm/element.h"
#include "../algorithm/size.h"
#include "../array.h"
#include "filter_adaptor.h"
#include "transform_adaptor.h"

namespace {
	[[maybe_unused]] void static_tests() noexcept {
		auto rngSize = tc::transform(tc::vector<int>(), [](int) noexcept { return 0; });
		static_assert(tc::has_size<decltype(rngSize)>);

		auto rngNoSize = tc::transform(tc::filter(tc::vector<int>(), [](int) noexcept { return false; }), [](int) noexcept { return 0; });
		static_assert(!tc::has_size<decltype(rngNoSize)>);

		int anNative[] = {1,2,3,4,5};
		auto anTc = tc::make_array(anNative);
		auto anTc2 = tc::make_array(anTc);
		auto anNativeTrans = tc::make_array(tc::transform(anNative, tc::identity()));
		auto anTcTrans = tc::make_array(tc::transform(anTc, tc::identity()));

		static_assert( tc::constexpr_size<decltype(anTc)>() == tc::constexpr_size<decltype(anNative)>() );
		static_assert( tc::constexpr_size<decltype(anTc2)>() == tc::constexpr_size<decltype(anNative)>() );
		static_assert( tc::constexpr_size<decltype(anNativeTrans)>() == tc::constexpr_size<decltype(anNative)>() );
		static_assert( tc::constexpr_size<decltype(anTcTrans)>() == tc::constexpr_size<decltype(anNative)>() );

		auto rngnoncopy = tc::transform(tc_as_constexpr(tc::make_array(tc::aggregate_tag, 1,2)), [](int) noexcept {
			struct NonCopyNonMoveable final {
				NonCopyNonMoveable(NonCopyNonMoveable const&) = delete;
				NonCopyNonMoveable(NonCopyNonMoveable&&) = delete;

				NonCopyNonMoveable() {}
			};
			return NonCopyNonMoveable();
		});
		tc::for_each(
			rngnoncopy,
			[](auto&& noncopy) noexcept {}
		);
		[[maybe_unused]] auto noncopy = tc::front(rngnoncopy);
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
