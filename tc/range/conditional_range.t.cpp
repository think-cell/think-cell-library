
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "conditional_range.h"
#include "literal_range.h"
#include "subrange.h"
#include "transform_adaptor.h"
#include "../algorithm/find.h"

#include <algorithm>
#include <list>
#include <vector>

namespace {
	// For testing purposes, we unconditionally want to create a select_range_adaptor, and not have the common reference.
	template<typename... FuncRng>
	constexpr auto make_select_range(int n, FuncRng&&... funcrng) return_ctor_MAYTHROW(
		tc::select_range_adaptor<tc::remove_rvalue_reference_t<decltype(std::declval<FuncRng>()())>...>,
		(tc::aggregate_tag, n, std::forward<FuncRng>(funcrng)...)
	)

	template <typename Lhs, typename Rhs>
	bool iterator_range_equal(Lhs const& lhs, Rhs const& rhs) {
		return std::equal(tc::begin(lhs), tc::end(lhs), tc::begin(rhs), tc::end(rhs));
	}
}

UNITTESTDEF(conditional_range) {
	static auto constexpr verify = [](auto&& rng1, auto&& rng2) {
		auto const select_first = make_select_range(0, tc_lazy(rng1), tc_lazy(rng2));
		TEST_RANGE_EQUAL(select_first, rng1); // generator
		_ASSERT(iterator_range_equal(select_first, rng1)); // iterator

		auto const select_second = make_select_range(1, tc_lazy(rng1), tc_lazy(rng2));
		TEST_RANGE_EQUAL(select_second, rng2); // generator
		_ASSERT(iterator_range_equal(select_second, rng2)); // iterator

		auto const find_null = tc::find_first<tc::return_element_or_null>(tc::take(select_first, tc_modified(tc::begin(select_first), ++_)), tc::front(rng2));
		_ASSERT(!find_null);
		auto const find_elem = tc::find_first<tc::return_element_or_null>(tc::take(select_second, tc_modified(tc::begin(select_second), ++_)), tc::front(rng2));
		_ASSERTEQUAL(find_elem, tc::begin(select_second));

		return select_first;
	};

	auto different_indices = verify(std::vector{1, 2, 3}, std::list{4, 5});
	static_assert(std::same_as<tc::index_t<decltype(different_indices)>, std::variant<tc::index_t<std::vector<int>>, tc::index_t<std::list<int>>>>);

	[[maybe_unused]] auto same_index = verify(std::vector{1, 2, 3}, tc::transform(std::vector{4, 5}, tc::identity{}));
	// TODO, size optimization: STATICASSERTEQUAL(sizeof(tc::index_t<decltype(same_index)>), sizeof(tc::index_t<std::vector<int>>));

	auto same_iterator = verify(std::vector{1, 2, 3}, std::vector{4, 5});
	static_assert(std::same_as<tc::iterator_t<decltype(same_iterator)>, tc::iterator_t<std::vector<int>>>);
}
