
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"
#include "optional.h"

UNITTESTDEF(optional_as_range) {
	std::optional<int> on;
	_ASSERT(tc::empty(on));
	_ASSERTEQUAL(tc::begin(on), tc::end(on));
	_ASSERTEQUAL(tc::end(on)-tc::begin(on), 0);
	on = 5;
	auto const it1 = tc::begin(on);
	_ASSERTEQUAL(*it1, 5);
	auto& n1 = *it1;
	++n1;
	_ASSERT(tc::equal(tc::single(6), on));
	_ASSERTEQUAL(tc::size(on), 1);
	tc::for_each(on, [](auto const n) noexcept {
		_ASSERTEQUAL(n, 6);
	});

	std::optional<int> const on2 = 8;
	auto itBegin = tc::begin(on2);
	auto itEnd = tc::end(on2);
	_ASSERTEQUAL(itBegin+1, itEnd);
	_ASSERTEQUAL(itEnd-1, itBegin);
	_ASSERTEQUAL(itEnd-itBegin, 1);
	++itBegin;
	_ASSERTEQUAL(itBegin, itEnd);
	--itBegin;
	_ASSERTEQUAL(itBegin, tc::begin(on2));

	constexpr std::optional<bool> ob;
	static_assert(!tc::any_of(ob));

	constexpr std::optional<int> on3 = 10;
	constexpr int n3 = *tc::begin(on3);
	static_assert(n3 == 10);
	static_assert(tc::all_of(on3, [](auto const n) constexpr noexcept { return 8 < n; }));
}
