
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "take_while.h"
#include "../algorithm/for_each_xxx.h"
#include "../algorithm/append.h"

UNITTESTDEF(GeneratorRangeTakeWhile) {
	auto GeneratorRange = tc::generator_range_output<int const&>([](auto const& sink) noexcept -> tc::break_or_continue {
		RETURN_IF_BREAK(tc::continue_if_not_break(sink, 1));
		RETURN_IF_BREAK(tc::continue_if_not_break(sink, 2));
		_ASSERTFALSE;
		RETURN_IF_BREAK(tc::continue_if_not_break(sink, 3));
		return tc::continue_;
	});

	static_assert(std::is_same<int, tc::range_value_t<decltype(tc::take_while(GeneratorRange))>>::value);

	tc::for_each(
		tc::take_while(
			GeneratorRange,
			[](auto const n) noexcept {
				return n<2;
			}
		),
		[](auto const n) noexcept {
			_ASSERTEQUAL(n, 1);
			return tc::constant<tc::continue_>();
		}
	);

	auto vecn = tc::explicit_cast<tc::vector<int>>(tc::make_range(1,2,3,4));
	static_assert(std::is_same<
		decltype(tc::for_each(vecn,tc::noop())),
		tc::constant<tc::continue_>
	>::value);

	auto rng = tc::take_while(vecn,[](auto const n) noexcept { return n<3; });
	static_assert(std::is_same<tc::vector<int>&, decltype(rng.base_range())>::value);

	tc::for_each(rng, [](auto const n) noexcept {
		_ASSERT(1==n || 2==n);
	});

	auto itBegin = tc::begin(rng);
	auto const itEnd = tc::end(rng);
	_ASSERT(itBegin != itEnd);
	_ASSERTEQUAL(1, *itBegin);
	++itBegin;
	_ASSERT(itBegin != itEnd);
	_ASSERTEQUAL(2, *itBegin);
	++itBegin;
	_ASSERT(itBegin == itEnd);
}
