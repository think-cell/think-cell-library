//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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

#include "range.h"
#include "range.t.h"
#include "compare.h"

#include "join_adaptor.h"

struct non_empty_generator {
	template<typename Func> void operator()(Func func) const { func(1); }
};

UNITTESTDEF(concat_void_generator_test) {
	auto rng = tc::concat(
		non_empty_generator(),
		tc::make_array(tc::aggregate_tag{}, 5)
	);

	tc::vector<int> vecn;
	auto PushBack = [&](int n) noexcept { vecn.push_back(n); };

	rng(PushBack);
	tc::as_const(rng)(PushBack);

	tc::for_each(rng, PushBack);
	tc::for_each(tc::as_const(rng), PushBack);

	TEST_RANGE_EQUAL(vecn, tc::make_array(tc::aggregate_tag{}, 1, 5, 1, 5, 1, 5, 1, 5));
}

UNITTESTDEF(concat_break_or_continue_generator_test) {
	auto rng = tc::concat(
		tc::make_array(tc::aggregate_tag{}, 2),
		tc::make_generator_range(tc::make_vector(tc::make_array(tc::aggregate_tag{}, 1, 3))),
		tc::make_array(tc::aggregate_tag{}, 5)
	);

	tc::vector<int> vecn;
	auto PushBack = [&](int n) noexcept { vecn.push_back(n); return tc::continue_if(1 != n); };

	rng(PushBack);
	tc::as_const(rng)(PushBack);

	tc::for_each(rng, PushBack);
	tc::for_each(tc::as_const(rng), PushBack);

	TEST_RANGE_EQUAL(vecn, tc::make_array(tc::aggregate_tag{}, 2, 1, 2, 1, 2, 1, 2, 1));
}

UNITTESTDEF(concat_index_test) {
	auto rng =
		tc::concat(
			tc::make_array(tc::aggregate_tag{}, 2, 3, 4),
			tc::make_singleton_range(1),
			tc::make_singleton_range(5)
		);

	_ASSERTEQUAL(*tc::begin_next(rng, 4), 5);
	_ASSERTEQUAL(*tc::end_prev(rng, 5), 2);

	auto it = tc::begin_next(rng, 2);
	_ASSERTEQUAL(*(it + 2), 5);
	_ASSERTEQUAL(*(it - 2), 2);
	_ASSERTEQUAL(*(tc::begin_next(rng, 4) - 2), 4);

	_ASSERTEQUAL(boost::begin(rng).distance_to(boost::end(rng)), 5);
	_ASSERTEQUAL(boost::end(rng).distance_to(boost::begin(rng)), -5);


	tc::vector<int> vecn;
	_ASSERTEQUAL(tc::break_, tc::for_each(
		tc::filter(
			tc::reverse(
				tc::concat(
					tc::make_array(tc::aggregate_tag{}, 1, 3, 4),
					tc::make_singleton_range(1),
					tc::make_singleton_range(5)
				)),
			[](int i) noexcept { return i % 2 == 1; }
		),
		[&](int i) noexcept { vecn.push_back(i); return 3 == i ? tc::break_ : tc::continue_; }
	));

	TEST_RANGE_EQUAL(vecn, tc::make_array(tc::aggregate_tag{}, 5, 1, 3));
	TEST_RANGE_LENGTH(rng, 5);
}

UNITTESTDEF(concat_with_empty_test) {
	auto rng = tc::concat(
		tc::make_empty_range<int>(),
		tc::concat(
			tc::make_singleton_range(1),
			tc::make_singleton_range(2)
		),
		tc::concat(
			tc::make_singleton_range(3),
			tc::make_singleton_range(4)
		)
	);

	_ASSERTEQUAL(*boost::begin(rng), 1);
	TEST_RANGE_LENGTH(rng, 4);
}

UNITTESTDEF(concat_different_value_types_test) {
	struct S {
		int m_i;
	};

	auto rng =
		tc::concat(
			tc::make_singleton_range(1),
			tc::make_array(tc::aggregate_tag{}, S{2}, S{3}, S{4}),
			tc::make_singleton_range(5)
		);

	struct SFunctor {
		tc::vector<int> m_vec;

		void operator()(int i) & noexcept { m_vec.push_back(i); }
		void operator()(S s) & noexcept { m_vec.push_back(s.m_i); }
	} functor;

	tc::for_each(rng, std::ref(functor));

	TEST_RANGE_EQUAL(functor.m_vec, tc::make_array(tc::aggregate_tag{}, 1, 2, 3, 4, 5));
}

UNITTESTDEF(concat_empty_test) {
	auto rng = tc::concat(tc::make_empty_range<int>(), tc::make_empty_range<int>());
	_ASSERT(boost::begin(rng) == boost::end(rng));
}

UNITTESTDEF(concat_shallow_const_test) {
	tc::vector<int> vecn{1};
	auto const rng = tc::concat(vecn, vecn);
	static_assert(std::is_same<decltype(*boost::begin(rng)), int&>::value, "");

	*boost::begin(rng) = 2;
	TEST_RANGE_EQUAL(rng, tc::make_array(tc::aggregate_tag{}, 2, 2));
}

UNITTESTDEF(concat_deep_const_test) {
	auto const rng = tc::concat(tc::make_singleton_range(1), tc::make_singleton_range(2));

	static_assert(std::is_same<decltype(*boost::begin(rng)), int const&>::value, "");

	_ASSERTEQUAL(*boost::begin(rng), 1);
}