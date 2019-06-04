
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.h"
#include "range.t.h"
#include "compare.h"

#include "concat_adaptor.h"

struct non_empty_generator {
	template<typename Func> void operator()(Func func) const { func(1); }
};

UNITTESTDEF(concat_void_generator_test) {
	auto rng = tc::concat(
		non_empty_generator(),
		MAKE_CONSTEXPR_ARRAY(5)
	);

	tc::vector<int> vecn;
	auto PushBack = [&](int n) noexcept { tc::cont_emplace_back(vecn, n); };

	rng(PushBack);
	tc::as_const(rng)(PushBack);

	tc::for_each(rng, PushBack);
	tc::for_each(tc::as_const(rng), PushBack);

	TEST_RANGE_EQUAL(vecn, MAKE_CONSTEXPR_ARRAY(1, 5, 1, 5, 1, 5, 1, 5));
}

UNITTESTDEF(concat_break_or_continue_generator_test) {
	auto rng = tc::concat(
		MAKE_CONSTEXPR_ARRAY(2),
		tc::make_generator_range(tc::make_vector(MAKE_CONSTEXPR_ARRAY(1, 3))),
		MAKE_CONSTEXPR_ARRAY(5)
	);

	tc::vector<int> vecn;
	auto PushBack = [&](int n) noexcept { tc::cont_emplace_back(vecn, n); return tc::continue_if(1 != n); };

	rng(PushBack);
	tc::as_const(rng)(PushBack);

	tc::for_each(rng, PushBack);
	tc::for_each(tc::as_const(rng), PushBack);

	TEST_RANGE_EQUAL(vecn, MAKE_CONSTEXPR_ARRAY(2, 1, 2, 1, 2, 1, 2, 1));
}

UNITTESTDEF(concat_index_test) {
	auto rng =
		tc::concat(
			MAKE_CONSTEXPR_ARRAY(2, 3, 4),
			MAKE_CONSTEXPR_ARRAY(1),
			MAKE_CONSTEXPR_ARRAY(5)
		);

	_ASSERTEQUAL(*tc::begin_next(rng, 4), 5);
	_ASSERTEQUAL(*tc::end_prev(rng, 5), 2);

	auto it = tc::begin_next(rng, 2);
	_ASSERTEQUAL(*(it + 2), 5);
	_ASSERTEQUAL(*(it - 2), 2);
	_ASSERTEQUAL(*(tc::begin_next(rng, 4) - 2), 4);

	_ASSERTEQUAL(tc::end(rng)-tc::begin(rng), 5);
	_ASSERTEQUAL(tc::begin(rng)-tc::end(rng), -5);


	tc::vector<int> vecn;
	_ASSERTEQUAL(tc::break_, tc::for_each(
		tc::filter(
			tc::reverse(
				tc::concat(
					MAKE_CONSTEXPR_ARRAY(1, 3, 4),
					MAKE_CONSTEXPR_ARRAY(1),
					MAKE_CONSTEXPR_ARRAY(5)
				)),
			[](int i) noexcept { return i % 2 == 1; }
		),
		[&](int i) noexcept { tc::cont_emplace_back(vecn, i); return 3 == i ? tc::break_ : tc::continue_; }
	));

	TEST_RANGE_EQUAL(vecn, MAKE_CONSTEXPR_ARRAY(5, 1, 3));
	TEST_RANGE_LENGTH(rng, 5);
}

UNITTESTDEF(concat_with_empty_test) {
	auto rng = tc::concat(
		tc::make_empty_range<int>(),
		tc::concat(
			MAKE_CONSTEXPR_ARRAY(1),
			MAKE_CONSTEXPR_ARRAY(2)
		),
		tc::concat(
			MAKE_CONSTEXPR_ARRAY(3),
			MAKE_CONSTEXPR_ARRAY(4)
		)
	);

	_ASSERTEQUAL(*tc::begin(rng), 1);
	TEST_RANGE_LENGTH(rng, 4);
}

UNITTESTDEF(concat_different_value_types_test) {
	struct S {
		int m_i;
	};

	auto rng =
		tc::concat(
			MAKE_CONSTEXPR_ARRAY(1),
			MAKE_CONSTEXPR_ARRAY(S{2}, S{3}, S{4}),
			MAKE_CONSTEXPR_ARRAY(5)
		);

	struct SFunctor {
		tc::vector<int> m_vec;

		void operator()(int i) & noexcept { tc::cont_emplace_back(m_vec, i); }
		void operator()(S s) & noexcept { tc::cont_emplace_back(m_vec, s.m_i); }
	} functor;

	tc::for_each(rng, std::ref(functor));

	TEST_RANGE_EQUAL(functor.m_vec, MAKE_CONSTEXPR_ARRAY(1, 2, 3, 4, 5));
}

UNITTESTDEF(concat_empty_test) {
	auto rng = tc::concat(tc::make_empty_range<int>(), tc::make_empty_range<int>());
	_ASSERTEQUAL(tc::begin(rng), tc::end(rng));
}

UNITTESTDEF(concat_shallow_const_test) {
	tc::vector<int> vecn{1};
	auto const rng = tc::concat(vecn, vecn);
	STATICASSERTSAME(decltype(*tc::begin(rng)), int&);

	*tc::begin(rng) = 2;
	TEST_RANGE_EQUAL(rng, MAKE_CONSTEXPR_ARRAY(2, 2));
}

UNITTESTDEF(concat_deep_const_test) {
	auto const rng = tc::concat(tc::single(1), tc::single(2));

	STATICASSERTSAME(decltype(*tc::begin(rng)), int const&);

	_ASSERTEQUAL(*tc::begin(rng), 1);
}

UNITTESTDEF(ConcatAdvanceToEndIterator) {
	{
		tc::vector<int> vecn(1, 0);
		tc::begin_next(tc::concat(vecn, vecn), tc::size(vecn) * 2);

		auto rng = tc::concat(vecn,vecn);
		auto it = tc::begin(rng);
		it +=2;
		_ASSERTEQUAL(tc::end(rng), it);
		it += 0;
		it -= 2;
		_ASSERTEQUAL(tc::begin(rng), it);
	}

	tc::vector<int> vecn(2,0);
	tc::vector<int> vecnEmpty;
	auto rng = tc::concat(vecn, vecnEmpty);
	auto it = tc::begin(rng);
	++it; ++it;
	auto it2 = tc::begin(rng);
	it2 += 2;
	_ASSERTEQUAL(it, it2);
	_ASSERTEQUAL(tc::end(rng), it);
	it += 0;
}

STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat("abc", "def"))>);
STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat("abc", tc::as_dec(10)))>);
STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat(tc::as_dec(10), "abc", std::string("de")))>);
STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat(tc::as_dec(10), "abc", tc::concat("xy", tc::as_dec(5)), std::string("de")))>);
static_assert(!tc::has_range_value<decltype(tc::concat("xy", tc::as_dec(5), L"a"))>::value);
STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat("abc", tc::as_dec(10), tc::concat("xy", tc::as_dec(5), L"a"), std::string("de")))>);
static_assert(!tc::has_range_value<decltype(tc::concat(tc::as_dec(5), tc::as_dec(10)))>::value);
static_assert(tc::is_concat_range<decltype(tc::concat(tc::as_dec(5), "abc"))>::value);
static_assert(tc::is_concat_range<decltype(tc::concat("abc", L"def"))>::value);
static_assert(tc::is_concat_range<decltype(tc::concat("abc", std::string("def")))>::value);
