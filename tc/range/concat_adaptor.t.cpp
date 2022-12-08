
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../algorithm/compare.h"
#include "../algorithm/append.h"
#include "../array.h"
#include "../string/format.h"
#include "../container/insert.h"

#include "concat_adaptor.h"
#include "filter_adaptor.h"
#include "reverse_adaptor.h"

struct non_empty_generator {
	template<typename Func> void operator()(Func func) const { func(1); }
};

UNITTESTDEF(concat_void_generator_test) {
	auto rng = tc::concat(
		non_empty_generator(),
		tc::single(5)
	);

	tc::vector<int> vecn;
	auto PushBack = [&](int n) noexcept { tc::cont_emplace_back(vecn, n); };

	tc::for_each(rng, PushBack);
	tc::for_each(tc::as_const(rng), PushBack);
	tc::for_each(tc_move(rng), PushBack);

	TEST_RANGE_EQUAL(vecn, as_constexpr(tc::make_array(tc::aggregate_tag, 1, 5, 1, 5, 1, 5)));
}

UNITTESTDEF(concat_break_or_continue_generator_test) {
	auto rng = tc::concat(
		tc::single(2),
		tc::make_generator_range(tc::make_vector(as_constexpr(tc::make_array(tc::aggregate_tag, 1, 3)))),
		tc::single(5)
	);

	tc::vector<int> vecn;
	auto PushBack = [&](int n) noexcept { tc::cont_emplace_back(vecn, n); return tc::continue_if(1 != n); };

	tc::for_each(rng, PushBack);
	tc::for_each(tc::as_const(rng), PushBack);
	tc::for_each(tc_move(rng), PushBack);

	TEST_RANGE_EQUAL(vecn, as_constexpr(tc::make_array(tc::aggregate_tag, 2, 1, 2, 1, 2, 1)));
}

UNITTESTDEF(concat_index_test) {
	auto rng =
		tc::concat(
			as_constexpr(tc::make_array(tc::aggregate_tag, 2, 3, 4)),
			tc::single(1),
			tc::single(5)
		);

	_ASSERTEQUAL(*tc::begin_next<tc::return_element_after>(rng, 4), 5);
	_ASSERTEQUAL(*tc::end_prev<tc::return_element_after>(rng, 5), 2);

	auto it = tc::begin_next<tc::return_border>(rng, 2);
	_ASSERTEQUAL(*(it + 2), 5);
	_ASSERTEQUAL(*(it - 2), 2);
	_ASSERTEQUAL(*(tc::begin_next<tc::return_border>(rng, 4) - 2), 4);

	_ASSERTEQUAL(tc::end(rng)-tc::begin(rng), 5);
	_ASSERTEQUAL(tc::begin(rng)-tc::end(rng), -5);


	tc::vector<int> vecn;
	_ASSERTEQUAL(tc::break_, tc::for_each(
		tc::filter(
			tc::reverse(
				tc::concat(
					as_constexpr(tc::make_array(tc::aggregate_tag, 1, 3, 4)),
					tc::single(1),
					tc::single(5)
				)),
			[](int i) noexcept { return i % 2 == 1; }
		),
		[&](int i) noexcept { tc::cont_emplace_back(vecn, i); return 3 == i ? tc::break_ : tc::continue_; }
	));

	TEST_RANGE_EQUAL(vecn, as_constexpr(tc::make_array(tc::aggregate_tag, 5, 1, 3)));
	TEST_RANGE_LENGTH(rng, 5);
}

UNITTESTDEF(concat_with_empty_test) {
	auto rng = tc::concat(
		tc::make_empty_range<int>(),
		tc::concat(
			tc::single(1),
			tc::single(2)
		),
		tc::concat(
			tc::single(3),
			tc::single(4)
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
			tc::single(1),
			as_constexpr(tc::make_array(tc::aggregate_tag, S{2}, S{3}, S{4})),
			tc::single(5)
		);

	struct SFunctor {
		tc::vector<int> m_vec;

		void operator()(int i) & noexcept { tc::cont_emplace_back(m_vec, i); }
		void operator()(S s) & noexcept { tc::cont_emplace_back(m_vec, s.m_i); }
	} functor;

	tc::for_each(rng, std::ref(functor));

	TEST_RANGE_EQUAL(functor.m_vec, as_constexpr(tc::make_array(tc::aggregate_tag, 1, 2, 3, 4, 5)));
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
	TEST_RANGE_EQUAL(rng, as_constexpr(tc::make_array(tc::aggregate_tag, 2, 2)));
}

UNITTESTDEF(concat_deep_const_test) {
	auto const rng = tc::concat(tc::single(1), tc::single(2));

	STATICASSERTSAME(decltype(*tc::begin(rng)), int const&);

	_ASSERTEQUAL(*tc::begin(rng), 1);
}

UNITTESTDEF(ConcatAdvanceToEndIterator) {
	{
		tc::vector<int> vecn(1, 0);
		void(tc::begin_next<tc::return_border>(tc::concat(vecn, vecn), tc::size(vecn) * 2));

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

UNITTESTDEF(concat_flattening) {
	tc::vector<int> vecn1{1,2,3};
	tc::vector<int> vecn2{4,5};
	tc::vector<int> vecn3{6,7,8};
	tc::vector<int> vecn4{9};
	tc::vector<int> vecnRhs{1,2,3,4,5,6,7,8,9};
	auto const rng = tc::concat(vecn1, tc::empty_range(), vecn2, tc::concat(tc::empty_range(), vecn3, tc::empty_range(), vecn4, tc::empty_range()));
	_ASSERT(tc::equal(rng, vecnRhs));
	auto const rng2 = tc::concat(vecn1, vecn2, vecn3, vecn4);
	STATICASSERTSAME(decltype(rng), decltype(rng2));
	STATICASSERTSAME(decltype(tc::concat(tc::empty_range(), tc::empty_range())), tc::empty_range);
	STATICASSERTSAME(decltype(tc::concat(vecn1, tc::empty_range())), decltype((vecn1)));
}

namespace
{
	TC_HAS_EXPR(decrement_operator, (T), --std::declval<T&>());

	template<typename Derived>
	struct forward_range_base : tc::range_iterator_from_index<Derived, int> {
		using this_type = forward_range_base;
		int values[3] = { 0, 1, 2 };
		static constexpr bool c_bHasStashingIndex=false;
		STATIC_OVERRIDE(begin_index)() const -> int { return 0; }
		STATIC_OVERRIDE(end_index)() const -> int { return 3; }
		STATIC_OVERRIDE(increment_index)(int& i) const -> void { ++i; }
		STATIC_OVERRIDE(dereference_index)(int i) -> int& { return values[i]; }
		STATIC_OVERRIDE(dereference_index)(int i) const -> int const& { return values[i]; }
	};

	struct forward_range : forward_range_base<forward_range> {};

	struct bidir_range : forward_range_base<bidir_range> {
		using Derived = bidir_range;
		using this_type = bidir_range;
		STATIC_FINAL(decrement_index)(int& i) const -> void { --i; }
	};

	[[maybe_unused]] void concat_iterator_auto_traversal_static_tests() {
		// Any concatenation which includes a forward-traversal range should be forward-traversal
		{
			auto rng = tc::concat(forward_range(), forward_range());
			auto it = tc::begin(rng);
			static_assert(!tc::has_decrement_index<decltype(rng)>::value);
			static_assert(!has_decrement_operator<decltype(it)>::value);
			static_assert(std::is_same<typename boost::iterator_traversal<decltype(it)>::type, boost::iterators::forward_traversal_tag>::value);
		}
		{
			auto rng = tc::concat(forward_range(), bidir_range());
			auto it = tc::begin(rng);
			static_assert(!tc::has_decrement_index<decltype(rng)>::value);
			static_assert(!has_decrement_operator<decltype(it)>::value);
			static_assert(std::is_same<typename boost::iterator_traversal<decltype(it)>::type, boost::iterators::forward_traversal_tag>::value);
		}
		{
			auto rng = tc::concat(bidir_range(), forward_range());
			auto it = tc::begin(rng);
			static_assert(!tc::has_decrement_index<decltype(rng)>::value);
			static_assert(!has_decrement_operator<decltype(it)>::value);
			static_assert(std::is_same<typename boost::iterator_traversal<decltype(it)>::type, boost::iterators::forward_traversal_tag>::value);
		}

		// A concatenation of bidirectional ranges should be bidirectional
		{
			auto rng = tc::concat(bidir_range(), bidir_range());
			auto it = tc::begin(rng);
			static_assert(tc::has_decrement_index<decltype(rng)>::value);
			static_assert(has_decrement_operator<decltype(it)>::value);
			static_assert(std::is_same<typename boost::iterator_traversal<decltype(it)>::type, boost::iterators::bidirectional_traversal_tag>::value);
		}
	}
}

STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat("abc", "def"))>);
STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat("abc", tc::as_dec(10)))>);
STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat(tc::as_dec(10), "abc", tc::string<char>("de")))>);
STATICASSERTSAME(char, tc::range_value_t<decltype(tc::concat(tc::as_dec(10), "abc", tc::concat("xy", tc::as_dec(5)), tc::string<char>("de")))>);
STATICASSERTSAME(int, tc::range_value_t<decltype(tc::transform(tc::concat("ab", UTF16("cd")), tc::fn_static_cast<int>()))>);
static_assert(!tc::has_range_value<decltype(tc::concat("xy", tc::as_dec(5), L"a"))>::value);
static_assert(!tc::has_range_value<decltype(tc::concat("abc", tc::as_dec(10), tc::concat("xy", tc::as_dec(5), L"a"), tc::string<char>("de")))>::value);
static_assert(tc::has_range_value<decltype(tc::concat(tc::as_dec(5), tc::as_dec(10)))>::value);
static_assert(!tc::has_range_value<decltype(tc::transform(tc::concat("ab", UTF16("cd")), tc::fn_increment()))>::value);
static_assert(tc::is_concat_range<decltype(tc::concat(tc::as_dec(5), "abc"))>::value);
static_assert(tc::is_concat_range<decltype(tc::concat("abc", L"def"))>::value);
static_assert(tc::is_concat_range<decltype(tc::concat("abc", tc::string<char>("def")))>::value);
