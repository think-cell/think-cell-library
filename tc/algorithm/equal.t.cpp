
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"

namespace {

//---- Equal with vector<int> -------------------------------------------------------------------------------------------------
UNITTESTDEF( equal_vec_int ) {
	tc::vector<int> ve;
	tc::vector<int> ve_1;
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v123_1, {1,2,3});
	TEST_init_hack(tc::vector, int, v143, {1,4,3});
	TEST_init_hack(tc::vector, int, v1234, {1,2,3,4});

	TEST_RANGE_EQUAL(ve, ve);
	TEST_RANGE_EQUAL(ve, ve_1);

	TEST_RANGE_NOT_EQUAL(ve, v123);
	TEST_RANGE_NOT_EQUAL(v123, ve);

	TEST_RANGE_EQUAL(v123, v123);
	TEST_RANGE_EQUAL(v123, v123_1);

	TEST_RANGE_NOT_EQUAL(v123, v143);
	TEST_RANGE_NOT_EQUAL(v123, v1234);
	TEST_RANGE_NOT_EQUAL(v1234, v123);
}

UNITTESTDEF( equal_vec_int_pred ) {
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v234, {2,3,4});

	auto const ofByOne = [](int const lhs, int const rhs) noexcept { return (lhs + 1) == rhs; }; // unsymmetrical to uncover wrong order of argument application to the predicate

	_ASSERT(tc::equal(v123, v234, ofByOne));
	_ASSERT(!tc::equal(v234, v123, ofByOne));
	_ASSERT(!tc::equal(v123, v123, ofByOne));

}

//---- Equal with generators --------------------------------------------------------------------------------------------------

UNITTESTDEF( equal_generator ) {
	tc::vector<int> ve;
	tc::vector<int> ve_1;
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v123_1, {1,2,3});
	TEST_init_hack(tc::vector, int, v143, {1,4,3});
	TEST_init_hack(tc::vector, int, v1234, {1,2,3,4});

	auto ge = tc::make_generator_range(ve);
	auto g123 = tc::make_generator_range(v123);
	auto g143 = tc::make_generator_range(v143);
	auto g1234 = tc::make_generator_range(v1234);

	STATIC_ASSERT(tc::range_with_iterators<decltype(ve)>);
	STATIC_ASSERT(!tc::range_with_iterators<decltype(ge)>);

	TEST_RANGE_EQUAL(ve, ge);
	TEST_RANGE_EQUAL(ge, ve);
	//TEST_RANGE_EQUAL(ge, ge); // Fails to compile with proper msg, as it should be.

	TEST_RANGE_EQUAL(v123, g123);
	TEST_RANGE_EQUAL(g123, v123);
	TEST_RANGE_EQUAL(v1234, g1234);
	TEST_RANGE_EQUAL(g1234, v1234);

	TEST_RANGE_NOT_EQUAL(v123, g143);
	TEST_RANGE_NOT_EQUAL(g123, v143);
	TEST_RANGE_NOT_EQUAL(v123, g1234);
	TEST_RANGE_NOT_EQUAL(g123, v1234);
	TEST_RANGE_NOT_EQUAL(v1234, g123);
	TEST_RANGE_NOT_EQUAL(g1234, v123);

}

UNITTESTDEF( equal_generator_pred ) {
	TEST_init_hack(tc::vector, int, v123, {1,2,3});
	TEST_init_hack(tc::vector, int, v234, {2,3,4});

	auto g123 = tc::make_generator_range(v123);
	auto g234 = tc::make_generator_range(v234);

	auto const ofByOne = [](int const lhs, int const rhs) noexcept { return (lhs + 1) == rhs; };  // unsymmetrical to uncover wrong order of argument application to the predicate

	//_ASSERT(tc::equal(g123, g234, ofByOne)); // Fails to compile with proper msg, as it should be.

	_ASSERT(tc::equal(v123, g234, ofByOne));
	_ASSERT(!tc::equal(v234, g123, ofByOne));
	_ASSERT(!tc::equal(v123, g123, ofByOne));

	_ASSERT(tc::equal(g123, v234, ofByOne));
	_ASSERT(!tc::equal(g234, v123, ofByOne));
	_ASSERT(!tc::equal(g123, v123, ofByOne));
}

UNITTESTDEF( variadic_assign_better ) {
	int nVar = 5;
	bool b=tc::assign_better(tc::fn_less(), nVar, 6, 5, 9);
	_ASSERT(!b);
	_ASSERTEQUAL(nVar, 5);
	b=tc::assign_better(tc::fn_less(), nVar, 3);
	_ASSERT(b);
	_ASSERTEQUAL(nVar, 3);
	b=tc::assign_better(tc::fn_less(), nVar, 5, 2, 8, 0);
	_ASSERT(b);
	_ASSERTEQUAL(nVar, 0);
}

namespace no_adl {
	template<typename Better, typename List, typename=void>
	struct has_assign_better_impl final: tc::constant<false> {};

	template<typename Better, typename Var, typename... Val>
	struct has_assign_better_impl<Better, tc::type::list<Var, Val...>, tc::void_t<decltype(tc::assign_better(std::declval<Better>(), std::declval<Var>(), std::declval<Val>()...))>> final: tc::constant<true> {};

	template<typename Better, typename... T>
	using has_assign_better = has_assign_better_impl<Better, tc::type::list<T...>>;
}
#ifndef __EMSCRIPTEN__
static_assert(!no_adl::has_assign_better<tc::fn_less, std::atomic<int>, int>::value);
static_assert(!no_adl::has_assign_better<tc::fn_less, std::atomic<int>, int, int>::value);
static_assert(no_adl::has_assign_better<tc::fn_less, std::atomic<int>&, int>::value);
static_assert(no_adl::has_assign_better<tc::fn_less, std::atomic<int>&, int, int>::value);
#endif
static_assert(no_adl::has_assign_better<tc::fn_less, int&, int>::value);
static_assert(no_adl::has_assign_better<tc::fn_less, int&, int, int>::value);
}
