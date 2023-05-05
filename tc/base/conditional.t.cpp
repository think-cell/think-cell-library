
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "conditional.h"

#define TERNARY_TYPE(first, second) \
	decltype(std::declval<bool>() ? first : second)

#define SAME_TYPE(first, second) \
	static_assert(std::is_same<first, second>::value)

#define CAST_LVALUEREF(TYPE) \
	static_cast<TYPE>(std::declval<std::decay_t<TYPE>&>())

#define TERNARY_TYPE_CAST_LVALUEREF(first, second) \
	TERNARY_TYPE(CAST_LVALUEREF(first), CAST_LVALUEREF(second))

#define TEST_TERNARY_STATIC_CAST_LVALUEREF(result, first, second) \
	SAME_TYPE(result, TERNARY_TYPE_CAST_LVALUEREF(first, second))

TEST_TERNARY_STATIC_CAST_LVALUEREF(int, int, int);
TEST_TERNARY_STATIC_CAST_LVALUEREF(int const&, int const&, int const&);
TEST_TERNARY_STATIC_CAST_LVALUEREF(int&, int&, int&);
TEST_TERNARY_STATIC_CAST_LVALUEREF(int&&, int&&, int&&);
TEST_TERNARY_STATIC_CAST_LVALUEREF(int const&&, int const&&, int const&&);

namespace {
	struct S {};
}

TEST_TERNARY_STATIC_CAST_LVALUEREF(S, S, S);
TEST_TERNARY_STATIC_CAST_LVALUEREF(S const&, S const&, S const&);
TEST_TERNARY_STATIC_CAST_LVALUEREF(S&, S&, S&);
TEST_TERNARY_STATIC_CAST_LVALUEREF(S&&, S&&, S&&);
TEST_TERNARY_STATIC_CAST_LVALUEREF(S const&&, S const&&, S const&&);

namespace { // https://developercommunity.visualstudio.com/content/problem/1178976/ternary-operator-with-xvalue-operands-for-non-clas.html
	[[maybe_unused]] decltype(auto) foo(bool b, S&& s1, S&& s2) {
		return b ? static_cast<S&&>(s1) : static_cast<S&&>(s2);
	}
	
	[[maybe_unused]] decltype(auto) foo(bool b, int&& n1, int&& n2) {
		return b ? static_cast<int&&>(n1) : static_cast<int&&>(n2);
	}
	
	[[maybe_unused]] decltype(auto) bar(bool b, S&& s1, S&& s2) {
		return b ? static_cast<S const&&>(s1) : static_cast<S const&&>(s2);
	}
	
	[[maybe_unused]] decltype(auto) bar(bool b, int&& n1, int&& n2) {
		return b ? static_cast<int const&&>(n1) : static_cast<int const&&>(n2);
	}
}
