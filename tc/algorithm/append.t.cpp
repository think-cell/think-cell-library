
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "append.h"

#include "../unittest.h"
#include "../string/format.h"
#include "../static_vector.h"
#include "../range/filter_adaptor.h"

static_assert(tc::appendable<char const*, tc::string<char>&>);
static_assert(!tc::appendable<int, tc::string<char>&>);
static_assert(tc::appendable<tc::char16 const*, tc::string<char>&>);
static_assert(tc::appendable<decltype(tc::as_dec(5)), tc::string<char>&>);
static_assert(!tc::appendable<tc::size_proxy<int>, tc::string<char>&>);
static_assert(!tc::appendable<tc::vector<int>, tc::string<char>&>);
static_assert(!tc::appendable<tc::static_vector<int, 3>, tc::string<char>&>);

UNITTESTDEF(nonappendable) {
	tc::vector<int> vecnTest{1, 2, 3};
	auto rngTest1=tc::transform(vecnTest, [](auto const n) noexcept { return n + 1; });
	static_assert(!tc::appendable<decltype(rngTest1), tc::string<char>&>);
	auto rngTest2=tc::filter(vecnTest, [](auto const n) noexcept { return n%2==1; });
	static_assert(!tc::appendable<decltype(rngTest2), tc::string<char>&>);
}

UNITTESTDEF(append_on_dtor) {
	{
		tc::string<char> str;
		tc::append(str, "Hello ");
		{
			tc::make_append_on_dtor(str, "World!");
		}
		_ASSERT(tc::equal(str, "Hello World!"));
	}
	{
		tc::string<char> str;
		{
			tc_auto_cref(a1, tc::make_append_on_dtor(str, "World!"));
			tc_auto_cref(a2, tc::make_append_on_dtor(str, "Hello "));
		}
		_ASSERT(tc::equal(str, "Hello World!"));
	}
}
