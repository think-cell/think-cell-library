
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "invoke.h"

namespace {
	[[maybe_unused]] void static_tests() noexcept {
		static auto constexpr rvalue_sink = [](int, double&&, char const&) noexcept {};
		static_assert(!tc::invocable<decltype(rvalue_sink), int, double&, char>);
		static_assert(!tc::invocable<decltype(rvalue_sink), std::tuple<int, double&, char>>);

		tc::invoke(rvalue_sink, 0, 0., 'a');
		tc::invoke(rvalue_sink, std::make_tuple(0, 0., 'a'));
		tc::invoke(rvalue_sink, std::make_tuple(0, 0.), 'a');
		tc::invoke(rvalue_sink, std::make_tuple(0, std::make_tuple(std::make_tuple(0.), 'a')));

		static auto constexpr lvalue_sink = [](int, double&, char const&) noexcept {};
		static_assert(!tc::invocable<decltype(lvalue_sink), int, double, char>);
		static_assert(!tc::invocable<decltype(lvalue_sink), std::tuple<int, double, char>>);
		static_assert(!tc::invocable<decltype(lvalue_sink), std::tuple<int&&, double&&, char&&>>);

		int a0 = 0;
		double a1 = 0.;
		char a2 = 'a';
		tc::invoke(lvalue_sink, a0, a1, a2);
		tc::invoke(lvalue_sink, std::tie(a0, a1, a2));
		tc::invoke(lvalue_sink, std::tie(a0, a1), a2);
		tc::invoke(lvalue_sink, std::forward_as_tuple(a0, std::forward_as_tuple(std::tie(a1), a2)));
	}
}
