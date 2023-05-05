
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
		auto rvalue_sink = [](int, double&&, char const&) noexcept {};
		static_assert(!tc::is_invocable<decltype(rvalue_sink), int, double&, char>::value);
		static_assert(!tc::is_invocable<decltype(rvalue_sink), std::tuple<int, double&, char>>::value);

		tc::invoke(rvalue_sink, 0, 0., 'a');
		tc::invoke(rvalue_sink, std::make_tuple(0, 0., 'a'));
		tc::invoke(rvalue_sink, std::make_tuple(0, 0.), 'a');
		tc::invoke(rvalue_sink, std::make_tuple(0, std::make_tuple(std::make_tuple(0.), 'a')));

		auto lvalue_sink = [](int, double&, char const&) noexcept {};
		static_assert(!tc::is_invocable<decltype(lvalue_sink), int, double, char>::value);
		static_assert(!tc::is_invocable<decltype(lvalue_sink), std::tuple<int, double, char>>::value);
		static_assert(!tc::is_invocable<decltype(lvalue_sink), std::tuple<int&&, double&&, char&&>>::value);

		int a0 = 0;
		double a1 = 0.;
		char a2 = 'a';
		tc::invoke(lvalue_sink, a0, a1, a2);
		tc::invoke(lvalue_sink, std::tie(a0, a1, a2));
		tc::invoke(lvalue_sink, std::tie(a0, a1), a2);
		tc::invoke(lvalue_sink, std::forward_as_tuple(a0, std::forward_as_tuple(std::tie(a1), a2)));
	}
}
