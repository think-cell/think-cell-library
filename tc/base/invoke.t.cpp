
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "invoke.h"
#include "casts.h"
#include "../tuple.h"

namespace {
	template <typename ... ExpectedArgs>
	struct test_arguments {
		mutable int foo;

		template <typename ... Args>
		int&& /* we return an rvalue reference, otherwise invoke would not give us temporaries */ operator()(Args&&...) const noexcept {
			([]{ STATICASSERTSAME(ExpectedArgs&&, Args&&); }(), ...);
			return tc_move(foo);
		}
	};

	[[maybe_unused]] void static_tests_invoke_arguments() noexcept {
		int i;
		tc::tuple<int> tpl;

		tc_invoke(
			(test_arguments<tc::temporary<int, 1>, tc::temporary<bool, 1>, char const(&)[4], int&&, int const&>{}),
			1, true, "abc", tc_move(i), tc::as_const(i)
		);

		tc_invoke(
			(test_arguments<tc::temporary<tc::tuple<>, 1>, tc::temporary<tc::tuple<int>, 1>, tc::tuple<int>&, tc::temporary<tc::tuple<int&>, 1>>{}), 
			tc::make_tuple(), tc::make_tuple(0), tpl, tc::tie(i)
		);

		tc_apply(
			(test_arguments<tc::temporary<int, 1>, int&, int&>{}),
			tc::make_tuple(), tc::make_tuple(0), tpl, tc::tie(i)
		);

		tc_apply(
			(test_arguments<tc::temporary<int, 2>, int&>{}),
			(tc::temporary<tc::tuple<int>, 1>(tc::make_tuple(0))), (tc::temporary<tc::tuple<int&>, 1>(tc::tie(i)))
		);
	}

	[[maybe_unused]] void static_tests_invoke() noexcept {
		tc_static_auto_constexpr_lambda(rvalue_sink) = [](int, double&&, char const&) noexcept {};
		static_assert(!tc::invocable<decltype(rvalue_sink), int, double&, char>);
		static_assert(!tc::invocable<decltype(rvalue_sink), tc::tuple<int, double&, char>>);

		tc_invoke(rvalue_sink, 0, 0., 'a');
		tc_invoke(rvalue_sink, tc::make_tuple(0, 0., 'a'));
		tc_invoke(rvalue_sink, tc::make_tuple(0, 0.), 'a');
		tc_invoke(rvalue_sink, tc::make_tuple(0, tc::make_tuple(tc::make_tuple(0.), 'a')));

		tc_static_auto_constexpr_lambda(lvalue_sink) = [](int, double&, char const&) noexcept {};
		static_assert(!tc::invocable<decltype(lvalue_sink), int, double, char>);
		static_assert(!tc::invocable<decltype(lvalue_sink), tc::tuple<int, double, char>>);
		static_assert(!tc::invocable<decltype(lvalue_sink), tc::tuple<int&&, double&&, char&&>>);

		int a0 = 0;
		double a1 = 0.;
		char a2 = 'a';
		tc_invoke(lvalue_sink, a0, a1, a2);
		tc_invoke(lvalue_sink, tc::tie(a0, a1, a2));
		tc_invoke(lvalue_sink, tc::tie(a0, a1), a2);
		tc_invoke(lvalue_sink, tc::tie(a0, tc::tie(tc::tie(a1), a2)));
	}

	[[maybe_unused]] void static_tests_apply() noexcept {
		// We need the lambda to test tc_apply without any arguments - the macro requires at least one.
		tc_static_auto_constexpr_lambda(apply) = [](auto fn, auto&& ... args) noexcept {
			tc_apply_pack(fn, tc_move_if_owned(args));
		};

		apply([](auto const& ... args) noexcept { STATICASSERTEQUAL(sizeof...(args), 4); }, tc::make_tuple(0, tc::make_tuple(1, 2), 3), 4);
		apply([]{});
		apply([]{}, tc::make_tuple(), tc::make_tuple());
	}
}
