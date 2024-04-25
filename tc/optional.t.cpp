
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "base/assert_defs.h"
#include "unittest.h"
#include "optional.h"
#include "array.h"

namespace {
	template <typename T>
	void test_optional_ctor(T const& value) {
		tc::optional<T> const def;
		_ASSERT(!def);

		tc::optional<T> const nullopt(std::nullopt);
		_ASSERT(!nullopt);

		tc::optional<T> const inplace(std::in_place, value);
		_ASSERT(inplace);
		_ASSERTEQUAL(*inplace, value);

		tc::optional<T> const copy_def(def);
		_ASSERT(!copy_def);
		tc::optional<T> const copy_inplace(inplace);
		_ASSERT(copy_inplace);
		_ASSERTEQUAL(*copy_inplace, value);

		tc::optional<T> const move_def(tc::optional<T>{});
		_ASSERT(!move_def);
		tc::optional<T> const move_inplace(tc::optional<T>{std::in_place, value});
		_ASSERT(move_inplace);
		_ASSERTEQUAL(*move_inplace, value);

		tc::optional<T> const generic_empty(tc::explicit_cast<T*>(nullptr));
		_ASSERT(!generic_empty);
		tc::optional<T> const generic_value(&value);
		_ASSERT(generic_value);
		_ASSERTEQUAL(*generic_value, value);
	}

	template <typename T, typename U>
	void test_optional_ops(T const& value1, U const& value2) {
		tc::optional<T> opt;
		_ASSERT(!opt);

		auto const fn_emplace = [&](T const& value) {
			tc::optional_emplace(opt, value);
			_ASSERT(opt);
			_ASSERTEQUAL(*opt, value);
		};
		auto const fn_assign_nullopt = [&]{
			opt = std::nullopt;
			_ASSERT(!opt);
		};
		auto const fn_assign_opt = [&](auto&& other) {
			auto save = other;

			opt = tc_move_if_owned(other);
			if (save) {
				_ASSERT(opt);
				_ASSERTEQUAL(*opt, *save);
			} else {
				_ASSERT(!opt);
			}
		};

		// reset operations on empty
		opt = std::nullopt; fn_assign_nullopt();
		opt = std::nullopt; fn_assign_opt(tc::optional<T>());
		opt = std::nullopt; fn_assign_opt(tc::as_lvalue(tc::optional<T>()));
		opt = std::nullopt; fn_assign_opt(tc::explicit_cast<T*>(nullptr));

		// emplace operations on empty
		opt = std::nullopt; fn_emplace(value1);
		opt = std::nullopt; fn_assign_opt(tc::optional<T>(std::in_place, value1));
		opt = std::nullopt; fn_assign_opt(tc::as_lvalue(tc::optional<T>(std::in_place, value1)));
		opt = std::nullopt; fn_assign_opt(&value1);

		// reset operations on non-empty
		tc::optional_emplace(opt, value1); fn_assign_nullopt();
		tc::optional_emplace(opt, value1); fn_assign_opt(tc::optional<T>());
		tc::optional_emplace(opt, value1); fn_assign_opt(tc::as_lvalue(tc::optional<T>()));
		tc::optional_emplace(opt, value1); fn_assign_opt(tc::explicit_cast<T*>(nullptr));

		// emplace operations on non-empty
		tc::optional_emplace(opt, value2); fn_emplace(value1);
		tc::optional_emplace(opt, value2); fn_assign_opt(tc::optional<T>(std::in_place, value1));
		tc::optional_emplace(opt, value2); fn_assign_opt(tc::as_lvalue(tc::optional<T>(std::in_place, value1)));
		tc::optional_emplace(opt, value2); fn_assign_opt(&value1);

		// self-assign
		opt = std::nullopt; fn_assign_opt(opt);
		opt = std::nullopt; fn_assign_opt(tc_move(opt));
		tc::optional_emplace(opt, value1); fn_assign_opt(opt);
		// tc::optional_emplace(opt, value1); fn_assign_opt(tc_move(opt)); - self-move assign is not guaranteed to work according to the standard
	}
}

UNITTESTDEF(optional) {
	test_optional_ctor(11);
	test_optional_ops(11, 42);

	test_optional_ctor(std::string("This is a string longer than SSO, so we can test actual heap allocation and stuff."));
	test_optional_ops(std::string("This is a string longer than SSO, so we can test actual heap allocation and stuff."), std::string("short"));

	struct SEmpty {
		SEmpty() = default;
		SEmpty(int) {}

		bool operator==(SEmpty const&) const noexcept = default;
		bool operator==(int) const noexcept { return true; }
	};
	test_optional_ctor(SEmpty{});
	test_optional_ops(SEmpty{}, 11);
}

UNITTESTDEF(make_optional_reference_or_value) {
	int obj = 42;
	int const cobj = 42;

	std::same_as<tc::optional<int&>> auto opt_obj = tc::make_optional_reference_or_value(obj);
	_ASSERTEQUAL(&*opt_obj, &obj);

	std::same_as<tc::optional<int const&>> auto opt_cobj = tc::make_optional_reference_or_value(cobj);
	_ASSERTEQUAL(&*opt_cobj, &cobj);

	std::same_as<tc::optional<int>> auto opt_rvalue = tc::make_optional_reference_or_value(42);
	_ASSERTEQUAL(*opt_rvalue, 42);

	struct empty {};
	empty empty_obj;
	empty const empty_cobj;

	std::same_as<tc::optional<empty>> auto opt_empty_obj = tc::make_optional_reference_or_value(empty_obj);
	_ASSERT(opt_empty_obj);

	std::same_as<tc::optional<empty>> auto opt_empty_cobj = tc::make_optional_reference_or_value(empty_cobj);
	_ASSERT(opt_empty_cobj);

	std::same_as<tc::optional<empty>> auto opt_empty_rvalue = tc::make_optional_reference_or_value(empty());
	_ASSERT(opt_empty_rvalue);
}

UNITTESTDEF(optional_as_range) {
	std::optional<int> on;
	_ASSERT(tc::empty(on));
	_ASSERTEQUAL(tc::begin(on), tc::end(on));
	_ASSERTEQUAL(tc::end(on)-tc::begin(on), 0);
	on = 5;
	auto const it1 = tc::begin(on);
	_ASSERTEQUAL(*it1, 5);
	auto& n1 = *it1;
	++n1;
	_ASSERT(tc::equal(tc::single(6), on));
	_ASSERTEQUAL(tc::size(on), 1);
	tc::for_each(on, [](auto const n) noexcept {
		_ASSERTEQUAL(n, 6);
	});

	std::optional<int> const on2 = 8;
	auto itBegin = tc::begin(on2);
	auto itEnd = tc::end(on2);
	_ASSERTEQUAL(itBegin+1, itEnd);
	_ASSERTEQUAL(itEnd-1, itBegin);
	_ASSERTEQUAL(itEnd-itBegin, 1);
	++itBegin;
	_ASSERTEQUAL(itBegin, itEnd);
	--itBegin;
	_ASSERTEQUAL(itBegin, tc::begin(on2));

	constexpr std::optional<bool> ob;
	static_assert(!tc::any_of(ob));

	constexpr std::optional<int> on3 = 10;
	constexpr int n3 = *tc::begin(on3);
	static_assert(n3 == 10);
	static_assert(tc::all_of(on3, [](auto const n) constexpr noexcept { return 8 < n; }));
}
