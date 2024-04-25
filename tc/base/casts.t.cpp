
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "casts.h"
#include "../unittest.h"

#include <utility>

namespace {
	[[maybe_unused]] void static_tests_as_const() {
		int value;
		tc::temporary<int, 0> tmp(value);

		STATICASSERTSAME(decltype(tc::as_const(value)), int const&);
		STATICASSERTSAME(decltype(tc::as_const(tmp)), int const&);
		STATICASSERTSAME(decltype(tc::as_const(0)), int&&);
		STATICASSERTSAME(decltype(tc::as_const(tc::temporary<int, 0>(0))), TC_FWD(tc::temporary<int, 0>&&));
	}
}

namespace {
	struct base{};
	struct derived final : base{};
}

UNITTESTDEF(DerivedCastTests) {
	base b;
	derived& d = tc::derived_cast<derived>(b);
	_ASSERTEQUAL(std::addressof(b), std::addressof(d));

	derived&& drref = tc::derived_cast<derived>(tc_move_if_owned(b));
	_ASSERTEQUAL(std::addressof(b), std::addressof(drref));
}
