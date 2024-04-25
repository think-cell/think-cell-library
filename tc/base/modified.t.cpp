
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "modified.h"
#include "../unittest.h"

UNITTESTDEF(tc_modified) {
	int lvalue = 0;
	static_assert(!tc::inplace_modifiable<int&>);
	_ASSERTEQUAL(tc_modified(lvalue, _ = 11), 11);
	_ASSERTEQUAL(lvalue, 0);

	int const clvalue = 0;
	static_assert(!tc::inplace_modifiable<int const&>);
	_ASSERTEQUAL(tc_modified(clvalue, _ = 11), 11);
	_ASSERTEQUAL(clvalue, 0);

	int rvalue = 0;
	static_assert(tc::inplace_modifiable<int&&>);
	_ASSERTEQUAL(tc_modified(tc_move(rvalue), _ = 11), 11);
	_ASSERTEQUAL(rvalue, 11);

	int const crvalue = 0;
	static_assert(!tc::inplace_modifiable<int const&&>);
	_ASSERTEQUAL(tc_modified(tc_move_always_even_const(crvalue), _ = 11), 11);
	_ASSERTEQUAL(crvalue, 0);

	int tmp_rvalue = 0;
	static_assert(tc::inplace_modifiable<tc::temporary<int, 0>&&>);
	_ASSERTEQUAL(static_cast<int&&>(tc_modified((tc::temporary<int, 0>(tmp_rvalue)), _ = 11)), 11);
	_ASSERTEQUAL(tmp_rvalue, 11);

	int tmp_crvalue = 0;
	static_assert(!tc::inplace_modifiable<tc::temporary<int const, 0>&&>);
	_ASSERTEQUAL(tc_modified((tc::temporary<int const, 0>(tmp_crvalue)), _ = 11), 11);
	_ASSERTEQUAL(tmp_crvalue, 0);

	int tmp_lvalue_i = 0;
	tc::temporary<int, 0> tmp_lvalue(tmp_lvalue_i);
	static_assert(!tc::inplace_modifiable<tc::temporary<int, 0>&>);
	_ASSERTEQUAL(tc_modified(tmp_lvalue, _ = 11), 11);
	_ASSERTEQUAL(tmp_lvalue_i, 0);

	int tmp_clvalue_i = 0;
	static_assert(!tc::inplace_modifiable<tc::temporary<int const, 0>&>);
	tc::temporary<int const, 0> tmp_clvalue(tmp_clvalue_i);
	_ASSERTEQUAL(tc_modified(tmp_clvalue, _ = 11), 11);
	_ASSERTEQUAL(tmp_clvalue_i, 0);
}
