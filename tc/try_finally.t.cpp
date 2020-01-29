// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.t.h"
#include "try_finally.h"

UNITTESTDEF( try_finally_no_exception ) {
	int state = 0;
	int result = tc::try_finally(
		[&]() {
			_ASSERTEQUAL(state, 0);
			state = 1;
			return 5;
		},
		[&]() {
			_ASSERTEQUAL(state, 1);
			state = 2;
		}
	);
	_ASSERTEQUAL(state, 2);
	_ASSERTEQUAL(result, 5);
}

UNITTESTDEF( try_finally_void_return ) {
	int state = 0;
	tc::try_finally(
		[&]() {
			_ASSERTEQUAL(state, 0);
			state = 1;
		},
		[&]() {
			_ASSERTEQUAL(state, 1);
			state = 2;
		}
	);
	_ASSERTEQUAL(state, 2);
}

UNITTESTDEF( try_finally_exception ) {
	int state = 0;
	try {
		tc::try_finally(
			[&]() {
				_ASSERTEQUAL(state, 0);
				state = 1;
				throw 0.0;
			},
			[&]() {
				_ASSERTEQUAL(state, 1);
				state = 2;
			}
		);
		_ASSERTFALSE;

	} catch(double const&) {
		_ASSERTEQUAL(state, 2);
		state = 3;
	}

	_ASSERTEQUAL(state, 3);
}

UNITTESTDEF(try_cleanup_no_exception) {
	int state = 0;
	int result = tc::try_cleanup(
		[&]() {
			_ASSERTEQUAL(state, 0);
			state = 1;
			return 5;
		},
		[&]() {
			_ASSERTFALSE;
		}
	);
	_ASSERTEQUAL(state, 1);
	_ASSERTEQUAL(result, 5);
}

UNITTESTDEF( try_cleanup_void_return ) {
	int state = 0;
	tc::try_cleanup(
		[&]() {
			_ASSERTEQUAL(state, 0);
			state = 1;
		},
		[&]() {
			_ASSERTFALSE;
		}
	);
	_ASSERTEQUAL(state, 1);
}

UNITTESTDEF(try_cleanup_exception) {
	int state = 0;
	try {
		tc::try_cleanup(
			[&]() {
				_ASSERTEQUAL(state, 0);
				state = 1;
				throw 0.0;
			},
				[&]() {
				_ASSERTEQUAL(state, 1);
				state = 2;
			}
		);
		_ASSERTFALSE; // Should not be reached

	} catch (double const&) {
		_ASSERTEQUAL(state, 2);
		state = 3;
	}

	_ASSERTEQUAL(state, 3);
}

// static tests
namespace {
	struct not_callable {};
	struct callable_noexcept { void operator()() noexcept {} };
	struct callable_except { void operator()() {} };

	static_assert(noexcept(tc::try_finally(callable_noexcept(), callable_noexcept())));
	static_assert(!noexcept(tc::try_finally(callable_except(), callable_noexcept())));
	static_assert(!noexcept(tc::try_finally(callable_noexcept(), callable_except())));

	static_assert(noexcept(tc::try_cleanup(callable_noexcept(), callable_noexcept())));
	static_assert(!noexcept(tc::try_cleanup(callable_except(), callable_noexcept())));
	static_assert(noexcept(tc::try_cleanup(callable_noexcept(), callable_except()))); // note difference from try_finally, since the cleanup functor cannot be called in this case.

	static_assert(noexcept(tc::try_cleanup(callable_noexcept(), not_callable()))); // This should compile, because the cleanup functor is not used.
}
