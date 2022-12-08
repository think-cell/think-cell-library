
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../algorithm/empty.h"
#include "../container/insert.h"

namespace {

struct empty_generator { 
  template<typename Func> tc::break_or_continue operator()(Func) const { return tc::continue_; }
};

struct non_empty_generator { 
  template<typename Func> tc::break_or_continue operator()(Func func) const { return tc::continue_if_not_break(func, 1); }
};

UNITTESTDEF( empty_range ) {
	{ // test container with empty() method
		tc::vector<int> vec;
		static_assert( has_mem_fn_empty<decltype(vec)>::value );
		_ASSERT( tc::empty(vec) );
		_ASSERT( tc::empty(vec) );

		tc::cont_emplace_back(vec, 1);
		_ASSERT( !tc::empty(vec) );
		_ASSERT( !tc::empty(vec) );
	}
	{ // test iterator range
		static_assert( !has_mem_fn_empty<tc::decay_t<decltype("")> >::value );
		static_assert( tc::is_range_with_iterators<decltype("")>::value );
		_ASSERT( tc::empty("") );

		static_assert( !has_mem_fn_empty<tc::decay_t<decltype("x")> >::value );
		static_assert( tc::is_range_with_iterators<decltype("x")>::value );
		_ASSERT( !tc::empty("x") );
	}
	{ // test generator range
		_ASSERT( tc::empty( empty_generator() ) );
		_ASSERT( !tc::empty( non_empty_generator() ) );
	}
}

}

