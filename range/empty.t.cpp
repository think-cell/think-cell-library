//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#include "range.t.h"
#include "empty.h"

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
		static_assert( has_mem_fn_empty<decltype(vec)>::value, "" );
		_ASSERT( tc::empty(vec) );
		_ASSERT( tc::empty(vec) );

		vec.emplace_back(1);
		_ASSERT( !tc::empty(vec) );
		_ASSERT( !tc::empty(vec) );
	}
	{ // test iterator range
		static_assert( !has_mem_fn_empty<std::decay_t<decltype("")> >::value, "" );
		static_assert( tc::is_range_with_iterators<decltype("")>::value, "" );
		_ASSERT( tc::empty("") );

		static_assert( !has_mem_fn_empty<std::decay_t<decltype("x")> >::value, "" );
		static_assert( tc::is_range_with_iterators<decltype("x")>::value, "" );
		_ASSERT( !tc::empty("x") );
	}
	{ // test generator range
		_ASSERT( tc::empty( empty_generator() ) );
		_ASSERT( !tc::empty( non_empty_generator() ) );
	}
}

}
