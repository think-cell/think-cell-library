#include "range.t.h"
#include "empty.h"

namespace {

struct empty_generator { 
  template<typename Func> RANGE_PROPOSAL_NAMESPACE::break_or_continue operator()(Func) const { return RANGE_PROPOSAL_NAMESPACE::continue_; }
};

struct non_empty_generator { 
  template<typename Func> RANGE_PROPOSAL_NAMESPACE::break_or_continue operator()(Func func) const { return RANGE_PROPOSAL_NAMESPACE::continue_if_void(func, 1); }
};

UNITTESTDEF( empty_range ) {
	{ // test container with empty() method
		std::vector<int> vec;
		static_assert( has_mem_fn_empty<typename std::decay<decltype(vec)>::type>::value, "" );
		_ASSERT( vec.empty() );
		_ASSERT( RANGE_PROPOSAL_NAMESPACE::empty(vec) );

		vec.emplace_back(1);
		_ASSERT( !vec.empty() );
		_ASSERT( !RANGE_PROPOSAL_NAMESPACE::empty(vec) );
	}
	{ // test iterator range
		static_assert( !has_mem_fn_empty<typename std::decay<decltype("")>::type>::value, "" );
		static_assert( RANGE_PROPOSAL_NAMESPACE::is_range_with_iterators<typename std::decay<decltype("")>::type>::value, "" );
		_ASSERT( boost::empty("") );
		_ASSERT( RANGE_PROPOSAL_NAMESPACE::empty("") );

		static_assert( !has_mem_fn_empty<typename std::decay<decltype("x")>::type>::value, "" );
		static_assert( RANGE_PROPOSAL_NAMESPACE::is_range_with_iterators<typename std::decay<decltype("x")>::type>::value, "" );
		_ASSERT( !boost::empty("x") );
		_ASSERT( !RANGE_PROPOSAL_NAMESPACE::empty("x") );
	}
	{ // test generator range
		_ASSERT( RANGE_PROPOSAL_NAMESPACE::empty( empty_generator() ) );
		_ASSERT( !RANGE_PROPOSAL_NAMESPACE::empty( non_empty_generator() ) );
	}
}

}
