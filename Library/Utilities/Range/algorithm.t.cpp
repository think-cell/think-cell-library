#include "../Range.h"
#include "range.t.h"

#include <vector>

namespace {
	using namespace RANGE_PROPOSAL_NAMESPACE;

	void static_tests() {
	}

UNITTESTDEF( quantifiers ) {

	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7});

	TEST_init_hack(std::vector, int, all_even, {2,4,6,8});
	TEST_init_hack(std::vector, int, all_odd, {3,5,7,9});

	auto even = [](int i){ return (i%2==0); };

	const int existing_value = 5;
	const int non_existing_value = 9;

	auto const_range = make_iterator_range(make_const(v)); TEST_RANGE_LENGTH(const_range, 7);
	
	_ASSERT( contains(const_range, existing_value));
	_ASSERT(!contains(const_range, non_existing_value));

	_ASSERT(! all_of(const_range, even));
	_ASSERT(  any_of(const_range, even));
	_ASSERT(!none_of(const_range, even));

	_ASSERT(  all_of(all_even, even));
	_ASSERT(  any_of(all_even, even));
	_ASSERT(!none_of(all_even, even));

	_ASSERT(! all_of(all_odd, even));
	_ASSERT(! any_of(all_odd, even));
	_ASSERT( none_of(all_odd, even));
}

UNITTESTDEF( sort_accumulate_each_unique_range_2 ) {
	struct SValAccu {
		SValAccu(int val, int accu) : m_val(val), m_accu(accu) {}
		int m_val;
		int m_accu;
	};
	{
		std::vector< SValAccu > vec;
		for( int i=0; i < 5; ++i ) {
			vec.emplace_back( 1, 1 );
		}
		tc::sort_accumulate_each_unique_range(
			vec,
			[](SValAccu const& lhs, SValAccu const& rhs) { return lhs.m_val < rhs.m_val; },
			[](SValAccu& lhs, SValAccu const& rhs) { lhs.m_accu+=rhs.m_accu; }
		);
		TEST_EQUAL( 1, vec.size() );
		TEST_EQUAL( 1, vec.front().m_val );
		TEST_EQUAL( 5, vec.front().m_accu );
	}
}

UNITTESTDEF( trim_leftright_if ) {
	TEST_init_hack(std::vector, int, v, {1,2,3,4,5,6,7,7,7});
	auto rng = trim_left_if(v, [] (int n) {return n<4;});
	_ASSERT(std::begin(rng) != std::end(rng));
	_ASSERTEQUAL(size(rng), 6);
	_ASSERTEQUAL(size(trim_right_if(rng, [] (int n) {return n==7;})), 3);
}

UNITTESTDEF( is_sorted ) {
	{
		int a[]={0};
		_ASSERT( boost::range::is_sorted(a) );
	}
	{
		int a[]={0,0};
		_ASSERT( boost::range::is_sorted(a) );
	}
	{
		int a[]={0,1};
		_ASSERT( boost::range::is_sorted(a) );
	}
	{
		int a[]={1,0};
		_ASSERT( !boost::range::is_sorted(a) );
	}
	{
		int a[]={0};
		_ASSERT( tc::is_strictly_sorted(a) );
	}
	{
		int a[]={0,0};
		_ASSERT( !tc::is_strictly_sorted(a) );
	}
	{
		int a[]={0,1};
		_ASSERT( tc::is_strictly_sorted(a) );
	}
	{
		int a[]={1,0};
		_ASSERT( !tc::is_strictly_sorted(a) );
	}
}

}
