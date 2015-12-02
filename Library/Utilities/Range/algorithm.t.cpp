#include "../Range.h"
#include "range.t.h"

#include <vector>

namespace {
	using namespace RANGE_PROPOSAL_NAMESPACE;

	void static_tests() {
		// copy_range with explicit move constructor
		struct SMove { explicit SMove(std::vector<int>&&) {} };
		tc::copy_range<SMove>(std::vector<int>());

		// copy_range with explicit constructor
		struct SCopy { explicit SCopy(std::vector<int>) {} };
		tc::copy_range<SCopy>(std::vector<int>());
	}

UNITTESTDEF( quantifiers ) {

	std::vector<int> v{1,2,3,4,5,6,7};

	std::vector<int> all_even{2,4,6,8};
	std::vector<int> all_odd{3,5,7,9};

	auto even = [](int i){ return (i%2==0); };

	int const existing_value = 5;
	int const non_existing_value = 9;

	auto const_range = slice(make_const(v)); TEST_RANGE_LENGTH(const_range, 7);
	
	_ASSERT( tc::find_first<tc::return_bool>(const_range, existing_value));
	_ASSERT(!tc::find_first<tc::return_bool>(const_range, non_existing_value));

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
		TEST_EQUAL( 1, tc_front(vec).m_val );
		TEST_EQUAL( 5, tc_front(vec).m_accu );
	}
}

UNITTESTDEF( trim_leftright_if ) {
	std::vector<int> v{1,2,3,4,5,6,7,7,7};
	auto rng = trim_left_if(v, [] (int n) {return n<4;});
	_ASSERT(std::begin(rng) != std::end(rng));
	_ASSERTEQUAL(tc::size(rng), 6);
	_ASSERTEQUAL(tc::size(tc::trim_right_if(rng, [] (int n) {return n==7;})), 3);
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

UNITTESTDEF( make_vector_on_r_vector_is_identity ) {
	std::vector<int> v{1,2,3};
	auto pvecdata = v.data();

	auto vNew = tc::make_vector(tc_move(v));
	_ASSERT( vNew.data() == pvecdata );
}

UNITTESTDEF(find_closest_if) {
	struct IntCompareOnce : boost::noncopyable {
		IntCompareOnce(int n) :m_n(n), m_bCompared(false) { }
		bool operator==(int n) const {
			_ASSERT( change(m_bCompared, true) );
			return m_n==n;
		}
	private:
		int m_n;
		bool mutable m_bCompared;
	};

	auto find=[](auto const& rngn, int iStart, int nTarget, int nComparisonsMax) {
		int nComparisons = 0;
		return tc::find_closest_if<tc::return_index_or_npos>(rngn, tc::begin_next(rngn,iStart), [&](IntCompareOnce const& n) {
			_ASSERT(++nComparisons<=nComparisonsMax);
			return n==nTarget;
		});
	};

	for (int iStart = 0; iStart < 4; ++iStart) {
		for (int nTarget = -1; nTarget < 4; ++nTarget) {
			int nComparisonsMax = -1==nTarget ? 5 : nTarget<iStart ? 2*(iStart-nTarget) : 1+2*(nTarget-iStart);
			_ASSERTEQUAL(find(std::initializer_list<IntCompareOnce>{{0},{1},{2},{3},{4}}, iStart, nTarget, nComparisonsMax), nTarget);
		}
	}
}

UNITTESTDEF(rangefilter_on_subrange) {
	std::vector<int> vecn={2,3,3,0, /*rngn starts here*/ 3,4,5,6,4,5,6,7};
	auto rngn=tc::drop_first(vecn, 4);

	{
		tc::sort_unique_inplace(rngn); // uses range_filter<tc::sub_range<std::vector<int>&> > internally
		int const anExpected[]={2,3,3,0, /*rngn starts here*/ 3,4,5,6,7};
		_ASSERT(tc::equal(vecn, anExpected));
		_ASSERT(boost::begin(rngn)==tc::begin_next(vecn,4));
		_ASSERT(boost::end(rngn)==boost::end(vecn));
	}

	{
		{
			range_filter<tc::sub_range<std::vector<int>&> > filter(rngn);
			auto it=boost::begin(rngn);
			filter.keep(it++);
			filter.keep(it++);
			filter.keep(it++);
			int i=0;
			while(boost::begin(filter)!=boost::end(filter)) {
				tc::drop_last_inplace(filter);
				++i;
			}
			_ASSERTEQUAL(i,3);
		}
		int const anExpected[]={2,3,3,0, /*rngn starts here*/};
		_ASSERT(tc::equal(vecn, anExpected));
		_ASSERT(boost::begin(rngn)==tc::begin_next(vecn,4));
		_ASSERT(boost::end(rngn)==boost::end(vecn));
	}
}

}
