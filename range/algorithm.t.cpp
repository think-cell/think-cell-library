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

#include "range.h"
#include "container.h" // tc::vector
#include "range.t.h"

#include <boost/core/ignore_unused.hpp>

namespace {
	using namespace tc;

	void static_tests() noexcept {
		// make_container with explicit move constructor
		struct SMove { explicit SMove(tc::vector<int>&&) noexcept {} };
		tc::make_container<SMove>(tc::vector<int>());

		// make_container with explicit constructor
		struct SCopy { explicit SCopy(tc::vector<int>) noexcept {} };
		tc::make_container<SCopy>(tc::vector<int>());
	}

UNITTESTDEF( quantifiers ) {

	tc::vector<int> v{1,2,3,4,5,6,7};

	tc::vector<int> all_even{2,4,6,8};
	tc::vector<int> all_odd{3,5,7,9};

	auto even = [](int i) noexcept { return (i%2==0); };

	int const existing_value = 5;
	int const non_existing_value = 9;

	auto const_range = slice(tc::as_const(v)); TEST_RANGE_LENGTH(const_range, 7);
	
	_ASSERT( tc::find_first<tc::return_bool>(const_range, existing_value));
	_ASSERT(!tc::find_first<tc::return_bool>(const_range, non_existing_value));

	_ASSERT(! all_of(const_range, even));
	_ASSERT(  any_of(const_range, even));

	_ASSERT(  all_of(all_even, even));
	_ASSERT(  any_of(all_even, even));

	_ASSERT(! all_of(all_odd, even));
	_ASSERT(! any_of(all_odd, even));
}

UNITTESTDEF( sort_accumulate_each_unique_range_2 ) {
	struct SValAccu final {
		SValAccu(int val, int accu) noexcept : m_val(val), m_accu(accu) {}
		int m_val;
		int m_accu;
	};
	{
		tc::vector< SValAccu > vec;
		for( int i=0; i < 5; ++i ) {
			vec.emplace_back( 1, 1 );
		}
		tc::sort_accumulate_each_unique_range(
			vec,
			[](SValAccu const& lhs, SValAccu const& rhs) noexcept { return lhs.m_val < rhs.m_val; },
			[](SValAccu& lhs, SValAccu const& rhs) noexcept { lhs.m_accu+=rhs.m_accu; }
		);
		TEST_EQUAL( 1, vec.size() );
		TEST_EQUAL( 1, tc_front(vec).m_val );
		TEST_EQUAL( 5, tc_front(vec).m_accu );
	}
}

UNITTESTDEF(filter_no_self_assignment_of_rvalues) {
	struct S {
		S() {}

		S(S const&){}

		S& operator=(S&& other) {
			_ASSERT(&other != this);
			boost::ignore_unused(other);
			return *this;
		}
	};

	tc::vector<S> vs{5,S{}};
	tc::sort_accumulate_each_unique_range(
		vs,
		[&](auto const&, auto const&) noexcept {
			return false;
		},
		[&](auto&, auto const&) noexcept {
		}
	);
}


UNITTESTDEF( trim_leftright_if ) {
	tc::vector<int> v{1,2,3,4,5,6,7,7,7};
	auto rng = trim_left_if<tc::return_drop>(v, [] (int n) noexcept {return n<4;});
	_ASSERT(std::begin(rng) != std::end(rng));
	_ASSERTEQUAL(tc::size(rng), 6);
	_ASSERTEQUAL(tc::size(tc::trim_right_if<tc::return_take>(rng, [] (int n) noexcept {return n==7;})), 3);
}

UNITTESTDEF( is_sorted ) {
	{
		int a[]={0};
		_ASSERT( tc::is_sorted(a) );
		boost::ignore_unused(a);
	}
	{
		int a[]={0,0};
		_ASSERT( tc::is_sorted(a) );
		boost::ignore_unused(a);
	}
	{
		int a[]={0,1};
		_ASSERT( tc::is_sorted(a) );
		boost::ignore_unused(a);
	}
	{
		int a[]={1,0};
		_ASSERT( !tc::is_sorted(a) );
		boost::ignore_unused(a);
	}
	{
		int a[]={0};
		_ASSERT( tc::is_strictly_sorted(a) );
		boost::ignore_unused(a);
	}
	{
		int a[]={0,0};
		_ASSERT( !tc::is_strictly_sorted(a) );
		boost::ignore_unused(a);
	}
	{
		int a[]={0,1};
		_ASSERT( tc::is_strictly_sorted(a) );
		boost::ignore_unused(a);
	}
	{
		int a[]={1,0};
		_ASSERT( !tc::is_strictly_sorted(a) );
		boost::ignore_unused(a);
	}
}

UNITTESTDEF( make_vector_on_r_vector_is_identity ) {
	tc::vector<int> v{1,2,3};
	auto pvecdata = v.data();
	boost::ignore_unused(pvecdata);

	auto vNew = tc::make_vector(tc_move(v));
	_ASSERT( vNew.data() == pvecdata );
}

UNITTESTDEF(find_closest_if) {
	struct IntCompareOnce final : boost::noncopyable {
		IntCompareOnce(int n) noexcept :m_n(n) { }
		bool operator==(int n) const& noexcept {
			_ASSERT( change(m_bCompared, true) );
			return m_n==n;
		}
	private:
		int m_n;
		bool mutable m_bCompared = false;
	};

	auto find=[](auto const& rngn, int iStart, int nTarget, int nComparisonsMax) noexcept {
		int nComparisons = 0;
		return tc::find_closest_if<tc::return_element_index_or_npos>(rngn, tc::begin_next(rngn, iStart), /*bSkipSelf*/false, [&](IntCompareOnce const& n) noexcept {
			_ASSERT(++nComparisons<=nComparisonsMax);
			return n==nTarget;
		});
	};

	for (int iStart = 0; iStart < 4; ++iStart) {
		for (int nTarget = -1; nTarget < 4; ++nTarget) {
			int nComparisonsMax = -1==nTarget ? 5 : nTarget<iStart ? 2*(iStart-nTarget) : 1+2*(nTarget-iStart);
			_ASSERTEQUAL(find(std::initializer_list<IntCompareOnce>{{0},{1},{2},{3},{4}}, iStart, nTarget, nComparisonsMax), nTarget);
			boost::ignore_unused(nComparisonsMax);
		}
	}
}

UNITTESTDEF(rangefilter_on_subrange) {
	tc::vector<int> vecn={2,3,3,0, /*rngn starts here*/ 3,4,5,6,4,5,6,7};
	auto rngn=tc::drop_first(vecn, 4);

	{
		tc::sort_unique_inplace(rngn); // uses range_filter<tc::sub_range<tc::vector<int>&> > internally
		int const anExpected[]={2,3,3,0, /*rngn starts here*/ 3,4,5,6,7};
		_ASSERT(tc::equal(vecn, anExpected));
		_ASSERT(boost::begin(rngn)==tc::begin_next(vecn,4));
		_ASSERT(boost::end(rngn)==boost::end(vecn));
	}

	{
		{
			range_filter<tc::sub_range<tc::vector<int>&> > filter(rngn);
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

UNITTESTDEF(is_strictly_sorted){
	int an[]={0,1,2,3,4,5};
	_ASSERT(tc::is_strictly_sorted(an));
	_ASSERT(!tc::is_strictly_sorted(tc::reverse(an)));
}

}
