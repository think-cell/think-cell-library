
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "range.h"
#include "container.h" // tc::vector
#include "range.t.h"
#include "concat_adaptor.h"
#include "join_adaptor.h"
#include "spirit_algorithm.h"

#include <random>

namespace {
	[[maybe_unused]] void static_tests() noexcept {
		// explicit_cast with explicit move constructor
		struct SMove { explicit SMove(tc::vector<int>&&) noexcept {} };
		void(tc::explicit_cast<SMove>(tc::vector<int>{}));

		// explicit_cast with explicit constructor
		struct SCopy { explicit SCopy(tc::vector<int>) noexcept {} };
		void(tc::explicit_cast<SCopy>(tc::vector<int>{}));
	}

UNITTESTDEF( quantifiers ) {

	tc::vector<int> v{1,2,3,4,5,6,7};

	tc::vector<int> all_even{2,4,6,8};
	tc::vector<int> all_odd{3,5,7,9};

	auto even = [](int i) noexcept { return i%2==0; };

	int const existing_value = 5;
	int const non_existing_value = 9;

	auto const_range = tc::slice(tc::as_const(v)); TEST_RANGE_LENGTH(const_range, 7);

	_ASSERT( tc::find_first<tc::return_bool>(const_range, existing_value));
	_ASSERT(!tc::find_first<tc::return_bool>(const_range, non_existing_value));

	_ASSERT(! tc::all_of(const_range, even));
	_ASSERT(  tc::any_of(const_range, even));

	_ASSERT(  tc::all_of(all_even, even));
	_ASSERT(  tc::any_of(all_even, even));

	_ASSERT(! tc::all_of(all_odd, even));
	_ASSERT(! tc::any_of(all_odd, even));
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
			tc::cont_emplace_back( vec, 1, 1 );
		}
		tc::sort_accumulate_each_unique_range(
			vec,
			[](SValAccu const& lhs, SValAccu const& rhs) noexcept { return lhs.m_val < rhs.m_val; },
			[](SValAccu& lhs, SValAccu const& rhs) noexcept { lhs.m_accu+=rhs.m_accu; }
		);
		TEST_EQUAL( 1, vec.size() );
		TEST_EQUAL( 1, tc::front(vec).m_val );
		TEST_EQUAL( 5, tc::front(vec).m_accu );
	}
}

UNITTESTDEF(filter_no_self_assignment_of_rvalues) {
	struct S {
		S() {}

		S(S const&){}

		S& operator=(S&& other) {
			_ASSERT(&other != this);
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
	auto rng = tc::trim_left_if<tc::return_drop>(v, [] (int n) noexcept {return n<4;});
	_ASSERT(tc::begin(rng) != tc::end(rng));
	_ASSERTEQUAL(tc::size(rng), 6);
	_ASSERTEQUAL(tc::size(tc::trim_right_if<tc::return_take>(rng, [] (int n) noexcept {return n==7;})), 3);
}

UNITTESTDEF( is_sorted ) {
	{
		int a[]={0};
		_ASSERT( tc::is_sorted(a) );
	}
	{
		int a[]={0,0};
		_ASSERT( tc::is_sorted(a) );
	}
	{
		int a[]={0,1};
		_ASSERT( tc::is_sorted(a) );
	}
	{
		int a[]={1,0};
		_ASSERT( !tc::is_sorted(a) );
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
	tc::vector<int> v{1,2,3};
	auto pvecdata = v.data();

	auto vNew = tc::make_vector(tc_move(v));
	_ASSERTEQUAL(vNew.data(), pvecdata);
}

UNITTESTDEF(find_closest_if) {
	struct IntCompareOnce final : boost::noncopyable {
		IntCompareOnce(int n) noexcept :m_n(n) { }
		bool operator==(int n) const& noexcept {
			_ASSERT( tc::change(m_bCompared, true) );
			return m_n==n;
		}
	private:
		int m_n;
		bool mutable m_bCompared = false;
	};

	auto find=[](auto const& rngn, int iStart, int nTarget, int nComparisonsMax) noexcept {
		int nComparisons = 0;
		return tc::find_closest_if_with_index<tc::return_element_index_or_npos>(rngn, iStart, /*bSkipSelf*/false, [&](IntCompareOnce const& n) noexcept {
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
	tc::vector<int> vecn={2,3,3,0, /*rngn starts here*/ 3,4,5,6,4,5,6,7};
	auto rngn=tc::begin_next<tc::return_drop>(vecn, 4);

	{
		tc::sort_unique_inplace(rngn); // uses range_filter<tc::subrange<tc::vector<int>&> > internally
		int const anExpected[]={2,3,3,0, /*rngn starts here*/ 3,4,5,6,7};
		_ASSERT(tc::equal(vecn, anExpected));
		_ASSERTEQUAL(tc::begin(rngn), tc::begin_next<tc::return_border>(vecn,4));
		_ASSERTEQUAL(tc::end(rngn), tc::end(vecn));
	}

	{
		{
			tc::range_filter<tc::subrange<tc::vector<int>&> > filter(rngn);
			auto it=tc::begin(rngn);
			filter.keep(it++);
			filter.keep(it++);
			filter.keep(it++);
			int i=0;
			while(tc::begin(filter)!=tc::end(filter)) {
				tc::drop_last_inplace(filter);
				++i;
			}
			_ASSERTEQUAL(i,3);
		}
		int const anExpected[]={2,3,3,0, /*rngn starts here*/};
		_ASSERT(tc::equal(vecn, anExpected));
		_ASSERTEQUAL(tc::begin(rngn), tc::begin_next<tc::return_border>(vecn,4));
		_ASSERTEQUAL(tc::end(rngn), tc::end(vecn));
	}
}

UNITTESTDEF(is_strictly_sorted){
	int an[]={0,1,2,3,4,5};
	_ASSERT(tc::is_strictly_sorted(an));
	_ASSERT(!tc::is_strictly_sorted(tc::reverse(an)));
}

UNITTESTDEF(remove_inplace_parser) {
	std::basic_string<char> input = "0123<font>4567<font10><font11><font12>89<font14><font";
	tc::remove_inplace(input, tc::asciilit("<font") > *(tc::char_<char> - tc::asciilit(">")) > tc::asciilit(">"));
	_ASSERTEQUAL(input, "0123456789<font");
}

UNITTESTDEF(Naryinterleave) {
	tc::vector<int> const vecnA({3,4,7,9});
	tc::vector<int> vecnB({2,4,8,9,11,17});
	auto const vecnBCopy = vecnB;
	tc::vector<int> const vecnC({-100,1000});

	tc::vector<int> vecnResult = tc::make_vector(vecnA, vecnB, vecnC);
	tc::sort_inplace(vecnResult);
	auto itResult = tc::begin(vecnResult);

	_ASSERTEQUAL(
		tc::continue_,
		tc::interleave_n(
			tc::fn_compare(),
			[&](auto const&... pairitb) noexcept {
				auto tplpairitb = std::make_tuple(pairitb...);
				if (std::get<0>(tplpairitb).second) {
					_ASSERTEQUAL(*itResult,*std::get<0>(tplpairitb).first);
					++itResult;
				}
				if (std::get<1>(tplpairitb).second) {
					_ASSERTEQUAL(*itResult,*std::get<1>(tplpairitb).first);
					*std::get<1>(tplpairitb).first += 100;
					++itResult;
				}
				if (std::get<2>(tplpairitb).second) {
					_ASSERTEQUAL(*itResult,*std::get<2>(tplpairitb).first);
					++itResult;
				}
			},
			vecnA, vecnB, vecnC
		)
	);

	_ASSERT(tc::equal(
		tc::transform(vecnBCopy, [](int n) noexcept {return n+100;}),
		vecnB
	));
}

UNITTESTDEF(NaryinterleaveBreak) {
	tc::vector<int> const vecnA({ 3,4,7,9 });
	tc::vector<int> vecnB({ 2,4,8,9,11,17 });
	auto const vecnBCopy = vecnB;
	tc::vector<int> const vecnC({ -100,1000 });

	tc::vector<int> vecnResult = tc::make_vector(vecnA, vecnB, vecnC);
	tc::sort_inplace(vecnResult);
	auto itResult = tc::begin(vecnResult);

	_ASSERTEQUAL(
		tc::break_,
		tc::interleave_n(
			tc::fn_compare(),
			[&](auto const&... pairitb) noexcept {
				auto tplpairitb = std::make_tuple(pairitb...);
				if (std::get<0>(tplpairitb).second) {
					_ASSERTEQUAL(*itResult, *std::get<0>(tplpairitb).first);
					++itResult;
					if (7 == *std::get<0>(tplpairitb).first) return tc::break_;
				}
				if (std::get<1>(tplpairitb).second) {
					_ASSERTEQUAL(*itResult, *std::get<1>(tplpairitb).first);
					*std::get<1>(tplpairitb).first += 100;
					++itResult;
				}
				if (std::get<2>(tplpairitb).second) {
					_ASSERTEQUAL(*itResult, *std::get<2>(tplpairitb).first);
					++itResult;
				}
				return tc::continue_;
			},
			vecnA, vecnB, vecnC
		)
	);

	_ASSERT(tc::equal(
		tc::transform(vecnBCopy, [](int n) noexcept {return n < 7 ? n + 100 : n; }),
		vecnB
	));
}

UNITTESTDEF(plurality_element_test) {
	auto const str = "abcdc";
	_ASSERTEQUAL('c', tc::plurality_element<tc::return_value>(str));
	auto const str2 = "";
	_ASSERTEQUAL(std::nullopt, tc::plurality_element<tc::return_value_or_none>(str2));
	auto const str3 = "a";
	_ASSERTEQUAL(tc::begin(str3),tc::plurality_element<tc::return_element>(str3));
	int an[] = {1,2,3,4,3,4,3,5,6};
	_ASSERTEQUAL(std::optional(4), tc::plurality_element<tc::return_value_or_none>(tc::filter(an, [](auto n) noexcept { return n % 2 == 0; })));
}

static_assert(std::is_move_constructible<decltype(tc::sort(std::declval<std::basic_string<char> const&>()))>::value);
static_assert(!std::is_move_constructible<decltype(tc::sort(std::declval<std::basic_string<char>>()))>::value);
static_assert(std::is_move_constructible<decltype(tc::sort(std::declval<tc::vector<int> const&>()))>::value);
static_assert(std::is_move_constructible<decltype(tc::sort(std::declval<tc::vector<int>>()))>::value);

UNITTESTDEF(sort_test) {
	tc::vector<int> vec1{6,1,2,9,5,0,3,4,7,8};
	_ASSERT(tc::equal(tc::sort(vec1), tc::iota(0, 10)));
	auto rngnSorted=tc::sort(tc::vector<int>{3,7,1,9,2,5,8,4,6,0});
	_ASSERT(tc::equal(rngnSorted, tc::iota(0, 10)));
	tc::vector<std::pair<int, int>> vecpairnn1{{5,0}, {3,0}, {0,0}, {6,0}, {1,0}, {5,1}, {1,1}, {5,2}, {3,1}, {0,1}, {0,2}, {6,1}};
	tc::vector<std::pair<int, int>> vecpairnn2{{0,0}, {0,1}, {0,2}, {1,0}, {1,1}, {3,0}, {3,1}, {5,0}, {5,1}, {5,2}, {6,0}, {6,1}};
	auto const rngpairnnSorted=tc::stable_sort(tc_move(vecpairnn1),tc::projected(tc::fn_compare(),[](auto const& pairnn) noexcept { return pairnn.first; }));
	_ASSERT(tc::equal(rngpairnnSorted, vecpairnn2));
}

#ifdef __clang__ // remove if std::sort is constexpr in xcode
UNITTESTDEF(constexpr_sort_test) {
	std::mt19937 gen; // same sequence of numbers each time for reproducibility
	std::uniform_int_distribution<> dist(0, 63);

	auto Test = [](auto rngn) noexcept {
		auto vecn = tc::explicit_cast<tc::vector<int>>(rngn);
		_ASSERTEQUAL(modified(vecn, tc::sort_inplace(_)), modified(vecn, tc::constexpr_sort_inplace_detail::constexpr_sort_inplace(tc::begin(_), tc::end(_), tc::fn_less())));
	};

	for( int i = 0; i < 17; ++i ) {
		Test(tc::begin_next<tc::return_take>([&](auto sink) noexcept { for(;;) RETURN_IF_BREAK( tc::continue_if_not_break(sink, dist(gen)) ); }, dist(gen)));
		Test(tc::iota(0, i));
		Test(tc::reverse(tc::iota(0, i)));
		Test(tc::repeat_n(i, 0));
		for( int j = 0; j < 7; ++j) {
			Test(tc::join(tc::repeat_n(i, tc::iota(0, j))));
		}
	}
}
#endif
}
