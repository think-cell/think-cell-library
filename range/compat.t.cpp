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
#include <array>

#pragma warning( push )
#pragma warning( disable: 4018 )
#include <boost/range/category.hpp>
#include <boost/range/iterator_range.hpp>
#pragma warning( pop )

namespace lookup {
	struct NoBegin final {};
	struct GlobalBegin final {};
	struct AdlBegin final {};

	int begin(AdlBegin&) noexcept { return 0; }
	int const begin(AdlBegin const&) noexcept { return 1; }
}	

long begin(lookup::GlobalBegin&) noexcept { return 2; }
long const begin(lookup::GlobalBegin const&) noexcept {return 3; }

namespace {
	using namespace tc;

	#pragma warning( push )
	#pragma warning( disable: 4101 ) // we do not need the variables, but they make the expressions cleaner

	void static_tests_adl_lookup() noexcept {
		tc::vector<int> has_std_begin;
		lookup::NoBegin has_no_begin;
		lookup::GlobalBegin has_global_begin;
		lookup::AdlBegin has_adl_begin;
		boost::iterator_range< std::array<unsigned long,1>::const_iterator > has_boost_begin;

		//STATIC_ASSERT(is_range_with_iterators<decltype(has_global_begin)>::value); Todo: find out how to make this work
		//STATIC_ASSERT(is_range_with_iterators<decltype(has_adl_begin)>::value);
		STATIC_ASSERT(is_range_with_iterators<decltype(has_std_begin)>::value);
		STATIC_ASSERT(is_range_with_iterators<decltype(has_boost_begin)>::value);
		STATIC_ASSERT(!is_range_with_iterators<decltype(has_no_begin)>::value);
	}

	#pragma warning( pop )
	
	struct TransFilterTest final {
		template <typename T>
		struct wrapped final {
			explicit wrapped(T const& t) noexcept : m_t(t) {}
			wrapped(wrapped<T> const& t) noexcept : m_t(t.m_t) {}
			T getT() const& noexcept { return m_t; }

			T m_t;
		};

		using wrapped_long = wrapped<long>;
		static bool filter35( wrapped_long const& wl ) noexcept { return wl.getT() == 3 || wl.getT() == 5; }
		static std::size_t transf_times_100( wrapped_long const& wl ) noexcept { std::size_t color = wl.getT() * 100; return color; }

		using WlList = tc::vector< wrapped_long > const&;
		//using WlList = tc::vector< wrapped_long >;        // works!
		WlList getWlList() const& noexcept { return m_list; } 
	
		using WlFilterdList = filter_adaptor<decltype(&filter35), WlList>;
		auto getWlFilterdList() const& noexcept return_decltype ( tc::filter(getWlList(), &filter35) )

		// This is were it gets wiered, as soon as you somehow use has_range_iterator<WlFilterdList> (here at class scope)
		// things go crashing down, even though has_range_iterator<WlFilterdList> is perfectly fine one line later at funtion scope

		using WlFilterdTransformedList = transform_adaptor<decltype(&transf_times_100), WlFilterdList, true>; STATIC_ASSERT(is_range_with_iterators<WlFilterdList>::value);
		//using WlFilterdTransformedList = transform_adaptor<decltype(&transf_times_100), WlFilterdList, is_range_with_iterators<WlFilterdList>::value>;
		//using WlFilterdTransformedList = transform_adaptor<decltype(&transf_times_100), WlFilterdList>;
		WlFilterdTransformedList getWlFilterdTransformedList() const& noexcept {
			STATIC_ASSERT(is_range_with_iterators<WlFilterdList>::value);
			STATIC_ASSERT(is_range_with_iterators<WlFilterdTransformedList>::value);
		
			return  tc::transform(getWlFilterdList(), &transf_times_100 );
		}
		//auto getWlFilterdTransformedList() const return_decltype ( tc::transform(SolidFillList(), &transf_times_100) )

		TransFilterTest() noexcept {
			TEST_init_hack(tc::vector, wrapped_long, list, {wrapped_long(1),wrapped_long(2),wrapped_long(3),wrapped_long(4),wrapped_long(5),wrapped_long(6)});
			m_list = list;
		}

		private:
			tc::vector<wrapped_long> m_list;
	};
	template<typename T> std::ostream& operator<<(std::ostream& os, TransFilterTest::wrapped<T> const& w) noexcept { os << w.m_t; return os; }


UNITTESTDEF( TransFilterTest ) {

	TransFilterTest t;
	auto const& res = t.getWlFilterdTransformedList();

	UNUSED_TEST_VARIABLE(res);
	//TEST_OUTPUT_RANGE(res);
	//TEST_init_hack(tc::vector, std::size_t, original, {std::size_t(300), std::size_t(500)});
	//TEST_RANGE_EQUAL(exp, res);
}

UNITTESTDEF( boost_iterator_range_compat ) {

	TEST_init_hack(tc::vector, unsigned long, original, {1,2,3,4,5,6,7,8});
	tc::vector<unsigned long> v = original;

	TEST_init_hack(tc::vector, unsigned long, baul_exp, {6});
	std::array<unsigned long, 1> baul; baul[0] = 6;

	auto mutable_range = boost::make_iterator_range(v); TEST_RANGE_LENGTH(mutable_range, 8);
	TEST_RANGE_EQUAL(original, mutable_range);

	boost::iterator_range< std::array<unsigned long,1>::const_iterator > const baul_r = boost::make_iterator_range(baul);

	//STATIC_ASSERT(is_range_with_iterators<decltype(baul)>::value);
	//STATIC_ASSERT(has_range_const_iterator<decltype(baul)>::value);
	//STATIC_ASSERT(std::is_same<range_iterator<decltype(baul)>::type, std::array<unsigned long,1>::iterator >::value );
	//STATIC_ASSERT(std::is_same<range_const_iterator<decltype(baul)>::type, std::array<unsigned long,1>::const_iterator >::value );
	
	TEST_RANGE_EQUAL(baul_exp, baul_r);

	auto baul_our_r = slice(baul);
	TEST_RANGE_EQUAL(baul_exp, baul_our_r);

}

UNITTESTDEF( boost_range_traits_compat ) {
	TEST_init_hack(tc::vector, unsigned long, original, {1,2,3,4,5,6,7,8});
	TEST_init_hack(tc::vector, unsigned long, exp, {2,4,6,8});

	auto fr = tc::filter(original, [](int i) { return i%2==0; });

	STATIC_ASSERT(std::is_same<decltype(std::begin(fr)), decltype(boost::begin(fr))>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_iterator<decltype(fr)>::type, typename boost::range_iterator<decltype(fr)>::type>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_size<decltype(fr)>::type, typename boost::range_size<decltype(fr)>::type>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_category<decltype(fr)>::type, typename boost::range_category<decltype(fr)>::type>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_category<decltype(original)>::type, std::random_access_iterator_tag>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_category<decltype(fr)>::type, std::bidirectional_iterator_tag>::value);
	
	auto bir = boost::make_iterator_range(fr);
	TEST_RANGE_EQUAL(exp, bir);
}

struct inner final {
	inner(int id) noexcept : i(id) {}
	int id() const& noexcept { return i*100; }
private:
	friend struct free_id;
	int i;
};

struct free_id final { int operator()(inner const& in) const& noexcept { return in.id(); } };
struct filter_stub final { template<typename T> bool operator()(T const&) const& noexcept { return true; } };

struct outer final {
	tc::vector<inner> m_in;

	outer() noexcept {
		TEST_init_hack(tc::vector, inner, tmp, {1,2,3,4,5,6,7,8});
		m_in = tmp;
	}

	using TRange = tc::filter_adaptor< filter_stub, tc::transform_adaptor< free_id, tc::vector<inner> const& , true>, true >;
	TRange trans_range() & noexcept {
		return tc::filter( tc::transform(tc::as_const(m_in), free_id()), filter_stub() );
	}
};

void static_tests() noexcept {

	//STATIC_ASSERT(std::is_same<typename tc::range_iterator<int>::type, error::no_iterator_detected>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_size<int>::type, std::size_t>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_category<int>::type, error::iterator_category_not_detected>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_reference<int>::type, error::reference_type_not_detected>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_difference<int>::type, error::difference_type_not_detected>::value);
}

UNITTESTDEF( deduce_traits ) {
	TEST_init_hack(tc::vector, int, exp, {100,200,300,400,500,600,700,800});
	outer o;

	auto trange = tc::filter(tc::transform(tc::as_const(o.m_in), free_id()), filter_stub());
	TEST_RANGE_EQUAL( exp, trange );

	tc::vector<int> res;
	tc::for_each( o.trans_range(), [&](int const& id) { res.emplace_back(id); } );
	TEST_RANGE_EQUAL(exp, res);
}

}
