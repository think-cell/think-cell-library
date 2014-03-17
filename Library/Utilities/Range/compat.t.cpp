#include "../Range.h"
#include "range.t.h"
#include <array>
#include <vector>
#include <ostream>

#include <array>
#include <vector>
#include <ostream>

#pragma warning( push )
#pragma warning( disable: 4018 )
#include <boost/range/category.hpp>
#include <boost/range/iterator_range.hpp>
#pragma warning( pop )

namespace lookup {
	struct NoBegin {};
	struct GlobalBegin {};
	struct AdlBegin {};

	int begin(AdlBegin&) { return 0; }
	int const begin(AdlBegin const&) { return 1; }
}	

long begin(lookup::GlobalBegin&) { return 2; };
long const begin(lookup::GlobalBegin const&) {return 3; };

namespace {
	using namespace RANGE_PROPOSAL_NAMESPACE;

	#pragma warning( push )
	#pragma warning( disable: 4101 ) // we do not need the variables, but they make the expressions cleaner

	void static_tests_adl_lookup() {
		std::vector<int> has_std_begin;
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
	
	struct TransFilterTest {
		template <typename T>
		struct wrapped {
			explicit wrapped(T const& t) : m_t(t) {}
			wrapped(wrapped<T> const& t) : m_t(t.m_t) {}
			T getT() const { return m_t; }

			T m_t;
		};

		typedef wrapped<long> wrapped_long;
		static bool filter35( wrapped_long const& wl ) { return wl.getT() == 3 || wl.getT() == 5; }
		static std::size_t transf_times_100( wrapped_long const& wl ) { std::size_t color = wl.getT() * 100; return color; }

		typedef std::vector< wrapped_long > const& WlList;
		//typedef std::vector< wrapped_long > WlList;        // works!
		WlList getWlList() const { return m_list; } 
	
		typedef filter_adaptor<decltype(&filter35), WlList> WlFilterdList;
		auto getWlFilterdList() const return_decltype ( tc::filter(getWlList(), &filter35) )

		// This is were it gets wiered, as soon as you somehow use has_range_iterator<WlFilterdList> (here at class scope)
		// things go crashing down, even though has_range_iterator<WlFilterdList> is perfectly fine one line later at funtion scope

		typedef transform_adaptor<decltype(&transf_times_100), WlFilterdList, true> WlFilterdTransformedList; STATIC_ASSERT(is_range_with_iterators<WlFilterdList>::value);
		//typedef  transform_adaptor<decltype(&transf_times_100), WlFilterdList, is_range_with_iterators<WlFilterdList>::value> WlFilterdTransformedList;
		//typedef  transform_adaptor<decltype(&transf_times_100), WlFilterdList> WlFilterdTransformedList;
		WlFilterdTransformedList getWlFilterdTransformedList() const {
			STATIC_ASSERT(is_range_with_iterators<WlFilterdList>::value);
			STATIC_ASSERT(is_range_with_iterators<WlFilterdTransformedList>::value);
		
			return  tc::transform(getWlFilterdList(), &transf_times_100 );
		}
		//auto getWlFilterdTransformedList() const return_decltype ( tc::transform(SolidFillList(), &transf_times_100) )

		TransFilterTest() {
			TEST_init_hack(std::vector, wrapped_long, list, {wrapped_long(1),wrapped_long(2),wrapped_long(3),wrapped_long(4),wrapped_long(5),wrapped_long(6)});
			m_list = list;
		}

		private:
			std::vector<wrapped_long> m_list;
	};
	template<typename T> std::ostream& operator<<(std::ostream& os, TransFilterTest::wrapped<T> const& w) { os << w.m_t; return os; }


UNITTESTDEF( TransFilterTest ) {

	TransFilterTest t;
	auto const& res = t.getWlFilterdTransformedList();

	UNUSED_TEST_VARIABLE(res);
	//TEST_OUTPUT_RANGE(res);
	//TEST_init_hack(std::vector, std::size_t, original, {std::size_t(300), std::size_t(500)});
	//TEST_RANGE_EQUAL(exp, res);
}

UNITTESTDEF( boost_iterator_range_compat ) {

	TEST_init_hack(std::vector, unsigned long, original, {1,2,3,4,5,6,7,8});
	std::vector<unsigned long> v = original;

	TEST_init_hack(std::vector, unsigned long, baul_exp, {6});
	std::array<unsigned long, 1> baul; baul[0] = 6;

	auto mutable_range = boost::make_iterator_range(v); TEST_RANGE_LENGTH(mutable_range, 8);
	TEST_RANGE_EQUAL(original, mutable_range);

	boost::iterator_range< std::array<unsigned long,1>::const_iterator > const baul_r = boost::make_iterator_range(baul);

	//STATIC_ASSERT(is_range_with_iterators<decltype(baul)>::value);
	//STATIC_ASSERT(has_range_const_iterator<decltype(baul)>::value);
	//STATIC_ASSERT(std::is_same<range_iterator<decltype(baul)>::type, std::array<unsigned long,1>::iterator >::value );
	//STATIC_ASSERT(std::is_same<range_const_iterator<decltype(baul)>::type, std::array<unsigned long,1>::const_iterator >::value );
	
	TEST_RANGE_EQUAL(baul_exp, baul_r);

	auto baul_our_r = tc::make_iterator_range(baul);
	TEST_RANGE_EQUAL(baul_exp, baul_our_r);

}

UNITTESTDEF( boost_range_traits_compat ) {
	TEST_init_hack(std::vector, unsigned long, original, {1,2,3,4,5,6,7,8});
	TEST_init_hack(std::vector, unsigned long, exp, {2,4,6,8});

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

struct inner {
	inner(int id) : i(id) {}
	int id() const { return i*100; }
private:
	friend struct free_id;
	int i;
};

struct free_id { int operator()(inner const& in) const { return in.id(); } };
struct filter_stub { template<typename T> bool operator()(T const&) const { return true; } };

struct outer {
	std::vector<inner> m_in;

	outer() {
		TEST_init_hack(std::vector, inner, tmp, {1,2,3,4,5,6,7,8});
		m_in = tmp;
	}

	typedef tc::filter_adaptor< filter_stub, tc::transform_adaptor< free_id, std::vector<inner> const &, true>, true > TRange;
	TRange trans_range() {
		return tc::filter( tc::transform(make_const(m_in), free_id()), filter_stub() );
	}
};

void static_tests() {

	//STATIC_ASSERT(std::is_same<typename tc::range_iterator<int>::type, error::no_iterator_detected>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_size<int>::type, std::size_t>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_category<int>::type, error::iterator_category_not_detected>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_reference<int>::type, error::reference_type_not_detected>::value);
	//STATIC_ASSERT(std::is_same<typename tc::range_difference<int>::type, error::difference_type_not_detected>::value);
}

UNITTESTDEF( deduce_traits ) {
	TEST_init_hack(std::vector, int, exp, {100,200,300,400,500,600,700,800});
	outer o;

	auto trange = tc::filter(tc::transform(make_const(o.m_in), free_id()), filter_stub());
	TEST_RANGE_EQUAL( exp, trange );

	std::vector<int> res;
	tc::for_each( o.trans_range(), [&](int const& id) { res.emplace_back(id); } );
	TEST_RANGE_EQUAL(exp, res);
}

}
