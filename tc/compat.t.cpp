
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "base/assert_defs.h"
#include "unittest.h"
#include "container/container.h" // tc::vector
#include "container/insert.h"
#include "range/filter_adaptor.h"


#include <array>

MODIFY_WARNINGS_BEGIN(((disable)(4018)))
#include <boost/range/category.hpp>
#include <boost/range/iterator_range.hpp>
MODIFY_WARNINGS_END

namespace lookup {
	struct NoBegin final {};
}

namespace {
	using HasStdBegin = tc::vector<int>;
	using HasNoBegin = lookup::NoBegin;
	using HasBoostBegin = boost::iterator_range< tc::iterator_t<std::array<unsigned long,1> const> >;

	STATIC_ASSERT(tc::is_range_with_iterators<HasStdBegin>::value);
	STATIC_ASSERT(tc::is_range_with_iterators<HasBoostBegin>::value);
	STATIC_ASSERT(!tc::is_range_with_iterators<HasNoBegin>::value);

	struct TransFilterTest final {
		template <typename T>
		struct wrapped final {
			explicit wrapped(T const& t) noexcept : m_t(t) {}
			T getT() const& noexcept { return m_t; }

			T m_t;
		};

		using wrapped_long = wrapped<long>;
		static bool filter35( wrapped_long const& wl ) noexcept { return wl.getT() == 3 || wl.getT() == 5; }
		static std::size_t transf_times_100( wrapped_long const& wl ) noexcept { std::size_t color = wl.getT() * 100; return color; }

		using WlList = tc::vector< wrapped_long > const&;
		//using WlList = tc::vector< wrapped_long >;        // works!
		WlList getWlList() const& noexcept { return m_list; }

		using WlFilterdList = tc::filter_adaptor<decltype(&filter35), WlList>;
		auto getWlFilterdList() const& return_decltype_noexcept( tc::filter(getWlList(), &filter35) )

		// This is where it gets weird, as soon as you somehow use has_range_iterator<WlFilterdList> (here at class scope)
		// things go crashing down, even though has_range_iterator<WlFilterdList> is perfectly fine one line later at function scope

		using WlFilterdTransformedList = tc::transform_adaptor<decltype(&transf_times_100), WlFilterdList, true>; STATIC_ASSERT(tc::is_range_with_iterators<WlFilterdList>::value);
		//using WlFilterdTransformedList = transform_adaptor<decltype(&transf_times_100), WlFilterdList, tc::is_range_with_iterators<WlFilterdList>::value>;
		//using WlFilterdTransformedList = transform_adaptor<decltype(&transf_times_100), WlFilterdList>;
		WlFilterdTransformedList getWlFilterdTransformedList() const& noexcept {
			STATIC_ASSERT(tc::is_range_with_iterators<WlFilterdList>::value);
			STATIC_ASSERT(tc::is_range_with_iterators<WlFilterdTransformedList>::value);

			return  tc::transform(getWlFilterdList(), &transf_times_100 );
		}

		TransFilterTest() noexcept {
			TEST_init_hack(tc::vector, wrapped_long, list, {wrapped_long(1),wrapped_long(2),wrapped_long(3),wrapped_long(4),wrapped_long(5),wrapped_long(6)});
			m_list = list;
		}

		private:
			tc::vector<wrapped_long> m_list;
	};


UNITTESTDEF( TransFilterTest ) {

	TransFilterTest t;
	auto_cref( res, t.getWlFilterdTransformedList() );

	UNUSED_TEST_VARIABLE(res);
	TEST_init_hack(tc::vector, std::size_t, original, {std::size_t(300), std::size_t(500)});
}

UNITTESTDEF( boost_iterator_range_compat ) {

	TEST_init_hack(tc::vector, unsigned long, original, {1,2,3,4,5,6,7,8});
	tc::vector<unsigned long> v = original;

	TEST_init_hack(tc::vector, unsigned long, baul_exp, {6});
	std::array<unsigned long, 1> baul; tc::front(baul) = 6;

	auto mutable_range = boost::make_iterator_range(v); TEST_RANGE_LENGTH(mutable_range, 8);
	TEST_RANGE_EQUAL(original, mutable_range);

	boost::iterator_range< tc::iterator_t<std::array<unsigned long,1> const> > const baul_r = boost::make_iterator_range(baul);

	STATIC_ASSERT(tc::is_range_with_iterators<decltype(baul)>::value);

	TEST_RANGE_EQUAL(baul_exp, baul_r);

	auto baul_our_r = tc::slice(baul);
	TEST_RANGE_EQUAL(baul_exp, baul_our_r);

}

UNITTESTDEF( boost_range_traits_compat ) {
	TEST_init_hack(tc::vector, unsigned long, original, {1,2,3,4,5,6,7,8});
	TEST_init_hack(tc::vector, unsigned long, exp, {2,4,6,8});

	auto fr = tc::filter(original, [](unsigned long i) noexcept { return i%2==0; });

	STATIC_ASSERT(std::is_same<decltype(std::begin(fr)), decltype(tc::begin(fr))>::value);

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

UNITTESTDEF( deduce_traits ) {
	TEST_init_hack(tc::vector, int, exp, {100,200,300,400,500,600,700,800});
	outer o;

	auto trange = tc::filter(tc::transform(tc::as_const(o.m_in), free_id()), filter_stub());
	TEST_RANGE_EQUAL( exp, trange );

	tc::vector<int> res;
	tc::for_each( o.trans_range(), [&](int const id) noexcept { tc::cont_emplace_back(res, id); } );
	TEST_RANGE_EQUAL(exp, res);
}

}
