
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../interval.h" // for tc::lohi
#include "zip_range.h"

namespace
{
	template<typename Rng, typename Traversal>
	struct traversal_override_adaptor
		: tc::index_range_adaptor<traversal_override_adaptor<Rng, Traversal>, Rng&, tc::index_range_adaptor_flags::inherit_all>
	{
		using this_type = traversal_override_adaptor;
		using base_ = typename traversal_override_adaptor::index_range_adaptor;
		using tc_index = typename base_::tc_index;

		traversal_override_adaptor(Rng& rng) : base_(tc::aggregate_tag, rng) {}

		STATIC_FINAL(decrement_index)(tc_index& idx) const& MAYTHROW
			requires tc::has_decrement_index<Rng>
				&& boost::iterators::detail::is_traversal_at_least<Traversal, boost::iterators::bidirectional_traversal_tag>::value
		{
			tc::decrement_index(this->base_range(), idx);
		}

		STATIC_FINAL_MOD(constexpr, advance_index)(tc_index& idx, typename boost::range_difference<Rng>::type d) const& MAYTHROW -> void
			requires tc::has_advance_index<Rng>
				&& boost::iterators::detail::is_traversal_at_least<Traversal, boost::iterators::random_access_traversal_tag>::value
		{
			tc::advance_index(this->base_range(),idx,d);
		}

		STATIC_FINAL_MOD(constexpr, distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept
			requires tc::has_distance_to_index<Rng>
				&& boost::iterators::detail::is_traversal_at_least<Traversal, boost::iterators::random_access_traversal_tag>::value
		{
			return tc::distance_to_index(this->base_range(),idxLhs,idxRhs);
		}
	};
}

UNITTESTDEF(zip_range_supported_ops) {
	std::vector<int> data = { 1, 2, 3, 4, 5 };
	std::vector<float> dataf = { 1, 2, 3, 4, 5 };
	auto data_fwd = traversal_override_adaptor<tc::vector<int>, boost::iterators::forward_traversal_tag>(data);
	auto data_bidir = traversal_override_adaptor<tc::vector<int>, boost::iterators::bidirectional_traversal_tag>(data);
	auto data_gen = [&](auto sink) noexcept { return tc::for_each(data, sink); };

	auto zip_ff = tc::zip(data_fwd, data_fwd);
	auto zip_fb = tc::zip(data_fwd, data_bidir);
	auto zip_fr = tc::zip(data_fwd, data);
	auto zip_bb = tc::zip(data_bidir, data_bidir);
	auto zip_br = tc::zip(data_bidir, data);
	auto zip_rr = tc::zip(data, data);
	auto zip_rfgf = tc::zip(dataf, data_fwd, data_gen, data_fwd);

	tc::for_each(zip_ff, tc::noop());
	tc::for_each(zip_fb, tc::noop());
	tc::for_each(zip_fr, tc::noop());
	tc::for_each(zip_bb, tc::noop());
	tc::for_each(zip_br, tc::noop());
	tc::for_each(zip_rr, tc::noop());
	tc::for_each(zip_rfgf, [](float&,int&,int&,int&) noexcept{});

	static_assert(!tc::has_decrement_index<decltype(zip_ff)>);
	static_assert(!tc::has_decrement_index<decltype(zip_fb)>);
	static_assert(!tc::has_decrement_index<decltype(zip_fr)>);
	static_assert(!tc::has_advance_index<decltype(zip_ff)>);
	static_assert(!tc::has_advance_index<decltype(zip_fb)>);
	static_assert(!tc::has_advance_index<decltype(zip_fr)>);
	static_assert(!tc::has_distance_to_index<decltype(zip_ff)>);
	static_assert(!tc::has_distance_to_index<decltype(zip_fb)>);
	static_assert(!tc::has_distance_to_index<decltype(zip_fr)>);

	static_assert(tc::has_decrement_index<decltype(zip_bb)>);
	static_assert(tc::has_decrement_index<decltype(zip_br)>);
	static_assert(!tc::has_advance_index<decltype(zip_bb)>);
	static_assert(!tc::has_advance_index<decltype(zip_br)>);
	static_assert(!tc::has_distance_to_index<decltype(zip_bb)>);
	static_assert(!tc::has_distance_to_index<decltype(zip_br)>);

	static_assert(tc::has_decrement_index<decltype(zip_rr)>);
	static_assert(tc::has_advance_index<decltype(zip_rr)>);
	static_assert(tc::has_distance_to_index<decltype(zip_rr)>);

	_ASSERTEQUAL(tc::size(zip_fr), 5);
	_ASSERTEQUAL(tc::size(zip_br), 5);
	_ASSERTEQUAL(tc::size(zip_rr), 5);
	_ASSERTEQUAL(tc::size(zip_rfgf), 5);

	static_assert(!tc::range_with_iterators<decltype(zip_rfgf)>);
}

UNITTESTDEF(zip_range_difference_type) {
	auto rngBigDiff = tc::iota(0, 2);
	auto rngSmallDiff = tc::all_values<tc::lohi>();

	using BigDiffType = boost::range_difference<decltype(rngBigDiff)>::type;
	using SmallDiffType = boost::range_difference<decltype(rngSmallDiff)>::type;
	// e.g. BigDiffType=int, SmallDiffType=signed char

	// If these initial assertions fail, the test needs to be adjusted. Find two ranges with different difference_types
	static_assert(!std::is_same<BigDiffType, SmallDiffType>::value);
	static_assert(sizeof(BigDiffType) > sizeof(SmallDiffType));

	static_assert(std::is_same<boost::range_difference<decltype(tc::zip(rngBigDiff, rngBigDiff))>::type, BigDiffType>::value);
	static_assert(std::is_same<boost::range_difference<decltype(tc::zip(rngBigDiff, rngSmallDiff))>::type, SmallDiffType>::value);
	static_assert(std::is_same<boost::range_difference<decltype(tc::zip(rngSmallDiff, rngSmallDiff))>::type, SmallDiffType>::value);
}

STATICASSERTSAME( (tc::range_value_t<decltype(tc::zip(std::declval<tc::vector<int>>(), std::declval<std::array<double,4>>()))>), (tc::tuple<int, double>) );

UNITTESTDEF(zip_range_sort_inplace) {
	tc::vector<int> veci = {47,11,23};
	tc::vector<double> vecf = {1,2,3};
	tc::sort_inplace(tc::zip(veci, vecf));
	_ASSERTEQUAL( tc::at(veci, 0), 11 );
	_ASSERTEQUAL( tc::at(veci, 1), 23 );
	_ASSERTEQUAL( tc::at(veci, 2), 47 );
	_ASSERTEQUAL( tc::at(vecf, 0), 2 );
	_ASSERTEQUAL( tc::at(vecf, 1), 3 );
	_ASSERTEQUAL( tc::at(vecf, 2), 1 );
}

UNITTESTDEF(zip_range_range_output_t) {
	tc::vector<int> const vecn ={1, 2, 3};
	tc::vector<double> vecf ={1.2, 2.2, 3.2};
	auto const rngnf = tc::zip(vecn, vecf);
	STATICASSERTSAME(tc::range_output_t<decltype(rngnf)>, (tc::type::list<tc::tuple<int const&, double&>>));
	STATICASSERTSAME(tc::range_value_t<decltype(rngnf)>, (tc::tuple<int, double>));

	auto const rngch = tc::generator_range_output<char const&>([](auto sink) noexcept {
		tc::for_each("abc", sink);
	});
	STATICASSERTSAME(tc::range_output_t<decltype(rngch)>, (tc::type::list<char const&>));
	auto const rngnchf = tc::zip(vecn, rngch, vecf);

	STATICASSERTSAME(tc::range_output_t<decltype(rngnchf)>, (tc::type::list<tc::tuple<int const&, char const&, double&>>));
	STATICASSERTSAME(tc::range_value_t<decltype(rngnchf)>, (tc::tuple<int, char, double>));
}
