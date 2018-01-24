//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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
#include "range.t.h"
#include "size.h"

namespace {
	void static_tests() noexcept {
		auto rngSize = tc::transform(tc::vector<int>(), [](int) noexcept { return 0; });
		static_assert(tc::size_impl::has_size<decltype(rngSize)>::value);

		auto rngNoSize = tc::transform(tc::filter(tc::vector<int>(), [](int) noexcept { return false; }), [](int) noexcept { return 0; });
		static_assert(!tc::size_impl::has_size<decltype(rngNoSize)>::value);

		int anNative[] = {1,2,3,4,5};
		auto anTc = tc::make_array(anNative);
		auto anTc2 = tc::make_array(anTc);
		auto anNativeTrans = tc::make_array(tc::transform(anNative, tc::identity()));
		auto anTcTrans = tc::make_array(tc::transform(anTc, tc::identity()));

		static_assert(std::is_same<decltype(constexpr_size(anTc)), decltype(tc::constexpr_size(anNative))>::value);
		static_assert(std::is_same<decltype(constexpr_size(anTc2)), decltype(tc::constexpr_size(anNative))>::value);
		static_assert(std::is_same<decltype(constexpr_size(anNativeTrans)), decltype(tc::constexpr_size(anNative))>::value);
		static_assert(std::is_same<decltype(constexpr_size(anTcTrans)), decltype(tc::constexpr_size(anNative))>::value);
	}
}

UNITTESTDEF(vector_int_ref_need_sfinae_transform) {
	tc::vector<int> vecn{1,2,3};
	auto rgntrnsfn = tc::transform(vecn, [](int& n) noexcept {return n*n;});
	auto it = boost::begin(rgntrnsfn);
	_ASSERTEQUAL(*it++,1);
	_ASSERTEQUAL(*it++,4);
	_ASSERTEQUAL(*it++,9);
}