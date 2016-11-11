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
#include "range.t.h"
#include "compare.h"

#include <boost/fusion/include/make_fused_function_object.hpp>
#include "merge_ranges.h"

UNITTESTDEF(merge_ranges_with_simple_usecase) {

	tc::vector<tc::vector<int>> vecvecn;
	vecvecn.emplace_back(tc::make_initializer_list({1,3,5}));
	vecvecn.emplace_back(tc::make_initializer_list({2,4}));

	int N=0;
	tc::for_each(
		tc::merge_many(vecvecn),
		[&](int const& n) noexcept {
			_ASSERTEQUAL(n, ++N);
		}
	);
	_ASSERT(N==5);
}


UNITTESTDEF(zip_range_adaptor_test) {
	{
		tc::vector<int> vecn{1,3,5};
		tc::vector<int> vecn2{2,6,10};

		auto rng = tc::zip(vecn,vecn2);

		_ASSERT(std::get<0>(*boost::begin(rng)) == 1);
		_ASSERTEQUAL(std::addressof(std::get<1>(*std::begin(rng))),std::addressof(vecn2[0]));

		auto it = boost::begin(vecn);
		auto it2 = boost::begin(vecn2);
		tc::for_each(
			rng,
			boost::fusion::make_fused_function_object(
				[&](int const& n, int const& n2) noexcept {
					_ASSERTEQUAL(*it, n);
					_ASSERTEQUAL(*it2, n2);
					_ASSERTEQUAL(std::addressof(*it), std::addressof(n));
					_ASSERTEQUAL(std::addressof(*it2), std::addressof(n2));
					++it; ++it2;
				}
			)
		);
		_ASSERT(boost::end(vecn) == it);
	}

	auto rng = tc::zip(
		tc::vector<int>{4,2,1},
		tc::make_counting_range(0,3)
	);

	int N=4;
	int N2=0;
	tc::for_each(
		rng,
		boost::fusion::make_fused_function_object(
			[&](int const& n, int const& n2) noexcept {
				VERIFYEQUAL(N, n) /= 2;
				++VERIFYEQUAL(N2, n2);
			}
		)
	);
	_ASSERTEQUAL(N2,3);
}

UNITTESTDEF(merge_ranges_with_unique_range_2) {
	tc::vector<tc::vector<int>> vecvecn;
	vecvecn.emplace_back(tc::make_initializer_list({1,1,3,3,3,5,5,5,5}));
	vecvecn.emplace_back(tc::make_initializer_list({2,2,4}));

	tc::vector<tc::vector<int>> vecvecn2;
	vecvecn2.emplace_back(tc::make_initializer_list({101,102,103,104,105,106,107,108,109}));
	vecvecn2.emplace_back(tc::make_initializer_list({110,111,112}));

	auto lesspred = tc::projected(tc::fn_less(), fn_std_get<0>());

	int n=0;
	int nTotal=0;
	tc::for_each(
		tc::merge_many(
			tc::transform(
				tc::make_counting_range(0,2),
				[&](int n) noexcept {
					return tc::ordered_unique_range(
						tc::zip(
							tc::make_range(vecvecn[n]),
							tc::make_range(vecvecn2[n])
						),
						lesspred
					);
				}
			),
			projected_front(lesspred)
		),
		[&](auto rng) noexcept {
			tc::for_each(
				rng,
				boost::fusion::make_fused_function_object(
					[&](int const& first, int& second) noexcept {
						second = n;
						_ASSERTEQUAL(first, n+1);
						++nTotal;
					}
				)
			);
			++n;
		}
	);
	_ASSERTEQUAL(n,5);
	_ASSERTEQUAL(nTotal, 12);
	_ASSERTEQUAL(vecvecn2[0][0],0);
	_ASSERTEQUAL(vecvecn2[0][1],0);
	_ASSERTEQUAL(vecvecn2[0][2],2);
	_ASSERTEQUAL(vecvecn2[1][0],1);
}
