
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "../base/assert_defs.h"
#include "../unittest.h"
#include "../algorithm/compare.h"

#include "merge_ranges.h"
#include "zip_range.h"

UNITTESTDEF(merge_ranges_with_simple_usecase) {

	tc::vector<tc::vector<int>> vecvecn;
	tc::cont_emplace_back(vecvecn, tc::make_array(tc::aggregate_tag,1,3,5));
	tc::cont_emplace_back(vecvecn, tc::make_array(tc::aggregate_tag,2,4));

	int N=0;
	tc::for_each(
		tc::merge_many(vecvecn),
		[&](int const n) noexcept {
			_ASSERTEQUAL(n, ++N);
		}
	);
	_ASSERTEQUAL(5, N);
}


UNITTESTDEF(zip_range_adaptor_test) {
	{
		tc::vector<int> vecn{1,3,5};
		tc::vector<int> vecn2{2,6,10};

		auto rng = tc::zip(vecn,vecn2);

		_ASSERTEQUAL(tc::get<0>(*tc::begin(rng)), 1);
		_ASSERTEQUAL(std::addressof(tc::get<1>(*tc::begin(rng))),std::addressof(vecn2[0]));

		auto it = tc::begin(vecn);
		auto it2 = tc::begin(vecn2);
		tc::for_each(
			rng,
			[&](int const& n, int const& n2) noexcept {
				_ASSERTEQUAL(*it, n);
				_ASSERTEQUAL(*it2, n2);
				_ASSERTEQUAL(std::addressof(*it), std::addressof(n));
				_ASSERTEQUAL(std::addressof(*it2), std::addressof(n2));
				++it; ++it2;
			}
		);
		_ASSERTEQUAL(tc::end(vecn), it);
	}

	auto rng = tc::zip(
		tc::vector<int>{4,2,1},
		tc::iota(0,3)
	);

	int N=4;
	int N2=0;
	tc::for_each(
		rng,
		[&](int const n, int const n2) noexcept {
			VERIFYEQUAL(N, n) /= 2;
			++VERIFYEQUAL(N2, n2);
		}
	);
	_ASSERTEQUAL(N2,3);
}

UNITTESTDEF(merge_ranges_with_unique_range_2) {
	tc::vector<tc::vector<int>> vecvecn;
	tc::cont_emplace_back(vecvecn, tc::make_array(tc::aggregate_tag,10));
	tc::cont_emplace_back(vecvecn, tc::make_array(tc::aggregate_tag,1,1,3,3,3,5,5,5,5,11));
	tc::cont_emplace_back(vecvecn, tc::make_array(tc::aggregate_tag,6,9));
	tc::cont_emplace_back(vecvecn, tc::make_array(tc::aggregate_tag,2,2,4,12));
	tc::cont_emplace_back(vecvecn, tc::make_array(tc::aggregate_tag,7,8));

	tc::vector<tc::vector<int>> vecvecn2;
	tc::cont_emplace_back(vecvecn2, tc::make_array(tc::aggregate_tag,100));
	tc::cont_emplace_back(vecvecn2, tc::make_array(tc::aggregate_tag,101,102,103,104,105,106,107,108,109,110));
	tc::cont_emplace_back(vecvecn2, tc::make_array(tc::aggregate_tag,111,112));
	tc::cont_emplace_back(vecvecn2, tc::make_array(tc::aggregate_tag,113,114,115,116));
	tc::cont_emplace_back(vecvecn2, tc::make_array(tc::aggregate_tag,117,118));

	auto lesspred = tc::projected(tc::fn_less(), tc_fn(tc::get<0>));

	int n=0;
	int nTotal=0;
	int nCompares = 0;
	tc::for_each(
		tc::merge_many(
			tc::transform(
				tc::iota(0,5),
				[&](int const n) noexcept {
					return tc::ordered_unique_range(
						tc::zip(
							tc::all(vecvecn[n]),
							tc::all(vecvecn2[n])
						),
						lesspred
					);
				}
			),
			[&](auto const& lhs, auto const& rhs) {
				++nCompares;
				return tc::projected(lesspred, tc::fn_front())(lhs, rhs);
			}
		),
		[&](auto rng) noexcept {
			tc::for_each(
				rng,
				[&](int const first, int& second) noexcept {
					second = n;
					_ASSERTEQUAL(first, n+1);
					++nTotal;
				}
			);
			++n;
		}
	);
	_ASSERTEQUAL(n,12);
	_ASSERTEQUAL(nTotal, 19);
	_ASSERTEQUAL(vecvecn2[0][0],9);
	_ASSERTEQUAL(vecvecn2[1][0],0);
	_ASSERTEQUAL(vecvecn2[1][1],0);
	_ASSERTEQUAL(vecvecn2[1][2],2);
	_ASSERTEQUAL(vecvecn2[1][5],4);
	_ASSERTEQUAL(vecvecn2[1][9],10);
	_ASSERTEQUAL(vecvecn2[2][0],5);
	_ASSERTEQUAL(vecvecn2[2][1],8);
	_ASSERTEQUAL(vecvecn2[3][0],1);
	_ASSERTEQUAL(vecvecn2[3][1],1);
	_ASSERTEQUAL(vecvecn2[3][2],3);
	_ASSERTEQUAL(vecvecn2[3][3],11);
	_ASSERTEQUAL(vecvecn2[4][0],6);
	_ASSERTEQUAL(vecvecn2[4][1],7);
}
