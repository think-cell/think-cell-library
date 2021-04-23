
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "interval.h"

#include "range.t.h"
#if TC_PRIVATE
#include "Library/HeaderOnly/chrono.h"
#endif
#include "round.h"

STATICASSERTEQUAL( sizeof(tc::interval<int>), sizeof(int) * 2 );

UNITTESTDEF(interval_center) {

	{
		auto TestCenterContained=[](auto low, auto high, auto center) noexcept {
			auto const intvl = tc::make_interval(std::forward<decltype(low)>(low), std::forward<decltype(high)>(high));
			_ASSERTEQUAL( intvl.midpoint(), center );
			_ASSERT( intvl.empty() || intvl.contains(intvl.midpoint()) ); // The empty interval contains nothing, not even its center
		};
		
		TestCenterContained(0, 1, 0);
		TestCenterContained(-1, 0, -1);
		TestCenterContained(0, 0, 0);

		TestCenterContained(0.0, 1.0, 0.5);
	}
	{
		auto TestCenterContained=[](auto pos, auto extent) noexcept {
			auto intvl = tc::make_centered_interval(pos, tc_move(extent));
			_ASSERTEQUAL( intvl.midpoint(), pos );
			_ASSERT( intvl.contains(pos) );
			return intvl;
		};
		
		_ASSERTEQUAL( TestCenterContained(2,5), tc::make_interval(0,5) );
		_ASSERTEQUAL( TestCenterContained(-3, 5), tc::make_interval(-5, 0));

		// TestCenterContained(2.5, 5); // Doesn't compile: ambiguous interpretation
		// TestCenterContained(0, 5.0); // Doesn't compile: ambiguous interpretation
	}

#if TC_PRIVATE
	tc::chrono::sys_days const tp = tc::chrono::year_month_day(2000,1,1).time_point();

	auto AddDays=[](tc::chrono::sys_days const& tpBase, int nDays) noexcept {
		return tpBase + tc::chrono::days(nDays);
	};
#endif
	//expand_length roundtrips
	{
		auto TestRoundtrip=[](auto intvl, auto extent, auto intvlExpanded) noexcept {
			_ASSERTEQUAL(
				modified(intvl, {
					_.expand_length(extent);
					VERIFYEQUAL(_, intvlExpanded).expand_length(-extent);
				}),
				intvl
			);
		};

		//int
		TestRoundtrip(tc::make_interval(-1, 1), 8, tc::make_interval(-5, 5));
		TestRoundtrip(tc::make_interval(-1, 1), 0, tc::make_interval(-1, 1));
		TestRoundtrip(tc::make_interval(0, 1), 1, tc::make_interval(-1, 1));
		TestRoundtrip(tc::make_interval(-1, 0), 1, tc::make_interval(-2, 0));
		TestRoundtrip(tc::make_interval(-1, 1), -4, tc::make_interval(1, -1));

		//double
		TestRoundtrip(tc::make_interval(-1.0, 1.0), 8.0, tc::make_interval(-5.0, 5.0));
		TestRoundtrip(tc::make_interval(-1.0, 1.0), 0.0, tc::make_interval(-1.0, 1.0));
		TestRoundtrip(tc::make_interval(0.0, 1.0), 1.0, tc::make_interval(-0.5, 1.5));
		TestRoundtrip(tc::make_interval(-1.0, 0.0), 1.0, tc::make_interval(-1.5, 0.5));
		TestRoundtrip(tc::make_interval(-1.0, 1.0), -4.0, tc::make_interval(1.0, -1.0));

#if TC_PRIVATE
		//time_point
		TestRoundtrip(tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)), tc::chrono::days(8), tc::make_interval(AddDays(tp, -5), AddDays(tp, 5)));
		TestRoundtrip(tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)), tc::chrono::days(0), tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)));
		TestRoundtrip(tc::make_interval(tp, AddDays(tp, 1)), tc::chrono::days(1), tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)));
		TestRoundtrip(tc::make_interval(AddDays(tp, -1), tp), tc::chrono::days(1), tc::make_interval(AddDays(tp, -2), tp));
		TestRoundtrip(tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)), tc::chrono::days(-4), tc::make_interval(AddDays(tp, 1), AddDays(tp, -1)));
#endif
	}
	//OffsetToFit
	{
		auto TestOffsetToFit=[](auto intvlToFit, auto intvlFitTarget) noexcept {
			auto const offset = intvlToFit.OffsetToFit(intvlFitTarget);
			if (intvlToFit.length() <= intvlFitTarget.length()) {
				_ASSERT( intvlFitTarget.contains(intvlToFit + offset) );
			} else {
				intvlFitTarget.expand_to(intvlToFit.length());
				_ASSERTEQUAL(intvlFitTarget, intvlToFit + offset);
			}
			_ASSERT(
				(intvlToFit + offset)[tc::lo]==intvlFitTarget[tc::lo]
				|| (intvlToFit + offset)[tc::hi]==intvlFitTarget[tc::hi]
			);
		};

		//int
		TestOffsetToFit(tc::make_interval(-1, 1), tc::make_interval(0, 10));
		TestOffsetToFit(tc::make_interval(-1, 1), tc::make_interval(-10, 0));
		TestOffsetToFit(tc::make_interval(-1, 1), tc::make_interval(-1, 1));
		TestOffsetToFit(tc::make_interval(-1, 1), tc::make_interval(0, 0));

		//double
		TestOffsetToFit(tc::make_interval(-1.0, 1.0), tc::make_interval(0.0, 10.0));
		TestOffsetToFit(tc::make_interval(-1.0, 1.0), tc::make_interval(-10.0, 0.0));
		TestOffsetToFit(tc::make_interval(-1.0, 1.0), tc::make_interval(-1.0, 1.0));
		TestOffsetToFit(tc::make_interval(-1.0, 1.0), tc::make_interval(0.0, 0.0));

#if TC_PRIVATE
		//time_point
		TestOffsetToFit(tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)), tc::make_interval(tp, AddDays(tp, 10)));
		TestOffsetToFit(tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)), tc::make_interval(AddDays(tp, -10), tp));
		TestOffsetToFit(tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)), tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)));
		TestOffsetToFit(tc::make_interval(AddDays(tp, -1), AddDays(tp, 1)), tc::make_interval(tp, tp));
#endif
	}
}


namespace {
	template<typename T>
	void AssertNextAfter() noexcept {
		// checking suitability of tc::nextafter_inplace for generating right exclusive intervals
		T const fInfinity = std::numeric_limits<T>::infinity();
		T const fZero = 0;
		T const fOne = 1;
		_ASSERT(0 < modified(fZero, tc::nextafter_inplace(_)));
		_ASSERTEQUAL(modified(fZero, tc::nextafter_inplace(_)) / 2, 0);
		_ASSERTEQUAL(modified(fOne, tc::nextafter_inplace(_)) - fOne, std::numeric_limits<T>::epsilon());
		_ASSERTEQUAL(modified(std::numeric_limits<T>::max(), tc::nextafter_inplace(_)), fInfinity);
		_ASSERTEQUAL(modified(fInfinity, tc::nextafter_inplace(_)), fInfinity);
	}
}

UNITTESTDEF(nextafter) {
	AssertNextAfter<float>();
	AssertNextAfter<double>();
}


UNITTESTDEF(linear_invert) {
	tc::linear_interval_transform<int> intvltransformf(tc::make_interval(2, 5), tc::make_interval(1, 5));
	auto const intvltransformfInverse = intvltransformf.inverted();
	_ASSERTEQUAL(intvltransformfInverse(intvltransformf(0)), 0);
	_ASSERTEQUAL(intvltransformfInverse(intvltransformf(1)), 1);
}

UNITTESTDEF(minmax_interval) {
	_ASSERTEQUAL(tc::minmax_interval(tc::vector<int>{1,2,3}), tc::make_interval(1,3));
}