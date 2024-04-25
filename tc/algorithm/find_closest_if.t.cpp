
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "find_closest_if.h"
#include "../unittest.h"

UNITTESTDEF(find_closest_if) {
	struct IntCompareOnce final : boost::noncopyable {
		IntCompareOnce(int n) noexcept :m_n(n) { }
		bool operator==(int const n) const& noexcept {
			_ASSERT( tc::change(m_bCompared, true) );
			return m_n==n;
		}
	private:
		int m_n;
		bool mutable m_bCompared = false;
	};

	tc_static_auto_constexpr_lambda(find) = [](auto const& rngn, int const iStart, int const nTarget, int const nComparisonsMax) noexcept {
		int nComparisons = 0;
		return tc::find_closest_if_with_index<tc::return_element_index_or_none>(rngn, iStart, /*bSkipSelf*/false, [&](IntCompareOnce const& n) noexcept {
			_ASSERT(++nComparisons<=nComparisonsMax);
			return n==nTarget;
		});
	};

	for (int iStart = 0; iStart < 4; ++iStart) {
		for (int nTarget = -1; nTarget < 4; ++nTarget) {
			int nComparisonsMax = -1==nTarget ? 5 : nTarget<iStart ? 2*(iStart-nTarget) : 1+2*(nTarget-iStart);
			_ASSERTEQUAL(find(tc::make_array<IntCompareOnce>(tc::aggregate_tag, 0, 1, 2, 3, 4), iStart, nTarget, nComparisonsMax), -1==nTarget ? std::nullopt : std::make_optional(nTarget));
		}
	}
}
