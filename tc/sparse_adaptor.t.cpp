
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "sparse_adaptor.h"
#include "range.t.h"

UNITTESTDEF(sparse_range) {
	bool const abVals[] = {true, false, false, true, false, false};
	auto const rngb = tc::sparse_range(tc::make_array(tc::aggregate_tag, std::make_pair(0u, true), std::make_pair(3u, true)), 6, false);
	_ASSERT(tc::equal(rngb, abVals));
	int iVal = 0;
	tc::for_each(rngb, [&](bool b) noexcept { _ASSERTEQUAL(b, abVals[iVal++]); });
}