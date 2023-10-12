
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "equal.h"

namespace tc {
	template< typename RangeReturn, typename RngLhs, typename RngRhs, typename Pred=tc::fn_equal_to_or_parse_match>
	[[nodiscard]] constexpr decltype(auto) longest_common_prefix(RngLhs&& rnglhs, RngRhs&& rngrhs, Pred pred=Pred()) MAYTHROW {
		static_assert(RangeReturn::allowed_if_always_has_border);

		tc_auto_cref(itlhsEnd, tc::end(rnglhs));
		tc_auto_cref(itrhsEnd, tc::end(rngrhs));
		auto itlhs=tc::begin(rnglhs);
		auto itrhs=tc::begin(rngrhs);
		while(itlhs < itlhsEnd && itrhs < itrhsEnd && tc::invoke(pred, *itlhs, *itrhs)) {
			++itlhs;
			++itrhs;
		}
		return std::make_pair(
			RangeReturn::pack_border(itlhs, tc_move_if_owned(rnglhs)),
			RangeReturn::pack_border(itrhs, tc_move_if_owned(rngrhs))
		);
	}

	template< typename RangeReturn, typename RngLhs, typename RngRhs, typename Pred=tc::fn_equal_to_or_parse_match>
	[[nodiscard]] constexpr decltype(auto) longest_common_prefix_lhs(RngLhs&& rnglhs, RngRhs&& rngrhs, Pred pred=Pred()) MAYTHROW {
		static_assert(RangeReturn::allowed_if_always_has_border);

		tc_auto_cref(itlhsEnd, tc::end(rnglhs));
		auto itlhs=tc::begin(rnglhs);
		tc::for_each(rngrhs, [&](auto const& rhs) noexcept {
			if (itlhs!=itlhsEnd && tc::invoke(pred,*itlhs, rhs)) {
				++itlhs;
				return tc::continue_;
			} else {
				return tc::break_;
			}
		});
		return RangeReturn::pack_border(itlhs, tc_move_if_owned(rnglhs));
	}
}
