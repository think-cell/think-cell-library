
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "element.h"

namespace tc {
	template< typename Rng, typename It, typename Func >
	auto for_each_iterator_pair_outwards(Rng&& rng, It itOrigin, bool bSkipSelf, Func func) noexcept
		-> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::span<It> const&>())), tc::constant<tc::continue_>>
	{
		std::array<It, 2> const aitLimit = { tc::begin(rng), tc::end(rng) };
		std::array<It, 2> ait = { itOrigin, itOrigin };

		if (!bSkipSelf) {
			_ASSERT(tc::back(aitLimit) != tc::back(ait));
			tc_return_if_break(tc::continue_if_not_break(func, tc::begin_next<tc::return_take>(tc::as_const(ait))))
			++tc::back(ait);
		} else if(tc::back(aitLimit) != tc::back(ait)) {
			++tc::back(ait);
		}

		for (;;) {
			if (tc::front(aitLimit) == tc::front(ait)) {
				for (; tc::back(ait) != tc::back(aitLimit); ++tc::back(ait)) {
					tc_return_if_break(tc::continue_if_not_break(func, tc::begin_next<tc::return_drop>(tc::as_const(ait))))
				}
				return tc::constant<tc::continue_>();
			}
			if (tc::back(aitLimit) == tc::back(ait)) {
				while (tc::front(ait) != tc::front(aitLimit)) {
					--tc::front(ait);
					tc_return_if_break(tc::continue_if_not_break(func, tc::begin_next<tc::return_take>(tc::as_const(ait))))
				}
				return tc::constant<tc::continue_>();
			}
			--tc::front(ait);
			tc_return_if_break(tc::continue_if_not_break(func, tc::as_const(ait)))
			++tc::back(ait);
		}
	}


	template < typename RangeReturn, typename Rng, typename It, typename Pred = tc::identity>
	[[nodiscard]] constexpr decltype(auto) find_closest_if(Rng&& rng, It it, bool const bSkipSelf, Pred pred = Pred()) noexcept {
		tc::storage_for<tc::iterator_cache<It>> oitc;
		if (tc::break_ == tc::for_each_iterator_pair_outwards(rng, tc_move(it), bSkipSelf, [&](auto const& rngit) noexcept {
			return tc::for_each(rngit, [&](auto const& it) noexcept {
				oitc.ctor(it);
				if (tc::explicit_cast<bool>(tc_invoke(pred, tc::as_const(**oitc)))) { return tc::break_; }
				oitc.dtor();
				return tc::continue_;
			});
		})) {
			tc_scope_exit { oitc.dtor(); };
			return RangeReturn::pack_element(oitc->m_it_(), tc_move_if_owned(rng), **tc_move(oitc));
		} else {
			return RangeReturn::pack_no_element(tc_move_if_owned(rng));
		}
	}

	template < typename RangeReturn, typename Rng, typename Index, typename Pred = tc::identity>
	[[nodiscard]] decltype(auto) find_closest_if_with_index(Rng&& rng, Index&& n, bool const bSkipSelf, Pred&& pred = Pred()) noexcept {
		return find_closest_if<RangeReturn>(tc_move_if_owned(rng), tc::begin_next<tc::return_border>(rng, tc_move_if_owned(n)), bSkipSelf, tc_move_if_owned(pred));
	}
}
