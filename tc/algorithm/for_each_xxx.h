
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../array.h"
#include "filter_inplace.h"
#include "restrict_size_decrement.h"


namespace tc {
	/////////////////////////////////////////////////////
	// may_remove_current

	namespace no_adl {
		template<typename Rng>
		struct [[nodiscard]] may_remove_current_impl final { // TODO VS2019: if constexpr does not work well in lambda in VS15.8.0
			tc::reference_or_value<Rng> m_rng;
			constexpr explicit may_remove_current_impl(Rng&& rng) noexcept: m_rng(tc::aggregate_tag, tc_move_if_owned(rng)) {}

			template<typename Func>
			constexpr auto operator()(Func func) /* no & */ MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(*m_rng))), tc::constant<tc::continue_>> {
				auto it=tc::begin(*m_rng);
				auto const itEnd=tc::end(*m_rng);
				while( it!=itEnd ) {
					auto const rsize = restrict_size_decrement(*m_rng, 0, 1);
					tc_yield(func, *it++);
				}
				return tc::constant<tc::continue_>();
			}
		};
	}

	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng >
	[[nodiscard]] constexpr auto may_remove_current(Rng&& rng) noexcept code_return_decltype(
		static_assert( !tc::range_filter_by_move_element< std::remove_reference_t<Rng> >::value );,
		no_adl::may_remove_current_impl<Rng>(tc_move_if_owned(rng))
	)
}
