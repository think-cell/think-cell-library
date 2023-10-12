
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../range/subrange.h"
#include "../storage_for.h"
#include "../base/assign.h"
#include "accumulate.h"
#include "compare.h"

namespace tc {

	template< typename RangeReturn, typename Better, typename Rng>
	[[nodiscard]] constexpr decltype(auto) best_element_impl(Better better, Rng&& rng) MAYTHROW {
		if constexpr( RangeReturn::requires_iterator ) {
			auto const itEnd=tc::end(rng); // MAYTHROW
			decltype(tc::begin(rng)) ait[2]={ tc::begin(rng) }; // MAYTHROW
			if(ait[0]==itEnd) {
				return RangeReturn::pack_no_element(tc_move_if_owned(rng));
			} else {
				tc::storage_for< tc::reference_or_value<decltype(*ait[0])> > aoref[2];
				aoref[0].ctor( aggregate_tag, *ait[0] ); // MAYTHROW
				for(;;){
					for( int i=0; i!=2; ++i ) { // we expect the compiler to unroll this loop
						// aoref[i] is constructed, aoref[1-i] is not constructed
						tc_scope_exit { aoref[i].dtor(); }; // also required in case of exception
						ait[1-i]=ait[i];
						for(;;) {
							// aoref[i] is constructed, aoref[1-i] is not constructed
							++ait[1-i];
							if(ait[1-i]==itEnd) {
								return RangeReturn::pack_element(tc_move_always(ait[i]),tc_move_if_owned(rng),**tc_move_always(aoref[i]));
							}
							aoref[1-i].ctor( aggregate_tag, *ait[1-i] ); // MAYTHROW
							try {
								if( tc::invoke(better, tc::as_const(**aoref[1-i]), tc::as_const(**aoref[i])) ) { // MAYTHROW
									break; // only path where aoref[1-i] is not destroyed
								}
							} catch(...) {
								aoref[1-i].dtor();
								throw;
							}
							aoref[1-i].dtor();
						}
					}
				}
			}
		} else if (auto ovalue = tc::accumulate_with_front(tc_move_if_owned(rng), [&](auto& valueBest, auto&& value) noexcept {
			return tc::assign_better(better, valueBest, tc_move_if_owned(value));
		})) {
			return RangeReturn::template pack_element<Rng>(*tc_move(ovalue));
		} else {
			return RangeReturn::template pack_no_element<Rng>();
		}
	}

	template< typename RangeReturn, typename Better, typename Rng, typename Projection = tc::identity>
	[[nodiscard]] constexpr decltype(auto) best_element(Better&& better, Rng&& rng, Projection&& projection = Projection()) MAYTHROW {
		return tc::best_element_impl<RangeReturn>(tc::projected(tc_move_if_owned(better), tc_move_if_owned(projection)), tc_move_if_owned(rng));
	}

	template< typename RangeReturn, typename Rng, typename Projection = tc::identity >
	[[nodiscard]] constexpr decltype(auto) min_element(Rng&& rng, Projection&& projection = Projection()) MAYTHROW {
		return tc::best_element<RangeReturn>(tc::fn_less(), tc_move_if_owned(rng), tc_move_if_owned(projection));
	}

	template< typename RangeReturn, typename Rng, typename Projection = tc::identity >
	[[nodiscard]] constexpr decltype(auto) max_element(Rng&& rng, Projection&& projection = Projection()) MAYTHROW {
		return tc::best_element<RangeReturn>(tc::fn_greater(), tc_move_if_owned(rng), tc_move_if_owned(projection));
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] constexpr decltype(auto) closest_element(Rng&& rng, T const& t) MAYTHROW {
		return tc::min_element<RangeReturn>(tc_move_if_owned(rng), [&](auto const& elem) MAYTHROW { return std::abs(t - elem); });
	}
}

