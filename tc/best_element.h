
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_fwd.h"
#include "assign.h"
#include "subrange.h"
#include "storage_for.h"
#include "accumulate.h"
#include "compare.h"

namespace tc {

	template< typename RangeReturn, typename Better, typename Rng, std::enable_if_t<!RangeReturn::requires_iterator>* = nullptr >
	[[nodiscard]] decltype(auto) best_element_impl(Better&& better, Rng&& rng) MAYTHROW {
		if (auto ovalue = tc::accumulate_with_front(std::forward<Rng>(rng), tc::fn_assign_better(std::forward<Better>(better)))) {
			return RangeReturn::template pack_element<Rng>(*tc_move(ovalue));
		} else {
			return RangeReturn::template pack_no_element<Rng>();
		}
	}

	template< typename RangeReturn, typename Better, typename Rng, std::enable_if_t<RangeReturn::requires_iterator>* = nullptr >
	[[nodiscard]] decltype(auto) best_element_impl(Better better, Rng&& rng) MAYTHROW {
		auto const itEnd=tc::end(rng); // MAYTHROW
		decltype(tc::begin(rng)) ait[2]={ tc::begin(rng) }; // MAYTHROW
		if(ait[0]==itEnd) {
			return RangeReturn::pack_no_element(std::forward<Rng>(rng));
		} else {
			tc::storage_for< tc::reference_or_value<decltype(*ait[0])> > aoref[2];
			aoref[0].ctor( aggregate_tag, *ait[0] ); // MAYTHROW
			for(;;){
				for( int i=0; i!=2; ++i ) { // we expect the compiler to unroll this loop
					// aoref[i] is constructed, aoref[1-i] is not constructed
					scope_exit( aoref[i].dtor() ); // also required in case of exception
					ait[1-i]=ait[i];
					for(;;) {
						// aoref[i] is constructed, aoref[1-i] is not constructed
						++ait[1-i];
						if(ait[1-i]==itEnd) {
							return RangeReturn::pack_element(tc_move_always(ait[i]),std::forward<Rng>(rng),**tc_move_always(aoref[i]));
						}
						aoref[1-i].ctor( aggregate_tag, *ait[1-i] ); // MAYTHROW
						try {
							if( better(tc::as_const(**aoref[1-i]), tc::as_const(**aoref[i])) ) { // MAYTHROW
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
	}

	template< typename RangeReturn, typename Better, typename Rng, typename Projection = tc::identity>
	[[nodiscard]] decltype(auto) best_element(Better&& better, Rng&& rng, Projection&& projection = Projection()) MAYTHROW {
		return tc::best_element_impl<RangeReturn>(tc::projected(std::forward<Better>(better), std::forward<Projection>(projection)), std::forward<Rng>(rng));
	}

	template< typename RangeReturn, typename Rng, typename Projection = tc::identity >
	[[nodiscard]] decltype(auto) min_element(Rng&& rng, Projection&& projection = Projection()) MAYTHROW {
		return tc::best_element<RangeReturn>(tc::fn_less(), std::forward<Rng>(rng), std::forward<Projection>(projection));
	}

	template< typename RangeReturn, typename Rng, typename Projection = tc::identity >
	[[nodiscard]] decltype(auto) max_element(Rng&& rng, Projection&& projection = Projection()) MAYTHROW {
		return tc::best_element<RangeReturn>(tc::fn_greater(), std::forward<Rng>(rng), std::forward<Projection>(projection));
	}

	template< typename RangeReturn, typename Rng, typename T >
	[[nodiscard]] decltype(auto) closest_element(Rng&& rng, T const& t) MAYTHROW {
		return tc::min_element<RangeReturn>(std::forward<Rng>(rng), [&](auto const& elem) MAYTHROW { return std::abs(t - elem); });
	}
}

