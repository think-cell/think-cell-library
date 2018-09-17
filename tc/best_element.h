
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_fwd.h"
#include "assign.h"
#include "sub_range.h"
#include "storage_for.h"
#include "accumulate.h"
#include "compare.h"

namespace tc {

	template< template<typename> class RangeReturn, typename Rng, typename Less, std::enable_if_t<!RangeReturn<Rng>::requires_iterator>* = nullptr >
	typename RangeReturn<Rng>::type best_element(Rng&& rng, Less&& less) MAYTHROW {
		if (auto ovalue = tc::accumulate_with_front(std::forward<Rng>(rng), tc::fn_assign_better(std::forward<Less>(less)))) {
			return RangeReturn<Rng>::pack_element(*tc_move(ovalue));
		} else {
			return RangeReturn<Rng>::pack_no_element();
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename Less, std::enable_if_t<RangeReturn<Rng>::requires_iterator>* = nullptr >
	decltype(auto) best_element(Rng&& rng, Less less) MAYTHROW {
		auto const itEnd=tc::end(rng); // MAYTHROW
		decltype(tc::begin(rng)) ait[2]={ tc::begin(rng) }; // MAYTHROW
		if(ait[0]==itEnd) {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		} else {
			tc::storage_for< tc::reference_or_value<tc::range_reference_t< Rng >> > aoref[2];
			aoref[0].ctor( aggregate_tag(), *ait[0] ); // MAYTHROW
			for(;;){
				for( int i=0; i!=2; ++i ) { // we expect the compiler to unroll this loop
					// aoref[i] is constructed, aoref[1-i] is not constructed
					scope_exit( aoref[i].dtor() ); // also required in case of exception
					ait[1-i]=ait[i];
					for(;;) {
						// aoref[i] is constructed, aoref[1-i] is not constructed
						++ait[1-i];
						if(ait[1-i]==itEnd) {
							return RangeReturn<Rng>::pack_element(tc_move_always(ait[i]),std::forward<Rng>(rng),**tc_move_always(aoref[i]));
						}
						aoref[1-i].ctor( aggregate_tag(), *ait[1-i] ); // MAYTHROW
						try {
							if( less(**aoref[1-i],**aoref[i]) ) { // MAYTHROW
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

	template< template<typename> class RangeReturn, typename Rng >
	decltype(auto) min_element(Rng&& rng) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::fn_less());
	}

	template< template<typename> class RangeReturn, typename Rng >
	decltype(auto) max_element(Rng&& rng) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::fn_greater());
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) closest_element(Rng&& rng, T const& t) MAYTHROW {
		return best_element<RangeReturn>(std::forward<Rng>(rng), tc::projected(tc::fn_less(), [&](T const& t1) MAYTHROW { return std::abs(t - t1); }));
	}
}

