
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "append.h"
#include "element.h"
#include "../range/reverse_adaptor.h"

#include <boost/range/algorithm/heap_algorithm.hpp>

namespace tc {
	template<typename Cont, typename T, typename Less = tc::fn_less>
	void replace_heap(Cont& cont, T&& t, Less less = Less()) noexcept {
		// Replace largest item in max-heap with t.
		// Equivalent to
		// std::ranges::pop_heap(cont, less);
		// tc::back(cont) = std::forward<T>(t);
		// std::ranges::push_heap(cont, less);
		auto const n = tc::size_raw(cont);
		auto IndexAndIterator = [&](decltype(n) i) noexcept {
			return tc::make_tuple(i, tc::at<tc::return_element>(cont, i));
		};
		auto nitHole = IndexAndIterator(0);
		auto const nWithRightChildEnd = (n - 1) / 2;
		for( ;; ) {
			auto const nHole = tc::get<0>(nitHole);
			decltype(nitHole) nitGreaterChild;
			if( nHole < nWithRightChildEnd ) { // hole has right child
				nitGreaterChild = tc::best(
					tc::projected(tc::reverse_binary_rel(less), tc_fn(*tc::get<1>)),
					IndexAndIterator(nHole * 2 + 1),
					IndexAndIterator(nHole * 2 + 2)
				);
			} else if( nHole == nWithRightChildEnd && 0 == n % 2 ) { // hole has very last left child
				nitGreaterChild = IndexAndIterator(nHole * 2 + 1);
			} else { // hole has no children
				break;
			}
			if( less(t, *tc::get<1>(nitGreaterChild)) ) {
				*tc::get<1>(nitHole) = tc_move_always(*tc::get<1>(nitGreaterChild));
				nitHole = nitGreaterChild;
			} else {
				break;
			}
		}
		*tc::get<1>(nitHole) = std::forward<T>(t);
	}

	template<typename Rng, typename Less = tc::fn_less, typename PredKeep = tc::constexpr_function<false>>
	auto sort_streaming(Rng&& rng, Less&& less = Less(), PredKeep&& predKeep = PredKeep()) noexcept {
		// Notes:
		//  * not a stable sort algorithm 
		//  * not an inplace sort algorithm
		//  * first element is generated in O(n)
		return tc::generator_range_output<tc::range_value_t<Rng const&>&>([
			rng = tc::make_reference_or_value(std::forward<Rng>(rng)),
			// std heap algorithm using less create max heap, we need min heap. Hence, we use greater.
			greater = tc::reverse_binary_rel(less),
			predKeep = tc::decay_copy(std::forward<PredKeep>(predKeep))
		](auto&& sink) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(sink, std::declval<tc::range_value_t<Rng const&>&>())), tc::constant<tc::continue_>> {
			auto vec = tc::make_vector(*rng);
			boost::range::make_heap(vec, greater);
			while( auto const it = tc::front<tc::return_element_or_null>(vec) ) {
				tc_yield(sink, *it); // MAYTHROW
				if( tc::invoke(predKeep, *it) ) {
					tc::replace_heap(vec, tc::range_value_t<decltype(vec)>(tc_move_always(*it)), greater);
				} else {
					boost::range::pop_heap(vec, greater);
					tc::drop_last_inplace(vec);
				}
			}
			return tc::constant<tc::continue_>();
		});
	}
}
