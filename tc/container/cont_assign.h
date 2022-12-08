
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../algorithm/append.h"
#include "container.h"
#include "cont_reserve.h"
#include "insert.h"

#include <boost/range/algorithm/copy.hpp>

namespace tc {

	////////////////////////
	// generic container algorithms

	static_assert(tc::is_instance<std::vector,tc::vector<int>>::value);

	template< typename Cont, typename... Rng>
	constexpr void cont_assign(Cont&& cont, Rng&&... rng) MAYTHROW {
		if( !std::is_constant_evaluated() ) {
			(tc::assert_no_overlap(cont, std::forward<Rng>(rng)), ...);
		}
		if constexpr( has_mem_fn_clear<Cont>::value ) {
			static_assert( std::is_lvalue_reference<Cont>::value );
			cont.clear();
			if constexpr (0<sizeof...(Rng)) {
				if constexpr( has_mem_fn_lower_bound<Cont>::value || has_mem_fn_hash_function<Cont>::value ) {
					tc::cont_must_insert_range(cont, tc::concat(std::forward<Rng>(rng)...)); // MAYTHROW
				} else {
					tc::append(cont, std::forward<Rng>(rng)...); // MAYTHROW
				}
			}
		} else {
			auto itOut = tc::begin(cont);
			tc::for_each(tc::concat(tc_move_if_owned(rng)...), [&](auto&& t) noexcept {
				*itOut = tc_move_if_owned(t);
				++itOut;
			}); // MAYTHROW
			_ASSERTE(tc::end(cont)==itOut);
		}
	}

	template<typename Cont, typename Rng>
	void cont_change_with_or(Cont& cont, Rng const& rng, bool& flag) noexcept {
		cont_change_with_or(cont, rng, flag, true);
	}

	template< typename Cont, typename Rng >
	bool cont_change(Cont& cont, Rng const& rng) noexcept {
		auto itcont=tc::begin(cont);
		auto const itcontEnd=tc::end(cont);
		auto itrng=tc::begin(rng);
		auto const itrngEnd=tc::end(rng);
		for(;;) {
			if( itcont==itcontEnd ) {
				if( itrng==itrngEnd ) {
					return false;
				} else {
					break;
				}
			}
			if( itrng==itrngEnd || !tc::equal_to(*itcont, *itrng) ) {
				tc::take_inplace( cont, itcont );
				break;
			}
			++itcont;
			++itrng;
		}
		tc::append(cont,tc::drop(rng,itrng));
		return true;
	}

	template<typename T, std::size_t N, typename Rng>
	bool cont_change(T (&a)[N], Rng const& rng) noexcept {
		auto it = tc::begin(rng);
		tc::any_accu anyChanged;
		for(std::size_t i=0; i<N; ++i) {
			_ASSERT(it != tc::end(rng));
			anyChanged(tc::change(a[i], *it));
			++it;
		}

		_ASSERTEQUAL(it, tc::end(rng));
		return anyChanged;
	}

	template<typename Cont, typename Rng, typename Flag>
	void cont_change_with_or(Cont& cont, Rng const& rng, Flag& flag, Flag flagChanged) noexcept {
		_ASSERTINITIALIZED(flag);
		if( flag==flagChanged ) {
			tc::cont_assign(cont, rng);
		} else if( tc::cont_change(cont, rng) ) {
			flag=tc_move(flagChanged);
		}
	}
}
