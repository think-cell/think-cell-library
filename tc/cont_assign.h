
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "container.h"
#include "cont_reserve.h"
#include "insert.h"
#include "append.h"

#include <boost/range/algorithm/copy.hpp>

namespace tc {

	////////////////////////
	// generic container algorithms

	static_assert(tc::is_instance<std::vector,tc::vector<int>>::value);

	template< typename Cont, typename... Rng, std::enable_if_t<has_mem_fn_clear<Cont>::value && !(has_mem_fn_lower_bound<Cont>::value || has_mem_fn_hash_function<Cont>::value)>* = nullptr>
	constexpr void cont_assign(Cont& cont, Rng&&... rng) MAYTHROW {
		//tc::assert_no_overlap(cont, std::forward<Rng>(rng));
		tc::cont_clear(cont);
		tc::append(cont, std::forward<Rng>(rng)...); // MAYTHROW
	}

	template< typename Cont, typename Rng, std::enable_if_t<has_mem_fn_clear<Cont>::value && (has_mem_fn_lower_bound<Cont>::value || has_mem_fn_hash_function<Cont>::value)>* = nullptr>
	constexpr void cont_assign(Cont& cont, Rng&& rng) MAYTHROW {
		tc::assert_no_overlap(cont, std::forward<Rng>(rng));
		tc::cont_clear(cont);
		tc::cont_must_insert_range(cont, std::forward<Rng>(rng)); // MAYTHROW
	}

	template< typename Cont, typename Rng, std::enable_if_t<!has_mem_fn_clear<std::remove_reference_t<Cont>>::value && tc::is_range_with_iterators<std::remove_reference_t<Rng>>::value>* = nullptr>
	constexpr void cont_assign(Cont&& cont, Rng&& rng) MAYTHROW {
		VERIFY(boost::copy(std::forward<Rng>(rng), tc::begin(cont))==tc::end(cont)); // MAYTHROW
	}

	template< typename Cont, typename Rng, std::enable_if_t<!has_mem_fn_clear<std::remove_reference_t<Cont>>::value && !tc::is_range_with_iterators<std::remove_reference_t<Rng>>::value>* = nullptr>
	constexpr void cont_assign(Cont&& cont, Rng&& rng) MAYTHROW {
		auto itOut = tc::begin(cont);
		tc::for_each(rng, [&](auto&& t) noexcept {
			*itOut = std::forward<decltype(t)>(t);
			++itOut;
		}); // MAYTHROW
		_ASSERTE(tc::end(cont)==itOut);
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
		bool bChanged = false;
		for(std::size_t i=0; i<N; ++i) {
			_ASSERT(it != tc::end(rng));
			bChanged = tc::change(a[i], *it) || bChanged;
			++it;
		}

		_ASSERTEQUAL(it, tc::end(rng));
		return bChanged;
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
