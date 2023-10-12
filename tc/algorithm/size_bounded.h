
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../container/container_traits.h"
#include "size.h"
#include "break_or_continue.h"
#include "for_each.h"

namespace tc {
	template< typename It, typename T, typename Sentinel >
	T advance_forward_bounded(It&& it, T n, Sentinel&& itBound) noexcept {
		_ASSERT(0 <= n);
		if constexpr( std::convertible_to<
			typename boost::iterator_traversal<std::remove_reference_t<It>>::type,
			boost::iterators::random_access_traversal_tag
		> && requires { itBound - it; } ) {
			if (tc::assign_better(tc::fn_less_equal(), n, tc::make_size_proxy(itBound - it))) {
				it = tc_move_if_owned(itBound);
			} else {
				it += n;
			}
			return n;
		} else {
			// size_proxy does not provide operator++ and the operation cannot fail here,
			// because nCount is always inside interval [0,n].
			auto nCount = tc::explicit_cast<decltype(tc::unmake_size_proxy(n))>(0);
			while (nCount != n && it != itBound) {
				++nCount;
				++it;
			}
			tc_return_cast(nCount);
		}
	}

	template< typename Rng, typename T>
	[[nodiscard]] constexpr auto size_bounded(Rng const& rng, T const nBound) noexcept {
		if constexpr( tc::has_size<Rng> ) {
			return tc::size(rng);
		} else if constexpr( tc::range_with_iterators<Rng> ) {
			return advance_forward_bounded( tc::begin(rng), nBound, tc::end(rng) );
		} else {
			T n = 0;
			if (0 < nBound) {
				auto const Enumerate = [&](tc::unused) noexcept { return tc::continue_if(nBound!=++n); };
				STATICASSERTSAME(tc::break_or_continue, decltype(tc::for_each(rng, Enumerate)), "size_bounded only works with interruptible generators");
				tc::for_each(rng, Enumerate);
			}
			return n;
		}
	}
}

