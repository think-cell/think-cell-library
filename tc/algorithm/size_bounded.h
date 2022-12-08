
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
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
	template< typename Rng, typename T>
	[[nodiscard]] constexpr auto size_bounded(Rng const& rng, T const nBound) noexcept {
		if constexpr( tc::has_size<Rng>::value ) {
			return tc::size(rng);
		} else if constexpr( tc::is_range_with_iterators<Rng>::value ) {
			return advance_forward_bounded( tc::begin(rng), nBound, tc::end(rng) );
		} else {
			T n = 0;
			if (0 < nBound) {
				auto Enumerate = [&](auto const&) noexcept { return tc::continue_if(nBound!=++n); };
				STATICASSERTSAME(tc::break_or_continue, decltype(tc::for_each(rng, Enumerate)), "size_bounded only works with interruptible generators");
				tc::for_each(rng, Enumerate);
			}
			return n;
		}
	}
}

