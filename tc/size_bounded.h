
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "container_traits.h"
#include "size.h"
#include "sub_range.h"


namespace tc {

	template< typename Rng, typename T, std::enable_if_t<tc::has_size<Rng>::value>* = nullptr >
	auto size_bounded(Rng const& rng, T const) noexcept {
		return tc::size(rng);
	}

	template< typename Rng, typename T, std::enable_if_t<!tc::has_size<Rng>::value && tc::is_range_with_iterators<Rng>::value>* = nullptr >
	auto size_bounded(Rng const& rng, T const nBound) noexcept {
		return advance_forward_bounded( tc::begin(rng), nBound, tc::end(rng) );
	}

	template< typename Rng, typename T, std::enable_if_t<!tc::has_size<Rng>::value && !tc::is_range_with_iterators<Rng>::value>* = nullptr >
	auto size_bounded(Rng const& rng, T const nBound) noexcept {
		T n = 0;
		if (0 < nBound) {
			auto Enumerate = [&](auto const&) noexcept { return tc::continue_if(nBound!=++n); };
			STATICASSERTSAME(tc::break_or_continue, decltype(rng(Enumerate)), "size_bounded only works with interruptible generators");
			tc::for_each(rng, Enumerate);
		}
		return n;
	}
}

