
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/has_xxx.h"
#include "size.h"
#include "for_each.h"

TC_HAS_MEM_FN_XXX_TRAIT_DEF( size_linear, const&)

namespace tc {
	template<typename Rng>
	[[nodiscard]] constexpr auto size_linear_raw(Rng const& rng) noexcept {
		if constexpr( tc::has_size<Rng const>::value ) {
			return tc::size_raw(rng);
		} else if constexpr( has_mem_fn_size_linear<Rng>::value ) {
			return rng.size_linear();
		} else if constexpr( tc::is_range_with_iterators<Rng const>::value ) {
#ifdef __cpp_lib_ranges // std::distance and boost::range::distance do not support end sentinels.
			return std::ranges::distance(tc::begin(rng), tc::end(rng));
#else
			typename boost::range_difference<Rng>::type result = 0;
			auto first = tc::begin(rng);
			auto const last = tc::end(rng);
			while (first != last) {
				++first;
				++result;
			}
			return result;
#endif
		} else {
			std::size_t sz=0;
			tc::for_each(rng, [&sz](auto&&...) noexcept {++sz;});
			return sz;
		}
	}

	template<typename T>
	[[nodiscard]] constexpr auto size_linear(T&& t) return_decltype_MAYTHROW(
		make_size_proxy(tc::size_linear_raw(std::forward<T>(t)))
	)
}
