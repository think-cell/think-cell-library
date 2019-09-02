
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "size.h"
#include "for_each.h"

namespace tc {
	template<typename Rng, std::enable_if_t<!tc::has_size<Rng const>::value && tc::is_range_with_iterators<Rng const>::value>* =nullptr>
	auto size_linear_raw(Rng const& rng) noexcept return_decltype(
		boost::distance(rng)
	)

	template<typename Rng, std::enable_if_t<!tc::has_size<Rng const>::value && !tc::is_range_with_iterators<Rng const>::value>* =nullptr>
	std::size_t size_linear_raw(Rng const& rng) noexcept {
		std::size_t sz=0;
		tc::for_each(rng, [&sz](auto&&...) noexcept {++sz;});
		return sz;
	}

	template<typename Rng, std::enable_if_t<tc::has_size<Rng const>::value>* =nullptr>
	constexpr auto size_linear_raw(Rng const& rng) noexcept return_decltype(
		tc::size_raw(rng)
	)

	template<typename T>
	constexpr auto size_linear(T&& t) noexcept return_decltype(
		make_size_proxy(tc::size_linear_raw(std::forward<T>(t)))
	)
}

