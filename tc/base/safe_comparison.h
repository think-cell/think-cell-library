
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits.h"
#include "casts.h"
#include <concepts>

namespace tc {
	// By default, a comparison is safe; specialize to mark as unsafe.
	// (We can't delegate to safely_convertible_to, as we can't tell whether `T == U` is valid because of implicit conversions,
	// or because of an overloaded operator== that takes those types and handles them safely.)
	template <typename T, typename U>
	constexpr bool safe_comparison = true;

	// Comparison between mixed integral types is only safe as long as neither is a char type and they have the same signedness.
	template <std::integral T, std::integral U>
		requires (!std::same_as<T, U>)
	constexpr bool safe_comparison<T, U>
		= !tc::char_type<T> && !tc::char_type<U>
		&& std::is_signed<T>::value == std::is_signed<U>::value;

	template <typename T, typename U>
	concept safely_equality_comparable_with = std::equality_comparable_with<T, U> && safe_comparison<tc::decay_t<T>, tc::decay_t<U>>;

	template <typename T, typename U>
	concept safely_totally_ordered_with = std::totally_ordered_with<T, U> && safe_comparison<tc::decay_t<T>, tc::decay_t<U>>;
}
