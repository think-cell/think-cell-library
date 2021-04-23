
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <type_traits>

namespace tc {
	template<typename T>
	using const_forward_t = std::conditional_t<std::is_lvalue_reference<T>::value, T, T const&&>;

	template <typename T, typename U>
	[[nodiscard]] static constexpr const_forward_t<T> const_forward(U&& u) {
		return static_cast<const_forward_t<T>>(u);
	}
}
