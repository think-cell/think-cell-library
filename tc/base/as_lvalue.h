
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

namespace tc {
	template <typename T>
	[[nodiscard]] constexpr T& as_lvalue(T&& t) noexcept {
		return static_cast<T&>(t); // required as soon as "P2266R3: Simpler implicit move" is implemented
	}
}
