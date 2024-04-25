
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "fundamental.h"

namespace tc {
	namespace no_adl {
		// Derived classes can be moved and copied.
		struct TC_EMPTY_BASES copyable {};

		// Derived classes can be moved, but not copied.
		struct TC_EMPTY_BASES noncopyable {
		protected:
			constexpr noncopyable() noexcept = default;
			noncopyable(noncopyable&&) noexcept = default;
			noncopyable& operator=(noncopyable&&) noexcept = default;
		};

		// Derived classes cannot be moved or copied.
		struct TC_EMPTY_BASES nonmovable {
			nonmovable& operator=(nonmovable&&) noexcept = delete;
		};
	}
	using no_adl::copyable;
	using no_adl::noncopyable;
	using no_adl::nonmovable;
}
