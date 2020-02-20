
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

namespace tc {
	namespace no_adl {
		// Derived classes can be moved, but not copied.
		struct noncopyable {
		protected:
			constexpr noncopyable() noexcept {}
			noncopyable(noncopyable&&) noexcept = default;
			noncopyable& operator=(noncopyable&&) noexcept = default;
		};
	}
	using no_adl::noncopyable;

	namespace no_adl {
		// Derived classes cannot be moved or copied.
		struct nonmovable {
			nonmovable& operator=(nonmovable&&) noexcept = delete;
		};
	}
	using no_adl::nonmovable;
}
