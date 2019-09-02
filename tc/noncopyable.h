
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

namespace tc {
	namespace no_adl
	{
		// derived classes can be moved, but not copied
		struct noncopyable {
		protected:
			noncopyable() noexcept {}

			noncopyable(noncopyable const&) = delete;
			noncopyable& operator=(noncopyable const&) = delete;
			noncopyable(noncopyable &&) noexcept = default;
			noncopyable& operator=(noncopyable &&) & noexcept = default;
		};
	}
	using no_adl::noncopyable;

	namespace no_adl {
		struct nonmovable {
		protected:
			nonmovable() noexcept {}
			~nonmovable() = default;

			nonmovable(nonmovable const&) = delete;
			nonmovable& operator=(nonmovable const&) = delete;
		};
	}
	using no_adl::nonmovable;
}
