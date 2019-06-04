
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include <utility>

namespace tc {
	struct noop {
		template<typename ...Args> void operator()(Args const&...) const& noexcept {}
	};

	template<typename T=void>
	struct never_called {
		template<typename ...Args> T operator()(Args const&...) const& noexcept {_ASSERTFALSE; return {}; }
	};

	template<>
	struct never_called<void> {
		template<typename ...Args> void operator()(Args const&...) const {_ASSERTFALSE; }
	};

	template<auto val>
	struct constexpr_function {
		template< typename ...Args > constexpr auto operator()( Args const&... ) const& noexcept { return val; }
	};

	#define MAKE_CONSTEXPR_FUNCTION(val) \
		[](auto&&...) constexpr noexcept { auto ret = val; static_assert(sizeof(ret)<=sizeof(void*)); return ret; }

	struct identity {
		template< typename T >
		T&& operator()(T&& t) const& noexcept {
			return std::forward<T>(t);
		}
	};
}
