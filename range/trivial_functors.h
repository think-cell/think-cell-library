//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

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

	template<typename T, T tValue>
	struct constexpr_function {
		template< typename ...Args > T operator()( Args const&... ) const& noexcept { return tValue; }
	};

	#define MAKE_CONSTEXPR_FUNCTION(val) (tc::constexpr_function< decltype(val), (val) >())

	struct identity {
		template< typename T >
		T&& operator()(T&& t) const& noexcept {
			return std::forward<T>(t);
		}
	};
}
