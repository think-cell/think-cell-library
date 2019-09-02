
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include <type_traits>

namespace tc {
	////////////////////////////////////////////////////////////////////////////////////
	// curiously recurring template patterns mapping comparison operators to compare

	namespace equality_comparable_adl {
		template<typename Derived> // suppress warning C4584: 'CDerivedClass': base-class 'tc::equality_comparable_adl::equality_comparable' is already a base-class of 'CBaseClass'
		struct equality_comparable {};

		template<typename Lhs, typename Rhs>
		auto operator!=(Lhs const& lhs, Rhs const& rhs) noexcept -> decltype(lhs==rhs) {
			STATICASSERTSAME(bool, decltype(lhs==rhs));
			return !(lhs==rhs);
		}
	}
	using equality_comparable_adl::equality_comparable;
}

