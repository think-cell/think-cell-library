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

#include "return_decltype.h"
#include <boost/mpl/has_xxx.hpp>
#include <type_traits>

namespace tc {
	////////////////////////////////////////////////////////////////////////////////////
	// curiously recurring template patterns mapping comparison operators to compare
	namespace equality_comparable_adl_barrier {
		BOOST_MPL_HAS_XXX_TRAIT_DEF(equality_comparable_tag)

		template< typename Derived >
		struct equality_comparable {
			using equality_comparable_tag = void;
		};

		template< typename Lhs, typename Rhs, std::enable_if_t<
			has_equality_comparable_tag<Lhs>::value || has_equality_comparable_tag<Rhs>::value
		>* = nullptr >
		auto operator!=(Lhs const& lhs, Rhs const& rhs) noexcept return_decltype( !(lhs==rhs) )
	}
	using equality_comparable_adl_barrier::equality_comparable;
}
