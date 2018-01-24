//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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

#include <boost/type_traits/detail/yes_no_type.hpp>

#include <type_traits>
#include <utility>

#define TC_HAS_MEM_FN_XXX_TRAIT_DEF( name, decoration, ... ) \
	template<typename U> \
	struct has_mem_fn_ ## name { \
	private: \
		template<typename T> static auto test(int) -> decltype(std::declval<T decoration >().name ( __VA_ARGS__ ), std::true_type()); \
		template<typename> static std::false_type test(...); \
	public: \
		static constexpr bool value = std::is_same<decltype(test<U>(0)), std::true_type>::value; \
	};

