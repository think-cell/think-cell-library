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
#include "type_traits.h"

namespace tc {
	template<typename T, std::enable_if_t<std::is_pointer<T>::value || std::is_class<T>::value || tc::is_bool<T>::value>* =nullptr>
	auto bool_cast(T const& t) noexcept ->/*to trigger SFINAE*/decltype(static_cast<bool>(t)) {
		return static_cast<bool>( VERIFYINITIALIZED(t) );
	}

	TC_HAS_EXPR(bool_cast, tc::bool_cast(std::declval<T>()))
}