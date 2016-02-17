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

#include <type_traits>

#pragma warning( disable : 4503 ) // 'identifier' : decorated name length exceeded, name was truncated
#pragma warning( disable : 4521 ) // '...' : multiple copy constructors specified (happens when two ctors, one with T const&, the other with T & parameter, are defined)

/////////////////////////////////////////////
// safer variants of std::move

template<typename T>
T&& tc_move_impl(std::remove_reference_t<T>& t) noexcept {
	static_assert(!std::is_lvalue_reference<T>::value, "Are you sure you want to move out of an lvalue reference? Then use tc_move_always.");
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "Cannot move out of const.");
	return static_cast<T&&>(t);
}

template<typename T>
T&& tc_move_impl(std::remove_reference_t<T>&& t) noexcept {
	static_assert(!std::is_lvalue_reference<T>::value, "Are you sure you want to move out of an lvalue reference? Then use tc_move_always.");
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "Cannot move out of const.");
	// TODO: static_assert(std::is_nothrow_move_constructible<std::remove_reference_t<T>>::value, "" );
	return static_cast<T&&>(t);
}

#define tc_move(t) tc_move_impl<decltype(t)>(t)
#define tc_move_if_owned(t) std::forward<decltype(t)>(t)

template<typename T>
std::remove_reference_t<T>&& tc_move_always(T&& t) noexcept { // same as std::move, but asserts on non-constness of argument
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "");
	return static_cast<std::remove_reference_t<T>&&>(t);
}

