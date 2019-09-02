
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <type_traits>

#pragma warning( disable : 4503 ) // 'identifier' : decorated name length exceeded, name was truncated
#pragma warning( disable : 4521 ) // '...' : multiple copy constructors specified (happens when two ctors, one with T const&, the other with T & parameter, are defined)

/////////////////////////////////////////////
// safer variants of std::move

template<typename T>
constexpr T&& tc_move_impl(std::remove_reference_t<T>& t) noexcept {
	static_assert(!std::is_lvalue_reference<T>::value, "Are you sure you want to move out of an lvalue reference? Then use tc_move_always.");
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "Cannot move out of const.");
	return static_cast<T&&>(t);
}

template<typename T>
constexpr T&& tc_move_impl(std::remove_reference_t<T>&& t) noexcept {
	static_assert(!std::is_lvalue_reference<T>::value, "Are you sure you want to move out of an lvalue reference? Then use tc_move_always.");
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "Cannot move out of const.");
	// TODO: static_assert(std::is_nothrow_move_constructible<std::remove_reference_t<T>>::value);
	return static_cast<T&&>(t);
}

#define tc_move(t) tc_move_impl<decltype(t)>(t)
#define tc_move_if_owned(t) std::forward<decltype(t)>(t)

template<typename T>
constexpr std::remove_reference_t<T>&& tc_move_always(T&& t) noexcept { // same as std::move, but asserts on non-constness of argument
	static_assert(!std::is_const<std::remove_reference_t<T>>::value);
	return static_cast<std::remove_reference_t<T>&&>(t);
}

