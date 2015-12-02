#pragma once

#include <type_traits>

#pragma warning( disable : 4503 ) // 'identifier' : decorated name length exceeded, name was truncated
#pragma warning( disable : 4521 ) // '...' : multiple copy constructors specified (happens when two ctors, one with T const&, the other with T & parameter, are defined)

/////////////////////////////////////////////
// safer variants of std::move

template<typename T>
T&& tc_move_impl(std::remove_reference_t<T>& t) {
	static_assert(!std::is_lvalue_reference<T>::value, "");
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "");
	return static_cast<T&&>(t);
}

template<typename T>
T&& tc_move_impl(std::remove_reference_t<T>&& t) {
	static_assert(!std::is_lvalue_reference<T>::value, "");
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "");
	// TODO: static_assert(std::is_nothrow_move_constructible<std::remove_reference_t<T>>::value, "" );
	return static_cast<T&&>(t);
}

#define tc_move(t) tc_move_impl<decltype(t)>(t)
#define tc_move_if_owned(t) std::forward<decltype(t)>(t)

template<typename T>
std::remove_reference_t<T>&& tc_move_always(T&& t) { // same as std::move, but asserts on non-constness of argument
	static_assert(!std::is_const<std::remove_reference_t<T>>::value, "");
	return static_cast<std::remove_reference_t<T>&&>(t);
}

