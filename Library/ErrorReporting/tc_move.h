#pragma once

#include <type_traits>

#pragma warning( disable : 4503 ) // 'identifier' : decorated name length exceeded, name was truncated
#pragma warning( disable : 4521 ) // '...' : multiple copy constructors specified (happens when two ctors, one with T const&, the other with T & parameter, are defined)

/////////////////////////////////////////////
// safer variants of std::move

template<typename T>
T&& tc_move_impl(typename std::remove_reference<T>::type& t) {
	static_assert(!std::is_lvalue_reference<T>::value, "");
	static_assert(!std::is_const<typename std::remove_reference<T>::type>::value, "");
	return static_cast<T&&>(t);
}

template<typename T>
T&& tc_move_impl(typename std::remove_reference<T>::type&& t) {
	static_assert(!std::is_lvalue_reference<T>::value, "");
	static_assert(!std::is_const<typename std::remove_reference<T>::type>::value, "");
	return static_cast<T&&>(t);
}

#define tc_move(t) tc_move_impl<decltype(t)>(t)
#define tc_move_if_owned(t) std::forward<decltype(t)>(t)

template<typename T>
typename std::remove_reference<T>::type&& tc_move_always(T&& t) { // same as std::move, but asserts on non-constness of argument
	static_assert(!std::is_const<typename std::remove_reference<T>::type>::value, "");
	return static_cast<typename std::remove_reference<T>::type&&>(t);
}

