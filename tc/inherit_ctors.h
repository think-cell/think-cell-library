
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once


#define INHERIT_CTORS( Derived, ... ) \
	template< typename T0, std::enable_if_t< \
		( std::is_same< __VA_ARGS__, tc::decay_t<T0> >::value || \
		!std::is_base_of< __VA_ARGS__, tc::decay_t<T0> >::value ) && \
		std::is_convertible< T0 &&, __VA_ARGS__ >::value \
	>* =nullptr> \
	Derived( T0&& t0 ) noexcept(std::is_nothrow_constructible<__VA_ARGS__, T0&&>::value) \
	:	__VA_ARGS__( std::forward<T0>(t0) ) {} \
	template< typename T0, std::enable_if_t< \
		( std::is_same< __VA_ARGS__, tc::decay_t<T0> >::value || \
		!std::is_base_of< __VA_ARGS__, tc::decay_t<T0> >::value ) && \
		!std::is_convertible< T0 &&, __VA_ARGS__ >::value \
	>* =nullptr> \
	explicit Derived( T0&& t0) noexcept(std::is_nothrow_constructible<__VA_ARGS__, T0&&>::value) \
	:	__VA_ARGS__( std::forward<T0>(t0) ) {} \
	template< typename T0, typename T1, typename ...Ts > \
	explicit Derived( T0&& t0, T1&& t1, Ts&& ... ts ) noexcept(std::is_nothrow_constructible<__VA_ARGS__, T0&&, T1&&, Ts&&...>::value) \
	:	__VA_ARGS__( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<Ts>(ts)... ) {}

// Cannot test for sizeof(Base)==sizeof(Derived) in enable_if.
// Derived is not a complete type yet and clang does not compile that.
// Visual Studio did compile it, but it never worked correctly: 
// _graphlabelposition had a different size than tc::geo::point<int> (because
// empty base class optimization failed) but the inherited operator=
// was used nonetheless.
#define INHERIT_ASSIGN( ... ) \
	template<typename TAssignSource, std::enable_if_t< \
		std::is_same< __VA_ARGS__, tc::decay_t<TAssignSource> >::value || \
		!std::is_base_of< __VA_ARGS__, tc::decay_t<TAssignSource> >::value \
	>* = nullptr> \
	this_type& operator=(TAssignSource&& t) & noexcept { \
		STATICASSERTEQUAL( sizeof(__VA_ARGS__), sizeof(this_type), "move other members?"); \
		__VA_ARGS__::operator=( std::forward<TAssignSource>(t) ); \
		return *this; \
	}

#define INHERIT_CTORS_ASSIGN( Derived, ... ) \
	INHERIT_CTORS( Derived, __VA_ARGS__ ) \
	INHERIT_ASSIGN( __VA_ARGS__ )
