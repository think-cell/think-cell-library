
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once


#define INHERIT_CTORS( Derived, Base ) \
	template< typename T0, std::enable_if_t< \
		( std::is_same< Base, tc::decay_t<T0> >::value || \
		!std::is_base_of< Base, tc::decay_t<T0> >::value ) && \
		std::is_convertible< T0 &&, Base >::value \
	>* =nullptr> \
	Derived( T0&& t0 ) noexcept(std::is_nothrow_constructible<Base, T0&&>::value) \
	:	Base( std::forward<T0>(t0) ) {} \
	template< typename T0, std::enable_if_t< \
		( std::is_same< Base, tc::decay_t<T0> >::value || \
		!std::is_base_of< Base, tc::decay_t<T0> >::value ) && \
		!std::is_convertible< T0 &&, Base >::value \
	>* =nullptr> \
	explicit Derived( T0&& t0) noexcept(std::is_nothrow_constructible<Base, T0&&>::value) \
	:	Base( std::forward<T0>(t0) ) {} \
	template< typename T0, typename T1, typename ...Ts > \
	explicit Derived( T0&& t0, T1&& t1, Ts&& ... ts ) noexcept(std::is_nothrow_constructible<Base, T0&&, T1&&, Ts&&...>::value) \
	:	Base( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<Ts>(ts)... ) {}

// Cannot test for sizeof(Base)==sizeof(Derived) in enable_if.
// Derived is not a complete type yet and clang does not compile that.
// Visual Studio did compile it, but it never worked correctly: 
// _graphlabelposition had a different size than tc::geo::point<int> (because
// empty base class optimization failed) but the inherited operator=
// was used nonetheless.
#define INHERIT_ASSIGN( Base ) \
	template<typename TAssignSource, std::enable_if_t< \
		std::is_same< Base, tc::decay_t<TAssignSource> >::value || \
		!std::is_base_of< Base, tc::decay_t<TAssignSource> >::value \
	>* = nullptr> \
	this_type& operator=(TAssignSource&& t) & noexcept { \
		static_assert( sizeof(Base)==sizeof(this_type), "move other members?"); \
		Base::operator=( std::forward<TAssignSource>(t) ); \
		return *this; \
	}

#define INHERIT_CTORS_ASSIGN( Derived, Base ) \
	INHERIT_CTORS( Derived, Base ) \
	INHERIT_ASSIGN( Base )
