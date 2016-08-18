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
	template<typename T, std::enable_if_t< \
		std::is_same< Base, tc::decay_t<T> >::value || \
		!std::is_base_of< Base, tc::decay_t<T> >::value \
	>* = nullptr> \
	this_type& operator=(T&& t) & noexcept { \
		static_assert( sizeof(Base)==sizeof(this_type), "move other members?"); \
		Base::operator=( std::forward<T>(t) ); \
		return *this; \
	}

#define INHERIT_CTORS_ASSIGN( Derived, Base ) \
	INHERIT_CTORS( Derived, Base ) \
	INHERIT_ASSIGN( Base )
