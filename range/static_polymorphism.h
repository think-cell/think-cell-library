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

#define STATIC_VIRTUAL_METHOD_NAME( Name ) \
	Name ## _ImplDoNotCallDirectly

#define STATIC_VIRTUAL( Name ) \
	using Name ## _derived_type = Derived; \
	template<typename... Args> \
	decltype(auto) Name(Args&& ...args) { \
		return tc::derived_cast<Derived>(*this).STATIC_VIRTUAL_METHOD_NAME( Name )(std::forward<Args>(args)...); \
	} \
	template<typename... Args> \
	decltype(auto) Name(Args&& ...args) const { \
		return tc::derived_cast<Derived>(*this).STATIC_VIRTUAL_METHOD_NAME( Name )(std::forward<Args>(args)...); \
	}

#define STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD(Mod, Name) \
	STATIC_VIRTUAL( Name ) \
	Mod \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_VIRTUAL_WITH_DEFAULT_IMPL( Name ) \
	STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD( BOOST_PP_EMPTY(), Name )

#define STATIC_FINAL_MOD(Mod, Name) \
	static_assert( \
		std::is_same< \
			typename this_type::Name ## _derived_type, \
			this_type \
		>::value, \
		"Static polymorphism error" \
	); \
	Mod \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_FINAL(Name) \
	STATIC_FINAL_MOD(BOOST_PP_EMPTY(), Name)

#define STATIC_OVERRIDE_MOD(Mod, Name) \
	static_assert( \
		std::is_same<typename this_type::Name ## _derived_type, Derived>::value, \
		"Static polymorphism error" \
	); \
	Mod \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_OVERRIDE( Name ) \
	STATIC_OVERRIDE_MOD( BOOST_PP_EMPTY(), Name )
