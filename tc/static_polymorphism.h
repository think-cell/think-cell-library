
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include <type_traits>

#define STATIC_VIRTUAL_METHOD_NAME( Name ) \
	Name ## _ImplDoNotCallDirectly

#define STATIC_VIRTUAL_MOD( Mod, Name ) \
	using Name ## _derived_type = Derived; \
	using Name ## _declaring_type = this_type; \
	template<typename Derived_=Derived, typename... Args> \
	Mod \
	auto Name(Args&& ...args) & MAYTHROW return_decltype_rvalue_by_ref ( \
		tc::derived_cast<Derived_>(*this). STATIC_VIRTUAL_METHOD_NAME(Name) (std::forward<Args>(args)...) \
	) \
	template<typename Derived_=Derived, typename... Args> \
	Mod \
	auto Name(Args&& ...args) const& MAYTHROW return_decltype_rvalue_by_ref ( \
		tc::derived_cast<Derived_>(this)-> STATIC_VIRTUAL_METHOD_NAME(Name) (std::forward<Args>(args)...) \
	) \
	template<typename Derived_=Derived, typename... Args> \
	Mod \
	auto Name(Args&& ...args) && MAYTHROW return_decltype_rvalue_by_ref ( \
		tc_move_always(tc::derived_cast<Derived_>(*this)). STATIC_VIRTUAL_METHOD_NAME(Name) (std::forward<Args>(args)...) \
	) \
	template<typename Derived_=Derived, typename... Args> \
	Mod \
	auto Name(Args&& ...args) const&& MAYTHROW return_decltype_rvalue_by_ref ( \
		std::move(tc::derived_cast<Derived_>(*this)). STATIC_VIRTUAL_METHOD_NAME(Name) (std::forward<Args>(args)...) \
	)

#define STATIC_VIRTUAL( Name ) \
	STATIC_VIRTUAL_MOD( BOOST_PP_EMPTY(), Name ) 

#define STATIC_VIRTUAL_CONSTEXPR( Name ) \
	STATIC_VIRTUAL_MOD( constexpr, Name )

// force implementation as static method, using STATIC_FINAL_MOD(static, Name) 
#define STATIC_STATIC_VIRTUAL( Name ) \
	using Name ## _derived_type = Derived; \
	using Name ## _declaring_type = this_type; \
	template<typename Derived_=Derived, typename... Args> \
	static auto Name(Args&& ...args) MAYTHROW return_decltype_rvalue_by_ref ( \
		Derived_:: STATIC_VIRTUAL_METHOD_NAME(Name) (std::forward<Args>(args)...) \
	)

#define STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD(Mod, Name) \
	STATIC_VIRTUAL( Name ) \
	Mod \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_VIRTUAL_WITH_DEFAULT_IMPL( Name ) \
	STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD( BOOST_PP_EMPTY(), Name )

#define STATIC_FINAL_MOD_DECLARING(Mod, Declaring, Name) \
	friend typename Declaring; \
	static_assert( \
		std::is_same< typename Declaring::Name ## _derived_type, this_type >::value, \
		"The class implementing the final static virtual method must be the Derived type of the class declaring the method." \
	); \
	Mod \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_WRAP(...) \
    __VA_ARGS__

#define STATIC_FINAL_DECLARING(Declaring, Name) \
	STATIC_FINAL_MOD_DECLARING(BOOST_PP_EMPTY(), STATIC_WRAP(Declaring), Name)

#define STATIC_FINAL_MOD(Mod, Name) \
	STATIC_FINAL_MOD_DECLARING(STATIC_WRAP(Mod), this_type::Name ## _declaring_type, Name)

#define STATIC_FINAL(Name) \
	STATIC_FINAL_MOD(BOOST_PP_EMPTY(), Name)

#define STATIC_OVERRIDE_MOD_DECLARING_BASE(Declaring, Name, ...) \
	friend typename Declaring; \
	__VA_ARGS__ \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_OVERRIDE_MOD_BASE(Name, ...) \
	STATIC_OVERRIDE_MOD_DECLARING_BASE(this_type::Name ## _declaring_type, Name, STATIC_WRAP(__VA_ARGS__))

#define STATIC_OVERRIDE_MOD_DECLARING(Mod, Declaring, Name) \
	STATIC_OVERRIDE_MOD_DECLARING_BASE(STATIC_WRAP(Declaring), Name, \
		static_assert( \
			std::is_same<typename Declaring::Name ## _derived_type, Derived>::value, \
			"The Derived type of the class implementing a non-final static virtual method must be the same as the one of the class declaring the method." \
		); \
		Mod \
	)

#define STATIC_OVERRIDE_DECLARING(Declaring, Name) \
	STATIC_OVERRIDE_MOD_DECLARING(BOOST_PP_EMPTY(), STATIC_WRAP(Declaring), Name)

#define STATIC_OVERRIDE_MOD(Mod, Name) \
	STATIC_OVERRIDE_MOD_DECLARING(STATIC_WRAP(Mod), this_type::Name ## _declaring_type, Name)

#define STATIC_OVERRIDE( Name ) \
	STATIC_OVERRIDE_MOD( BOOST_PP_EMPTY(), Name )
