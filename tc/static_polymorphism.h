
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include <type_traits>

#define STATIC_VIRTUAL_METHOD_NAME( Name ) \
	Name ## _ImplDoNotCallDirectly

#define STATIC_VIRTUAL_DISPATCH_IMPL_NAME( Name ) \
	Name ## _DispatchDoNotCallDirectly

#define STATIC_VIRTUAL_FALLBACK_NAME( Name ) \
	Name ## _FallbackDoNotCallDirectly

#define STATIC_VIRTUAL_FORWARD_IMPL( Mod, Name, Decoration ) \
	template<typename Derived_=Derived, typename... Args> \
	Mod \
	auto Name(Args&&... args) Decoration return_decltype_xvalue_by_ref_MAYTHROW( \
		/* tc::derived_cast<Derived_>(*this) fails on MSVC 15.8 when evaluated as constant. */ \
		STATIC_VIRTUAL_DISPATCH_IMPL_NAME(Name)(static_cast<Derived_ Decoration>(*tc::derived_cast<Derived_>(this)), std::forward<Args>(args)...) \
	)

#define STATIC_VIRTUAL_FORWARD_ALL_IMPL( Mod, Name ) \
	using Name ## _derived_type = Derived; \
	using Name ## _declaring_type = this_type; \
	STATIC_VIRTUAL_FORWARD_IMPL( Mod, Name, & ) \
	STATIC_VIRTUAL_FORWARD_IMPL( Mod, Name, const& ) \
	STATIC_VIRTUAL_FORWARD_IMPL( Mod, Name, && ) \
	STATIC_VIRTUAL_FORWARD_IMPL( Mod, Name, const&& )

#define STATIC_VIRTUAL_MOD( Mod, Name ) \
	template<typename Derived_, typename... Args> \
	static \
	Mod \
	auto STATIC_VIRTUAL_DISPATCH_IMPL_NAME(Name)(Derived_&& derived, Args&&... args) return_decltype_xvalue_by_ref_MAYTHROW( \
		std::forward<Derived_>(derived).STATIC_VIRTUAL_METHOD_NAME(Name)(std::forward<Args>(args)...) \
	) \
	STATIC_VIRTUAL_FORWARD_ALL_IMPL( Mod, Name )

#define STATIC_VIRTUAL( Name ) \
	STATIC_VIRTUAL_MOD( BOOST_PP_EMPTY(), Name ) 

#define STATIC_VIRTUAL_CONSTEXPR( Name ) \
	STATIC_VIRTUAL_MOD( constexpr, Name )

// force implementation as static method, using STATIC_FINAL_MOD(static, Name) 
#define STATIC_STATIC_VIRTUAL( Name ) \
	using Name ## _derived_type = Derived; \
	using Name ## _declaring_type = this_type; \
	template<typename Derived_=Derived, typename... Args> \
	static auto Name(Args&& ...args) return_decltype_xvalue_by_ref_MAYTHROW( \
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

#define STATIC_FINAL_DECLARING(Declaring, Name) \
	STATIC_FINAL_MOD_DECLARING(BOOST_PP_EMPTY(), TC_FWD(Declaring), Name)

#define STATIC_FINAL_MOD(Mod, Name) \
	STATIC_FINAL_MOD_DECLARING(TC_FWD(Mod), this_type::Name ## _declaring_type, Name)

#define STATIC_FINAL(Name) \
	STATIC_FINAL_MOD(BOOST_PP_EMPTY(), Name)

#define STATIC_OVERRIDE_MOD_DECLARING_BASE(Declaring, Name, ...) \
	friend typename Declaring; \
	__VA_ARGS__ \
	auto STATIC_VIRTUAL_METHOD_NAME( Name )

#define STATIC_OVERRIDE_MOD_BASE(Name, ...) \
	STATIC_OVERRIDE_MOD_DECLARING_BASE(this_type::Name ## _declaring_type, Name, TC_FWD(__VA_ARGS__))

#define STATIC_OVERRIDE_MOD_DECLARING(Mod, Declaring, Name) \
	STATIC_OVERRIDE_MOD_DECLARING_BASE(TC_FWD(Declaring), Name, \
		static_assert( \
			std::is_same<typename Declaring::Name ## _derived_type, Derived>::value, \
			"The Derived type of the class implementing a non-final static virtual method must be the same as the one of the class declaring the method." \
		); \
		Mod \
	)

#define STATIC_OVERRIDE_DECLARING(Declaring, Name) \
	STATIC_OVERRIDE_MOD_DECLARING(BOOST_PP_EMPTY(), TC_FWD(Declaring), Name)

#define STATIC_OVERRIDE_MOD(Mod, Name) \
	STATIC_OVERRIDE_MOD_DECLARING(TC_FWD(Mod), this_type::Name ## _declaring_type, Name)

#define STATIC_OVERRIDE( Name ) \
	STATIC_OVERRIDE_MOD( BOOST_PP_EMPTY(), Name )

#define STATIC_VIRTUAL_WITH_FALLBACK_MOD(Mod, Name) \
	template<typename U, typename... Args> \
	struct has_static_virtual_ ## Name ## _override { \
	private: \
		template<typename T> static auto test(int) -> decltype(std::declval<T>().STATIC_VIRTUAL_METHOD_NAME(Name)( std::declval<Args>()... ), std::true_type()); \
		template<typename> static std::false_type test(...); \
	public: \
		static constexpr bool value = decltype(test<U>(0))::value; \
	}; \
	template<typename Derived_, typename... Args, std::enable_if_t<!has_static_virtual_ ## Name ## _override<Derived_, Args...>::value>* = nullptr> \
	static \
	auto STATIC_VIRTUAL_DISPATCH_IMPL_NAME(Name)(Derived_&& derived, Args&&... args) return_decltype_xvalue_by_ref_MAYTHROW( \
		std::forward<Derived_>(derived).STATIC_VIRTUAL_FALLBACK_NAME(Name)(std::forward<Args>(args)...) \
	) \
	STATIC_VIRTUAL( Name ) \
	Mod \
	auto STATIC_VIRTUAL_FALLBACK_NAME(Name)
