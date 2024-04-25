
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include <type_traits>
#include "tag_type.h"

namespace tc {
	DEFINE_TAG_TYPE(unchecked_derived_cast_tag)
}

#define STATIC_VIRTUAL_METHOD_NAME( Name ) \
	Name ## _ImplDoNotCallDirectly

#define STATIC_VIRTUAL_DISPATCH_IMPL_NAME( Name ) \
	Name ## _DispatchDoNotCallDirectly

#define STATIC_VIRTUAL_FALLBACK_NAME( Name ) \
	Name ## _FallbackDoNotCallDirectly

#define STATIC_VIRTUAL_FORWARD_IMPL( Name, Decoration, ExpandCall, DerivedCast, MaybeUncheckedDerivedCastTagWithComma ) \
	template<typename Derived_=Derived, typename... Args> \
	constexpr auto Name(Args&&... args) Decoration return_decltype_allow_xvalue_MAYTHROW( \
		ExpandCall( \
			Name, \
			TC_FWD(DerivedCast<Derived_>(static_cast<this_type Decoration>(*MSVC_WORKAROUND_THIS))), \
			MaybeUncheckedDerivedCastTagWithComma tc_move_if_owned(args)... \
		) \
	)

#define STATIC_VIRTUAL_FORWARD_ALL_IMPL( Name, ExpandCall, DerivedCast, MaybeUncheckedDerivedCastTagWithComma ) \
	STATIC_VIRTUAL_FORWARD_IMPL( Name, &, ExpandCall, DerivedCast, TC_FWD(MaybeUncheckedDerivedCastTagWithComma) ) \
	STATIC_VIRTUAL_FORWARD_IMPL( Name, const&, ExpandCall, DerivedCast, TC_FWD(MaybeUncheckedDerivedCastTagWithComma) ) \
	STATIC_VIRTUAL_FORWARD_IMPL( Name, &&, ExpandCall, DerivedCast, TC_FWD(MaybeUncheckedDerivedCastTagWithComma) ) \
	STATIC_VIRTUAL_FORWARD_IMPL( Name, const&&, ExpandCall, DerivedCast, TC_FWD(MaybeUncheckedDerivedCastTagWithComma) )

#define STATIC_VIRTUAL_COMMON( Name ) \
	using Name ## _derived_type = Derived; \
	using Name ## _declaring_type = this_type;

#define TC_STATIC_VIRTUAL_METHOD_CALL(Name, self, ...) \
	self.STATIC_VIRTUAL_METHOD_NAME(Name)(__VA_ARGS__)

#define STATIC_VIRTUAL( Name ) \
	STATIC_VIRTUAL_COMMON( Name ) \
	STATIC_VIRTUAL_FORWARD_ALL_IMPL( Name, TC_STATIC_VIRTUAL_METHOD_CALL, tc::derived_cast, /*MaybeUncheckedDerivedCastTagWithComma*/ )

#define UNCHECKED_STATIC_VIRTUAL( Name ) \
	STATIC_VIRTUAL( Name ) \
	STATIC_VIRTUAL_FORWARD_ALL_IMPL( Name, TC_STATIC_VIRTUAL_METHOD_CALL, tc::unchecked_derived_cast, TC_FWD(tc::unchecked_derived_cast_tag,) )

// force implementation as static method, using STATIC_FINAL_MOD(static, Name) 
#define STATIC_STATIC_VIRTUAL( Name ) \
	using Name ## _derived_type = Derived; \
	using Name ## _declaring_type = this_type; \
	template<typename Derived_=Derived, typename... Args_> \
	static constexpr auto Name(Args_&& ...args) return_decltype_allow_xvalue_MAYTHROW( \
		Derived_:: STATIC_VIRTUAL_METHOD_NAME(Name) (tc_move_if_owned(args)...) \
	)

#define STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD( Mod, Name ) \
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

#define TC_STATIC_VIRTUAL_DISPATCH_CALL(Name, self, ...) \
	STATIC_VIRTUAL_DISPATCH_IMPL_NAME(Name)(self, __VA_ARGS__)

#define STATIC_VIRTUAL_WITH_FALLBACK_MOD(Mod, Name) \
	STATIC_VIRTUAL_COMMON( Name ) \
	template<typename Derived_, typename... Args> \
	static constexpr auto STATIC_VIRTUAL_DISPATCH_IMPL_NAME(Name)(Derived_&& derived, Args&&... args) return_decltype_allow_xvalue_MAYTHROW( \
		tc_move_if_owned(derived).STATIC_VIRTUAL_METHOD_NAME(Name)(tc_move_if_owned(args)...) \
	) \
	template<typename Derived_, typename... Args TC_REQUIRES_CWG2369_WORKAROUND( !requires { std::declval<Derived_>().STATIC_VIRTUAL_METHOD_NAME(Name)( std::declval<Args>()... ); } ) \
	static constexpr auto STATIC_VIRTUAL_DISPATCH_IMPL_NAME(Name)(Derived_&& derived, Args&&... args) return_decltype_allow_xvalue_MAYTHROW( \
		tc_move_if_owned(derived).STATIC_VIRTUAL_FALLBACK_NAME(Name)(tc_move_if_owned(args)...) \
	) \
	STATIC_VIRTUAL_FORWARD_ALL_IMPL( Name, TC_STATIC_VIRTUAL_DISPATCH_CALL, tc::derived_cast, /*MaybeUncheckedDerivedCastTagWithComma*/ ) \
	Mod \
	auto STATIC_VIRTUAL_FALLBACK_NAME(Name)

#define STATIC_VIRTUAL_WITH_FALLBACK(Name) \
	STATIC_VIRTUAL_WITH_FALLBACK_MOD( BOOST_PP_EMPTY(), Name )
