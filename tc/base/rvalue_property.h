
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "return_decltype.h"
#include "move.h"

#define RVALUE_THIS_NAMED_OVERLOAD_CONST(METHOD, FUNC) \
	template<typename... Args> constexpr auto METHOD(Args&& ...args) const& return_decltype_allow_xvalue_MAYTHROW(FUNC(*this, tc_move_if_owned(args)...)) \
	template<typename... Args> constexpr auto METHOD(Args&& ...args) const&& return_decltype_allow_xvalue_MAYTHROW(FUNC(tc_move_always_even_const(*this), tc_move_if_owned(args)...))

// An underscore is appended to the static member function name because otherwise Visual Studio (as of version 19.15.26726) would trigger error C1202:
// "recursive type or function dependency context too complex", while using the return_decltype_* macros for sfinae

#define RVALUE_THIS_OVERLOAD_CONST(METHOD) \
	RVALUE_THIS_NAMED_OVERLOAD_CONST(METHOD, BOOST_JOIN(METHOD,_))

#define RVALUE_THIS_NAMED_OVERLOAD_MOVABLE(METHOD, FUNC) \
	RVALUE_THIS_NAMED_OVERLOAD_CONST(METHOD, FUNC) \
	template<typename... Args> constexpr auto METHOD(Args&& ...args) && return_decltype_allow_xvalue_MAYTHROW(FUNC(tc_move_always(*this), tc_move_if_owned(args)...))

#define RVALUE_THIS_OVERLOAD_MOVABLE(METHOD) \
	RVALUE_THIS_NAMED_OVERLOAD_MOVABLE(METHOD, BOOST_JOIN(METHOD,_))

#define RVALUE_THIS_NAMED_OVERLOAD_MOVABLE_MUTABLE_REF(METHOD, FUNC) \
	RVALUE_THIS_NAMED_OVERLOAD_MOVABLE(METHOD, FUNC) \
	template<typename... Args> constexpr auto METHOD(Args&& ...args) & return_decltype_allow_xvalue_MAYTHROW(FUNC(*this, tc_move_if_owned(args)...))

#define RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(METHOD) \
	RVALUE_THIS_NAMED_OVERLOAD_MOVABLE_MUTABLE_REF(METHOD, BOOST_JOIN(METHOD,_))
