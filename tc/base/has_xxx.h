
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"

#include <type_traits>
#include <utility>

#define TC_HAS_MEM_FN_XXX_CONCEPT_DEF( name, decoration, ... ) \
	template<typename T> \
	concept has_mem_fn_ ## name = requires { std::declval<T decoration >().name( __VA_ARGS__ ); };

#define TC_HAS_STATIC_FN_XXX_CONCEPT_DEF( name, ... ) \
	template<typename T> \
	concept has_static_fn_ ## name = requires { T::name(__VA_ARGS__); };
