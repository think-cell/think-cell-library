
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"

#include <boost/type_traits/detail/yes_no_type.hpp>

#include <type_traits>
#include <utility>

#define TC_HAS_MEM_FN_XXX_TRAIT_DEF( name, decoration, ... ) \
	template<typename U> \
	struct has_mem_fn_ ## name { \
	private: \
		template<typename T> static auto test(int) -> decltype(std::declval<T decoration >().name ( __VA_ARGS__ ), std::true_type()); \
		template<typename> static std::false_type test(...); \
	public: \
		static constexpr bool value = std::is_same<decltype(test<U>(0)), std::true_type>::value; \
	};

