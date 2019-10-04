
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits.h"
#include "conditional.h"

#include <optional>

namespace tc {
	template<typename Optional, typename T, std::enable_if_t<tc::is_instance<std::optional, std::remove_reference_t<Optional>>::value>* = nullptr>
	constexpr decltype(auto) optional_or( Optional&& optional, T&& t ) noexcept {
		return CONDITIONAL_RVALUE_AS_REF(optional, *std::forward<Optional>(optional), std::forward<T>(t));
	}

	template<typename Optional, typename Func, std::enable_if_t<tc::is_instance<std::optional, std::remove_reference_t<Optional>>::value>* = nullptr>
	constexpr decltype(auto) optional_or_eval( Optional&& optional, Func func ) noexcept {
		return CONDITIONAL_PRVALUE_AS_VAL(optional, *std::forward<Optional>(optional), func());
	}
}
