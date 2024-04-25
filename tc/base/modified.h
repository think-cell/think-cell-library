
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "type_traits_fwd.h"
#include "temporary.h"

namespace tc {
	template <typename T>
	concept inplace_modifiable = !std::is_lvalue_reference<tc::unwrap_temporary_t<T>>::value && !std::is_const<std::remove_reference_t<tc::unwrap_temporary_t<T>>>::value;

	template<typename T, typename Fn>
	[[nodiscard]] constexpr decltype(auto) modified_impl(T&& t, Fn fn) MAYTHROW {
		if constexpr (inplace_modifiable<T&&>) {
			fn(tc_unwrap_temporary(t));
			return tc_move(t);
		} else {
			auto tCopy = tc::decay_copy(tc_unwrap_temporary(t));
			fn(tCopy);
			return tCopy;
		}
	}
}

#define tc_modified(obj, ...) tc::modified_impl(obj, [&](auto& _) MAYTHROW -> void { __VA_ARGS__; })
