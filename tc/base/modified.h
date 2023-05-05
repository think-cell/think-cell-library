
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "type_traits_fwd.h"

namespace tc {
	template<typename T, typename Fn>
	[[nodiscard]] constexpr decltype(auto) modified_impl(T&& t, Fn fn) MAYTHROW {
		if constexpr(std::is_lvalue_reference<T>::value || std::is_const<std::remove_reference_t<T> >::value) {
			auto tCopy=tc::decay_copy(t);
			fn(tCopy);
			return tCopy;
		} else {
			fn(t);
			return tc_move(t);
		}
	}
}

#define tc_modified(obj, ...) tc::modified_impl(obj, [&](auto& _) MAYTHROW -> void { __VA_ARGS__; })
