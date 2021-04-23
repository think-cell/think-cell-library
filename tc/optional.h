
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits.h"
#include "conditional.h"
#include "meta.h"

#include <optional>

namespace tc {
	template<typename Optional, typename T>
	[[nodiscard]] constexpr decltype(auto) optional_or( Optional&& optional, T&& t ) noexcept {
		return CONDITIONAL_RVALUE_AS_REF(optional, *std::forward<Optional>(optional), std::forward<T>(t));
	}

	template<typename Optional, typename Func>
	[[nodiscard]] constexpr decltype(auto) optional_or_eval( Optional&& optional, Func func ) noexcept {
		return CONDITIONAL_PRVALUE_AS_VAL(optional, *std::forward<Optional>(optional), func());
	}
}

namespace boost {
	template<typename T>
	struct range_mutable_iterator<std::optional<T>> {
		using type = T*;
	};
	template<typename T>
	struct range_const_iterator<std::optional<T>> {
		using type = T const*;
	};
}

namespace tc { 	namespace begin_end_adl {
	template<typename T>
	constexpr T* begin(begin_end_tag_t, std::optional<T>& ot) noexcept {
		return ot ? std::addressof(*ot) : nullptr;
	}
	template<typename T>
	constexpr T const* begin(begin_end_tag_t, std::optional<T> const& ot) noexcept {
		return ot ? std::addressof(*ot) : nullptr;
	}
	template<typename T>
	constexpr T* end(begin_end_tag_t, std::optional<T>& ot) noexcept {
		return ot ? std::addressof(*ot)+1 : nullptr;
	}
	template<typename T>
	constexpr T const* end(begin_end_tag_t, std::optional<T> const& ot) noexcept {
		return ot ? std::addressof(*ot)+1 : nullptr;
	}
} }
