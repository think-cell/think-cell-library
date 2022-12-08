
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/conditional.h"
#include "base/utility.h"
#include "range/meta.h"

#include <optional>

namespace tc {
	template<typename Optional, typename T>
	[[nodiscard]] constexpr decltype(auto) optional_value_or( Optional&& optional, T&& t ) noexcept {
		if constexpr( tc::is_instance_or_derived<tc::make_lazy, T>::value ) {
			static_assert( !tc::has_actual_common_reference<decltype(*std::declval<Optional>()), T&&> ); // Should optional_value_or(std::optional<make_lazy<T>>(), make_lazy<T>()) return make_lazy<T>&& or T?
			return CONDITIONAL_PRVALUE_AS_VAL(optional, *std::forward<Optional>(optional), std::forward<T>(t)());
		} else {
			return CONDITIONAL_RVALUE_AS_REF(optional, *std::forward<Optional>(optional), std::forward<T>(t));
		}
	}

	template<typename Func>
	auto not_singleton_or(auto&& t, Func func) noexcept->decltype(func()) {
		if( tc::explicit_cast<bool>(t) ) {
			return tc_move_if_owned(t);
		} else {
			return func();
		}
	}

	template<typename Exception>
	decltype(auto) throw_if_null(auto&& t) THROW(Exception) {
		if( tc::explicit_cast<bool>(t) ) {
			return tc_move_if_owned(t);
		} else {
			throw Exception();
		}
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

namespace tc::begin_end_adl {
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
}
