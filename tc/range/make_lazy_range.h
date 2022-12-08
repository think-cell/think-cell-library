
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/return_decltype.h"
#include "../base/invoke.h"
#include "meta.h"
#include "../algorithm/for_each.h"

namespace tc {
	namespace no_adl {
		template<typename Func>
		struct make_lazy_range final {
			static_assert(tc::decayed<Func>);
			static_assert(!std::is_rvalue_reference<decltype(std::declval<Func&&>()())>());

			constexpr explicit make_lazy_range(Func func) noexcept : m_func(tc_move(func)) {}

			template<typename Sink>
			constexpr friend decltype(auto) for_each_impl(make_lazy_range&& self, Sink&& sink) MAYTHROW {
				return tc::for_each(tc_move(self).m_func(), std::forward<Sink>(sink));
			}

			friend auto range_output_t_impl(make_lazy_range&&) noexcept -> tc::range_output_t<decltype(std::declval<Func&&>()())>; // declaration only
		private:
			Func m_func;
		};
	}
	using no_adl::make_lazy_range;
}

// MAKE_LAZY_RANGE is used to store unmovable range into our range adaptors
#define MAKE_LAZY_RANGE( ... ) tc::make_lazy_range([&](auto&&...) MAYTHROW -> decltype(auto) { return __VA_ARGS__; })
