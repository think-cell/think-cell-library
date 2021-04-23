
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "return_decltype.h"
#include "invoke.h"

namespace tc {
	namespace no_adl {
		template< typename Func >
		struct make_lazy final {
		private:
			// Using decltype(std::declval<tc::decay_t<Func> const&>()()) fails in MSVC
			using result_type = std::invoke_result_t<tc::decay_t<Func> const&>;

		public:
			constexpr explicit make_lazy(Func&& func) noexcept
				: m_func(std::forward<Func>(func))
			{}
			constexpr operator result_type () const& MAYTHROW {
				return m_func(); // MAYTHROW
			}
			constexpr result_type operator()() const& MAYTHROW {
				return m_func(); // MAYTHROW
			}
		private:
			tc::decay_t<Func> m_func;
		};
		template< typename Func >
		make_lazy(Func&& func)->make_lazy<Func>;
	}
	using no_adl::make_lazy;
}

// lazy rvalues are returned by value - avoid decltype on __VA_ARGS__, because expression usually contains lambdas
#define MAKE_LAZY( ... ) tc::make_lazy([&]() MAYTHROW -> decltype(auto) { return tc::lvalue_or_decay(__VA_ARGS__); })
