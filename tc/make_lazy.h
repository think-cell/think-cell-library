
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "return_decltype.h"
#include "invoke.h"

namespace tc {
	namespace lazy_detail {
		namespace no_adl {
			template< typename Func >
			struct lazy_impl final {
			private:
				// Using decltype(std::declval<tc::decay_t<Func> const&>()()) fails in MSVC
				using result_type = std::invoke_result_t<tc::decay_t<Func> const&>;

			public:
				lazy_impl(Func&& func) noexcept
					: m_func(std::forward<Func>(func))
				{}
				operator result_type () const& MAYTHROW {
					return m_func(); // MAYTHROW
				}
				result_type operator()() const& MAYTHROW {
					return m_func(); // MAYTHROW
				}
			private:
				tc::decay_t<Func> m_func;
			};
		}

		template< typename Func >
		[[nodiscard]] auto make_lazy(Func&& func)
			return_ctor_noexcept( no_adl::lazy_impl<Func>, ( std::forward<Func>(func) ) )
	}
}

// lazy rvalues are returned by value - avoid decltype on __VA_ARGS__, because expression usually contains lambdas
#define MAKE_LAZY( ... ) tc::lazy_detail::make_lazy([&]() MAYTHROW -> decltype(auto) { return tc::lvalue_or_decay(__VA_ARGS__); })
