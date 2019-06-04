
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "result_of.h"
#include "return_decltype.h"

namespace tc {
	namespace lazy_detail {
		namespace no_adl {
			template< typename Func >
			struct lazy_impl final {
				lazy_impl(Func&& func) noexcept
					: m_func(std::forward<Func>(func))
				{}
				operator std::invoke_result_t< tc::decay_t<Func> const& > () const& noexcept(noexcept(std::declval<tc::decay_t<Func> const&>()())) {
					return m_func();
				}
				std::invoke_result_t< tc::decay_t<Func> const& > operator()() const& noexcept(noexcept(std::declval<tc::decay_t<Func> const&>()())) {
					return m_func();
				}
			private:
				tc::decay_t<Func> m_func;
			};
		}

		template< typename Func >
		auto make_lazy(Func&& func) noexcept
			return_ctor( no_adl::lazy_impl<Func>, ( std::forward<Func>(func) ) )
	}
}

// lazy rvalues are returned by value - avoid decltype on __VA_ARGS__, because expression usually contains lambdas
#define MAKE_LAZY( ... ) tc::lazy_detail::make_lazy([&]() MAYTHROW -> decltype(auto) { return tc::lvalue_or_decay(__VA_ARGS__); })
