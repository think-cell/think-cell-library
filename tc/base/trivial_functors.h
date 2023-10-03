
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "utility.h"

namespace tc {
	namespace no_adl {
		struct noop {
			template<typename... Args> constexpr void operator()(Args const&...) const& noexcept {}
		};

		template<typename T=void>
		struct never_called {
			template<typename... Args> T operator()(Args const&...) const& noexcept {_ASSERTFALSE; return tc::construct_default_or_terminate<T>(); }
		};

		// Use tc::constexpr_function<val> for types that are qualified for non type template parameter. Otherwise use MAKE_CONSTEXPR_FUNCTION.
		template<auto val>
		struct constexpr_function /* not final */ {
			static_assert( sizeof(val)<=sizeof(void*) );
			template< typename... Args >
			constexpr auto operator()( Args const&... ) const& noexcept { return val; }
		};

		struct identity {
			template< typename T >
			constexpr T&& operator()(T&& t) const& noexcept {
				return std::forward<T>(t);
			}
		};
	}
	using no_adl::noop;
	using no_adl::never_called;
	using no_adl::constexpr_function;
	using no_adl::identity;
}

// MAKE_CONSTEXPR_FUNCTION is guaranteed a constexpr function.
#define MAKE_CONSTEXPR_FUNCTION(...) \
	[]() constexpr noexcept { \
		/*bool_constant is needed, because otherwise MSVC doesn't correctly evaluate the `if constexpr`*/ \
		if constexpr (tc::constant<sizeof(decltype(__VA_ARGS__)) <= sizeof(void*)>::value) { \
			return [](auto&& ...) constexpr noexcept { \
				constexpr auto _ = __VA_ARGS__; \
				return _; \
			}; \
		} else { \
			return [](auto&& ...) constexpr noexcept -> decltype(auto) { \
				return tc_as_constexpr(__VA_ARGS__); \
			}; \
		} \
	}()	
