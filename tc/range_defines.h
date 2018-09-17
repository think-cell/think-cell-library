
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#ifdef TC_PRIVATE
	#include "Library/ErrorReporting/assert_fwd.h"
#else
	#include <type_traits>
	#include <cstdlib>
	#include <cassert>

	#define _CHECKS
	#ifndef _ASSERT
		#include <cassert>
		#define _ASSERT(...) assert((__VA_ARGS__))
	#endif
	#ifndef _ASSERTE
		#define _ASSERTE(...) (static_cast<void>(0))
	#endif
	#ifndef _ASSERTDEBUG
		#define _ASSERTDEBUG(...) _ASSERT((__VA_ARGS__))
	#endif
	#ifndef _ASSERTFALSE
		#define _ASSERTFALSE _ASSERT(false)
	#endif
	#ifndef _ASSERTNOTIFY
		#define _ASSERTNOTIFY(...) _ASSERT((__VA_ARGS__))
	#endif
	#ifndef _ASSERTNOTIFYFALSE
		#define _ASSERTNOTIFYFALSE _ASSERTFALSE
	#endif
	#ifndef _ASSERTEQUAL
		#define _ASSERTEQUAL(a, b) assert((a)==(b))
	#endif
	#ifndef _ASSERTEQUALDEBUG
		#define _ASSERTEQUALDEBUG(a, b) _ASSERTEQUAL(a, b)
	#endif
	#ifndef _ASSERTINITIALIZED
		#define _ASSERTINITIALIZED( expr ) static_cast<void>(expr)
	#endif
	#ifndef _ASSERTPRINT
		#define _ASSERTPRINT( cond, ... ) assert( cond )
	#endif
	#ifndef VERIFYEQUAL
		#include <utility>
		namespace ErrorHandling {
			template <typename Expr, typename Const>
			Expr&& VerifyEqual(Expr&& expr, Const const& c) {
				_ASSERTEQUAL(expr, c);
				return std::forward<Expr>(expr);
			}
		}
		#define VERIFYEQUAL( expr, constant ) ErrorHandling::VerifyEqual(expr, constant)
	#endif
	#ifndef VERIFY
		#include <utility>
		namespace ErrorHandling {
			template <typename Expr>
			Expr&& Verify(Expr&& expr) {
				_ASSERT(expr);
				return std::forward<Expr>(expr);
			}
		}
		#define VERIFY( expr ) ErrorHandling::Verify(expr)
	#endif
	#ifndef VERIFYINITIALIZED
		#define VERIFYINITIALIZED( expr ) (expr)
	#endif
	#ifndef VERIFYNOTIFY
		#define VERIFYNOTIFY VERIFY
	#endif
	#ifndef NOBADALLOC
		#define NOBADALLOC( expr ) (expr)
	#endif
	#ifndef NOEXCEPT
		#define NOEXCEPT( ... ) \
			[&]() noexcept -> decltype(auto) { \
				return (__VA_ARGS__); \
			}()
	#endif

	#define THROW(...) noexcept(false)

	#define MAYTHROW noexcept(false)

	#define switch_no_default(...) \
	switch( auto const& /*lifetime extended until end of switch block*/ __switch=(__VA_ARGS__) ) \
	default: \
		if ( _ASSERTFALSE, false ) {std::abort(); /*never executed, but the compiler might complain about this code path not returning a value*/} \
		else

	namespace tc {
		template<typename T, std::enable_if_t<std::is_void<T>::value>* =nullptr >
		T construct_default_or_terminate() noexcept {}

		template<typename T, std::enable_if_t<std::is_default_constructible<T>::value>* =nullptr >
		T construct_default_or_terminate() noexcept {
			try {
				return T();
			} catch ( ... ) {
				std::abort(); // same behavior as violated noexcept
			}
		}

		template<typename T, std::enable_if_t<!std::is_void<T>::value && !std::is_default_constructible<T>::value>* =nullptr>
		T construct_default_or_terminate() noexcept {
			std::abort(); // same behavior as violated noexcept
		}
	}

	namespace tc {
		using char16 = std::conditional_t<2==sizeof(wchar_t), wchar_t, char16_t>;
	}

#endif
