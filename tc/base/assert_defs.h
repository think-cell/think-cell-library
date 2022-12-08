
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#ifdef TC_PRIVATE
	#include "Library/ErrorReporting/assert_fwd.h"
#else
	#include "fundamental.h"
	#include <type_traits>
	#include <cstdlib>
	#include <cassert>

	#define _CHECKS
	#ifndef IF_TC_CHECKS
		#define IF_TC_CHECKS(...) __VA_ARGS__
	#endif
	#ifndef IF_TC_DEBUG
		#ifdef _DEBUG
			#define IF_TC_DEBUG(...) __VA_ARGS__
		#else
			#define IF_TC_DEBUG(...)
		#endif
	#endif
	#ifndef _ASSERT
		#include <cassert>
		#define _ASSERT(...) assert(TC_FWD(__VA_ARGS__))
	#endif
	#ifndef TRYASSERT
		#define TRYASSERT _ASSERT
	#endif
	#ifndef _ASSERTE
		#define _ASSERTE(...) (static_cast<void>(0))
	#endif
	#ifndef _ASSERTDEBUG
		#define _ASSERTDEBUG(...) IF_TC_DEBUG(_ASSERT((__VA_ARGS__)))
	#endif
	#ifndef _ASSERTFALSE
		#define _ASSERTFALSE _ASSERT(false)
	#endif
	#ifndef _ASSERTNORETURN
		#define _ASSERTNORETURN _ASSERT
	#endif
	#ifndef _ASSERTNOTIFY
		#define _ASSERTNOTIFY _ASSERT
	#endif
	#ifndef _ASSERTNOTIFYFALSE
		#define _ASSERTNOTIFYFALSE _ASSERTFALSE
	#endif
	#ifndef _ASSERTENOTIFY
		#define _ASSERTENOTIFY _ASSERTE
	#endif
	#ifndef _ASSERTEQUAL
		#define _ASSERTEQUAL(a, b) assert((a)==(b))
	#endif
	#ifndef _ASSERTDEBUGEQUAL
		#define _ASSERTDEBUGEQUAL(a, b) IF_TC_DEBUG(_ASSERTEQUAL(a, b))
	#endif
	#ifndef _ASSERTANYOF
		#include <boost/preprocessor/seq/enum.hpp>
		#define _ASSERTDEBUGANYOF(expr, values) [](auto const& e, auto const&... val) noexcept { assert( ((e == val) || ...) ); }(expr, BOOST_PP_SEQ_ENUM(values))
	#endif
	#ifndef _ASSERTDEBUGANYOF
		#define _ASSERTDEBUGANYOF(expr, values) IF_TC_DEBUG(_ASSERTANYOF(expr, values))
	#endif
	#ifndef _ASSERTINITIALIZED
		#define _ASSERTINITIALIZED( expr ) tc::discard(expr)
	#endif
	#ifndef _ASSERTPRINT
		#define _ASSERTPRINT( cond, ... ) assert( cond )
	#endif
	#ifndef VERIFYEQUAL
		#include <utility>
		namespace ErrorHandling {
			template <typename Expr, typename Const>
			constexpr Expr&& VerifyEqual(Expr&& expr, Const const& c) {
				_ASSERTEQUAL(expr, c);
				return std::forward<Expr>(expr);
			}
		}
		#define VERIFYEQUAL( expr, constant ) ErrorHandling::VerifyEqual(expr, constant)
	#endif
	#ifndef VERIFYEQUALNOPRINT
		#define VERIFYEQUALNOPRINT VERIFYEQUAL
	#endif
	#ifndef VERIFY
		#include <utility>
		namespace ErrorHandling {
			template <typename Expr>
			constexpr Expr&& Verify(Expr&& expr) {
				_ASSERT(expr);
				return std::forward<Expr>(expr);
			}
		}
		#define VERIFY( expr ) ErrorHandling::Verify(expr)
	#endif
	#ifndef VERIFYPRED
		#include <utility>
		namespace ErrorHandling {
			template <typename Expr, typename Pred>
			constexpr Expr&& VerifyPred(Expr&& expr, Pred pred) {
				_ASSERT(pred(expr));
				return std::forward<Expr>(expr);
			}
		}
		#define VERIFYPRED( expr, ... ) ErrorHandling::VerifyPred(expr, [&](auto const& _) { return (__VA_ARGS__); })
	#endif
	#ifndef VERIFYINITIALIZED
		#define VERIFYINITIALIZED( expr ) (expr)
	#endif
	#ifndef VERIFYNOTIFYEQUAL
		#define VERIFYNOTIFYEQUAL VERIFYEQUAL
	#endif
	#ifndef VERIFYNOTIFY
		#define VERIFYNOTIFY VERIFY
	#endif
	#ifndef VERIFYNORETURN
		#define VERIFYNORETURN VERIFY
	#endif
	#ifndef VERIFYCRITICALPRED
		#define VERIFYCRITICALPRED VERIFYPRED
	#endif
	#ifndef VERIFYNOTIFYPRED
		#define VERIFYNOTIFYPRED VERIFYPRED
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
	#ifndef NOEXCEPT_NO_LAMBDA
		#define NOEXCEPT_NO_LAMBDA( ... ) (__VA_ARGS__)
	#endif

	#define switch_no_default(...) \
	switch( auto const& /*lifetime extended until end of switch block*/ __switch=(__VA_ARGS__) ) \
	default: \
		if ( _ASSERTFALSE, false ) {std::abort(); /*never executed, but the compiler might complain about this code path not returning a value*/} \
		else

	namespace tc {
		template<typename T>
		T construct_default_or_terminate() noexcept {
			if constexpr( !std::is_void<T>::value ) {
				if constexpr( std::is_default_constructible<T>::value ) {
					try {
						return T();
					} catch ( ... ) {}
				}
				std::abort(); // same behavior as violated noexcept
			}
		}
	}
#endif
