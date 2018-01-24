//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program.
// If not, see <http://www.gnu.org/licenses/>.
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#ifdef TC_PRIVATE
	#include "Library/ErrorReporting/assert_fwd.h"
#else
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
	#ifndef _ASSERTINITIALIZED
		#define _ASSERTINITIALIZED( expr ) (expr)
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

		template<typename T, std::enable_if_t<std::is_reference<T>::value>* =nullptr >
		T construct_default_or_terminate() noexcept {
			return static_cast<T>(*boost::implicit_cast<std::remove_reference_t<T>*>(nullptr));
		}

		template<typename T, std::enable_if_t<!std::is_void<T>::value && !std::is_default_constructible<T>::value && !std::is_reference<T>::value>* =nullptr>
		T construct_default_or_terminate() noexcept {
			std::abort(); // like violated noexcept
		}
	}
#endif
