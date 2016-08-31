//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
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
		#define VERIFYEQUAL( expr, constant ) (expr)
	#endif
	#ifndef VERIFY
		#define VERIFY( expr ) (expr)
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
			}
	#endif

	#define MAYTHROW noexcept(false)

	#include <initializer_list>

	namespace tc {
		template<typename T>
		std::initializer_list<T> make_initializer_list(std::initializer_list<T> il) noexcept {
			return il;
		}
	}

	#define RANGE_UNITTEST_OUTPUT

	#define switch_no_default(...) \
	switch( auto const& /*lifetime extended until end of switch block*/ __switch=(__VA_ARGS__) ) \
	default: \
		if ( _ASSERTFALSE, false ) {std::terminate(); /*never executed, but the compiler might complain about this code path not returning a value*/} \
		else

#endif

#include "range_fwd.h"

//-----------------------------------------------------------------------------------------------------------------------------
// STATIC_ASSERT_OVERLOAD_NOT_SELECTED
//
// see equal.h for a usage example

#define STATIC_ASSERT_OVERLOAD_NOT_SELECTED(NAME, EXPR, MSG, RETURN_T, ...) \
	std::enable_if_t< EXPR,  RETURN_T> NAME(__VA_ARGS__) noexcept { \
		static_assert(!(EXPR), MSG); \
		return std::declval<RETURN_T>(); \
	}

