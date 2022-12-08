
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "explicit_cast_fwd.h"

#include <algorithm>

namespace tc {
	template< typename T >
	constexpr void dtor( T & t ) noexcept { // can call dtor on const&, but does not seem sensible
#ifdef _MSC_VER // Cannot call destructor on uninitialized int during constant evaluation in MSVC.
		if constexpr( !std::is_trivially_destructible<T>::value )
#endif
		{
			t.~T();
		}
#if defined(_DEBUG) && defined(TC_PRIVATE)
		if( !std::is_constant_evaluated() ) {
			tc::fill_with_dead_pattern(t);
		}
#endif
	}

	// needed in GCC
	template< typename T, std::size_t N >
	constexpr void dtor( T (&a)[N] ) noexcept { // can call dtor on const&, but does not seem sensible
		for( std::size_t i=0; i!=N; ++i) {
			dtor(a[i]);
		}
	}

	template <typename T, typename... Args> requires (0 == sizeof...(Args)) || tc::is_safely_constructible<T, Args&&...>::value
	constexpr void ctor(T& t, Args&&... args) noexcept(std::is_nothrow_constructible<T, Args&&...>::value) {
		std::construct_at(std::addressof(t), std::forward<Args>(args)...);
	}

	template <typename T, typename... Args>
	constexpr void ctor(T& t, Args&&... args) noexcept(noexcept(tc::explicit_cast<T>(std::declval<Args>()...))) {
		if constexpr( std::is_move_constructible<T>::value ) {
			if( std::is_constant_evaluated() ) {
				tc::ctor(t, tc::explicit_cast<T>(std::forward<Args>(args)...));
				return;
			}
		}
		// :: ensures that non-class scope operator new is used.
		// cast to void* ensures that built-in placement new is used  (18.6.1.3).
		// not using std::construct_at to guarantee copy elision.
		::new (static_cast<void*>(std::addressof(t))) T(tc::explicit_cast<T>(std::forward<Args>(args)...)); 
	}

	namespace renew_detail {
		template <typename T, typename... Args>
		constexpr void renew(T& t, Args&&... args) noexcept(
			// If t is not guaranteed default constructed in case tc::ctor throws, call std::terminate.
			noexcept(tc::ctor(t, std::declval<Args>()...)) || !std::is_trivially_default_constructible<T>::value
		) {
			tc::dtor(t);
			tc::ctor(t, std::forward<Args>(args)...);
		}
	}

	template< typename T >
	void renew_default(T& t) noexcept {
		static_assert( std::is_nothrow_default_constructible<T>::value );
		tc::dtor(t);
		::new (static_cast<void*>(std::addressof(t))) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
	}

	template< typename T > // renew_value with non-empty argument list is useful in generic code
	constexpr void renew_value(T& t) return_MAYTHROW(
		renew_detail::renew(t)
	)

	template< typename T, typename... Args >
	constexpr void renew(T& t, Args&&... args) noexcept(noexcept(renew_detail::renew(t, std::forward<Args>(args)...)))	{
		static_assert(!std::is_trivially_default_constructible<T>::value || 0 < sizeof...(Args), "You must decide between renew_default and renew_value!");
		renew_detail::renew(t, std::forward<Args>(args)...);
	}
}

// Sean Parent says that assignment should correspond to implicit construction, not explicit construction
// Use tc::renew to call explicit constructors, but beware of self-assignment. 

// WATCH OUT, NOT SELF-ASSIGN AWARE
#define ASSIGN_BY_RENEW( T, S ) \
	T& operator=( S s ) & noexcept \
	{ \
		static_assert( std::is_convertible< S, T >::value, "assignment must correspond to implicit construction" ); \
		/* \
		- Lvalues may alias (parts of) *this, so don't use renew. \
		- For rvalue references passed to the C++ library, the caller must ensure that they can be treated as temporaries, e.g., that they don't alias,
		  incl. for move assignment, see C++11 Standard 17.6.4.9 or http://www.open-std.org/jtc1/sc22/wg21/docs/lwg-defects.html#1204 . \
		  We adopt the same policy. \
		- Values cannot alias. */ \
		/* check for overlap of memory ranges, most general check for self-assignment I could come up with, even if it does not catch all cases, e.g., heap-allocated memory */ \
		tc::assert_no_overlap(*this, std::forward<S>(s)); \
		tc::renew( *this, std::forward<S>(s) ); \
		return *this; \
	}

#define ASSIGN_BY_SWAP(T) \
	T& operator=( T other ) & noexcept { \
		swap( *this, other ); /*not tc::swap, which may be implemented in terms of move, which would be circular*/ \
		return *this; \
	}
