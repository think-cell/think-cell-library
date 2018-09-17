
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"

#include <algorithm>


namespace tc {
	template< typename T >
	void dtor( T & t ) noexcept { // can call dtor on const&, but does not seem sensible
		t.~T();
#ifdef _DEBUG
		// static_cast<void*> to silence warning: destination for this 'memset' call is a pointer to dynamic class; vtable pointer will be overwritten [-Wdynamic-class-memaccess]
		std::memset( static_cast<void*>(std::addressof(t)), 0xcc, sizeof( t ) );
#endif
	}

	// needed in GCC
	template< typename T, std::size_t N >
	void dtor( T (&a)[N] ) noexcept { // can call dtor on const&, but does not seem sensible
		for( std::size_t i=0; i!=N; ++i) {
			dtor(a[i]);
		}
	}

	template< typename T >
	void renew(T& t) noexcept {
		static_assert(!std::is_trivially_default_constructible<T>::value, "You must decide between ctor_default and ctor_value!");
		tc::dtor(t);
		::new (static_cast<void*>(std::addressof(t))) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
	}

	template< typename T >
	void renew_default(T& t) noexcept {
		// static_assert( std::is_nothrow_default_constructible<T>::value );
		tc::dtor(t);
		::new (static_cast<void*>(std::addressof(t))) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
	}

	template< typename T, typename... Args > // ctor_value with non-empty argument list is useful in generic code
	void renew_value(T& t, Args&& ... args) noexcept	{
		// static_assert( std::is_nothrow_default_constructible<T>::value );
		tc::dtor(t);
		::new (static_cast<void*>(std::addressof(t))) T(std::forward<Args>(args)...); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
	}

	template<typename T, typename First, typename... Args>
	void renew(T& t, First&& first, Args&& ... args) noexcept {
		// static_assert( std::is_nothrow_constructible<T, First&&, Args&& ...>::value );
		tc::dtor(t);
		// In C++, new T(...) is direct initialization just like T t(...).
		// For non-class types, only implicit conversions are considered, so it is equivalent to T t=...
		// For class types, explicit conversions are considered, unlike for T t=...
		::new (static_cast<void*>(std::addressof(t))) T(std::forward<First>(first), std::forward<Args>(args)...); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
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
		swap( *this, other ); /*not boost::swap, which may be implemented in terms of move, which would be circular*/ \
		return *this; \
	}

