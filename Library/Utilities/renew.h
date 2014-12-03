#pragma once

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
	#include "Library/ErrorReporting/assert_fwd.h"
#endif
#include "Library/Utilities/is_constructible.h"

#include <algorithm>

namespace tc {
	template< typename T >
	void dtor( T & t ){ // can call dtor on const&, but does not seem sensible
		t.~T();
#ifdef _DEBUG
		memset( std::addressof(t), 0xcc, sizeof( t ) );
#endif
	}

	// needed in GCC
	template< typename T, std::size_t N >
	void dtor( T (&a)[N] ){ // can call dtor on const&, but does not seem sensible
		for( std::size_t i=0; i!=N; ++i) {
			dtor(a[i]);
		}
	}

	template< typename T >
	void renew(T& t) {
		// This check is not strict enough. The following struct is !std::is_trivially_default_constructible,
		// but ctor_default does not initialize n to 0, while ctor_value does:
		//	struct Foo {
		//		std::string n; // has user-defined default ctor
		//		int n; // has no user-defined default ctor
		//	};
		static_assert(!tc::is_trivially_default_constructible<T>::value, "You must decide between ctor_default and ctor_value!");
		tc::dtor(t);
		new (std::addressof(t)) T;
	}

	template< typename T >
	void renew_default(T& t) {
		tc::dtor(t);
		new (std::addressof(t)) T;
	}

	template< typename T >
	void renew_value(T& t) {
		tc::dtor(t);
		new (std::addressof(t)) T();
	}

	template<typename T, typename First, typename... Args>
	T& renew( T& t,First&& first,Args&&... args ) {
		tc::dtor(t);
		new (std::addressof(t)) T(std::forward<First>(first),std::forward<Args>(args)...);
		return t;
	}
}

// Sean Parent says that assignment should correspond to implicit construction, not explicit construction
// Use tc::renew to call explicit constructors, but beware of self-assignment. 

// WATCH OUT, NOT SELF-ASSIGN AWARE
#define ASSIGN_BY_RENEW( T, S ) \
	T& operator=( S s ) { \
		static_assert( std::is_convertible< S, T >::value, "assignment must correspond to implicit construction" ); \
		/* \
		- Lvalues may alias (parts of) *this, so don't use renew. \
		- For rvalue references passed to the C++ library, the caller must ensure that they can be treated as temporaries, e.g., that they don't alias,
		  incl. for move assignment, see C++11 Standard 17.6.4.9 or http://www.open-std.org/jtc1/sc22/wg21/docs/lwg-defects.html#1204 . \
		  We adopt the same policy. \
		- Values cannot alias. */ \
		/* check for overlap of memory ranges, most general check for self-assignment I could come up with, even if it does not catch all cases, e.g., heap-allocated memory */ \
		_ASSERT( std::min( \
			reinterpret_cast<std::size_t>(this)+sizeof(*this), \
			reinterpret_cast<std::size_t>(std::addressof(s))+sizeof(s) \
		)<=std::max( \
			reinterpret_cast<std::size_t>(this), \
			reinterpret_cast<std::size_t>(std::addressof(s)) \
		) ); \
		tc::renew( *this, std::forward<S>(s) ); \
		return *this; \
	}

#define ASSIGN_BY_SWAP(T) \
	T& operator=( T other ) { \
		swap( *this, other ); /*not boost::swap, which may be implemented in terms of move, which would be circular*/ \
		return *this; \
	}

