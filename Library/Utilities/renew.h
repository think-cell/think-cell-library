#pragma once

#include "perfect_forward.h"
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
		static_assert( !std::is_trivially_default_constructible<T>::value, "default initialization is noop" );
		tc::dtor(t);
		new (std::addressof(t)) T; // default initialization http://en.cppreference.com/w/cpp/language/default_initialization
	}

	template< typename T >
	void renew_zero(T& t) {
		static_assert( std::is_trivially_default_constructible<T>::value, "zero initialization is the same as default initialization" );
		tc::dtor(t);
		new (std::addressof(t)) T(); // zero initialization http://en.cppreference.com/w/cpp/language/zero_initialization 
	}

	#define PART1() \
		template<typename T, 
	#define PART2() \
		> void renew( T& t,
	#define PART3() ) { \
			tc::dtor(t); \
			new (std::addressof(t)) T(
	#define PART4() ); \
		}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4
}

// WATCH OUT, NOT SELF-ASSIGN AWARE
#define ASSIGN_BY_RENEW( T, S ) \
	T& operator=( S s ) { \
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

