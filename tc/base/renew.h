
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "explicit_cast_fwd.h"

#include <algorithm>
#include <typeinfo>

namespace tc {
	template<typename T >
	constexpr void assert_most_derived(T const& t) noexcept {
		if constexpr( !std::is_final<T>::value && std::is_polymorphic<T>::value ) { // The runtime check is not necessary if T is final, and not available if T is not polymorphic.
			if( !std::is_constant_evaluated() ) { // type_info::operator== is not constexpr
				_ASSERTEQUAL(typeid(t), typeid(T));
			}
		}
	}

	template< typename T >
	constexpr void dtor_static( T & t ) noexcept { // can call dtor on const&, but does not seem sensible
#ifdef _MSC_VER // Cannot call destructor on uninitialized int during constant evaluation in MSVC.
		if constexpr( !std::is_trivially_destructible<T>::value )
#endif
		{
			t.T::~T(); // Intentionally ignore dynamic type of T.
		}
#if defined(_DEBUG) && defined(TC_PRIVATE)
		if( !std::is_constant_evaluated() ) {
			tc::fill_with_dead_pattern(t);
		}
#endif
	}

	template <typename T, typename... Args> requires (0==sizeof...(Args) && std::is_trivially_default_constructible<T>::value/*value initialization*/) || tc::safely_constructible_from<T, Args&&...>
	constexpr void ctor(T& t, Args&&... args) noexcept(std::is_nothrow_constructible<T, Args&&...>::value) {
MODIFY_WARNINGS_BEGIN(((disable)(4244))) // double to float is tc::safely_constructible_from but triggers warning/error 4244 with std::construct_at.
		std::construct_at(std::addressof(t), tc_move_if_owned(args)...);
MODIFY_WARNINGS_END
	}

	template <typename T, typename... Args> requires (!((0==sizeof...(Args) && std::is_trivially_default_constructible<T>::value/*value initialization*/) || tc::safely_constructible_from<T, Args&&...>)) && tc::explicit_castable_from<T, Args&&...>
	constexpr void ctor(T& t, Args&&... args) noexcept(noexcept(tc::explicit_cast<T>(std::declval<Args>()...))) {
		if constexpr( std::is_move_constructible<T>::value ) {
			if( std::is_constant_evaluated() ) {
				tc::ctor(t, tc::explicit_cast<T>(tc_move_if_owned(args)...));
				return;
			}
		}
		// :: ensures that non-class scope operator new is used.
		// cast to void* ensures that built-in placement new is used  (18.6.1.3).
		// not using std::construct_at to guarantee copy elision.
		::new (static_cast<void*>(std::addressof(t))) T(tc::explicit_cast<T>(tc_move_if_owned(args)...)); 
	}

	namespace renew_detail {
		template <typename T, typename... Args>
		constexpr void renew(T& t, Args&&... args) noexcept(
			// If t is not guaranteed default constructed in case tc::ctor throws, call std::terminate.
			noexcept(tc::ctor(t, std::declval<Args>()...)) || !std::is_trivially_default_constructible<T>::value
		) {
			tc::assert_most_derived(t);
			tc::dtor_static(t);
			tc::ctor(t, tc_move_if_owned(args)...);
		}
	}

	template< typename T >
	void renew_default(T& t) noexcept {
		static_assert( std::is_nothrow_default_constructible<T>::value );
		tc::assert_most_derived(t);
		tc::dtor_static(t);
		::new (static_cast<void*>(std::addressof(t))) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
	}

	template< typename T > // renew_value with non-empty argument list is useful in generic code
	constexpr void renew_value(T& t) return_MAYTHROW(
		renew_detail::renew(t)
	)

	template< typename T, typename... Args >
	constexpr void renew(T& t, Args&&... args) noexcept(noexcept(renew_detail::renew(t, tc_move_if_owned(args)...)))	{
		static_assert(!std::is_trivially_default_constructible<T>::value || 0 < sizeof...(Args), "You must decide between renew_default and renew_value!");
		renew_detail::renew(t, tc_move_if_owned(args)...);
	}

	template<typename Lhs, typename... Rhs>
	constexpr void assign_explicit_cast(Lhs& lhs, Rhs&&... rhs) MAYTHROW {
		lhs=tc::explicit_cast<Lhs>(tc_move_if_owned(rhs)...);
	}
}

#define ASSIGN_BY_RENEW_IMPL(Lhs, Rhs, /*SelfAssignCheck*/...) \
	Lhs& operator=(Rhs rhs) & noexcept { \
		/*static_assert( std::convertible_to< S, T >, "assignment must correspond to implicit construction" ); */ \
		__VA_ARGS__ { \
			tc::renew(*this, tc_move_if_owned(rhs)); \
		} \
		return *this; \
	}

#define COPY_SELF_ASSIGN_CHECK(Lhs, Rhs) \
	static_assert(std::same_as<Lhs, std::remove_cvref_t<Rhs>> && std::is_lvalue_reference<Rhs>::value); \
	if(std::addressof(rhs)!=this)

// Self assignment check only need to be applied if std::remove_cvref_t<Rhs> and Lhs are the same type and Rhs is an lvalue reference, because
//   1. for rvalue references passed to the C++ library, the caller must ensure that they can be treated as temporaries, e.g., that they don't alias.
//      - https://eel.is/c++draft/res.on.arguments#note-2
//      - https://eel.is/c++draft/lib.types.movedfrom#2
//   2. values cannot alias.
//   3. for same type copy assignment, rhs must be unchanged. https://en.cppreference.com/w/cpp/named_req/CopyAssignable

// ASSIGN_BY_RENEW:  assignment which has no danger of overlapping/self assignment. We must look case by case if Lhs and Rhs are different types and may overlap.
#define ASSIGN_BY_RENEW(Lhs, Rhs) ASSIGN_BY_RENEW_IMPL(TC_FWD(Lhs), TC_FWD(Rhs), static_assert(!std::same_as<Lhs, std::remove_cvref_t<Rhs>> || !std::is_lvalue_reference<Rhs>::value);)
// COPY_ASSIGN_BY_RENEW: same type copy assignment. check against self assignment is always needed because of dtor->ctor.
#define COPY_ASSIGN_BY_RENEW(Lhs, Rhs) ASSIGN_BY_RENEW_IMPL(TC_FWD(Lhs), TC_FWD(Rhs), COPY_SELF_ASSIGN_CHECK(TC_FWD(Lhs), TC_FWD(Rhs)))

#define ASSIGN_BY_SWAP(T) \
	T& operator=( T other ) & noexcept { \
		swap( *this, other ); /*not tc::swap, which may be implemented in terms of move, which would be circular*/ \
		return *this; \
	}
