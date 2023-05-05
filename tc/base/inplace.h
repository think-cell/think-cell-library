
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "noncopyable.h"
#include "has_xxx.h"
#include "tc_move.h"
#include "type_traits_fwd.h"
#include <atomic>

// Defined as a macro, to preserve the short-circuiting semantics of && and ||
// Example:
//	bool b = ...;
//	tc_inplace(b) && bool_convertible_condition()
//	tc_inplace(b) || bool_convertible_condition()
#define tc_inplace(expr) \
	(expr)=VERIFYINITIALIZED(expr)


namespace tc {
	namespace inplace_adl {
		template< typename T >
		struct [[nodiscard]] inplace final : tc::noncopyable {
			constexpr explicit inplace(T& t) noexcept:m_t(VERIFYINITIALIZED(t)){}
			T& m_t;
		};
	}
	using inplace_adl::inplace;

#pragma push_macro("DEFINE_INPLACE_OPERATOR")
#define DEFINE_INPLACE_OPERATOR(op, name) \
	TC_HAS_MEM_FN_XXX_CONCEPT_DEF( name, & ) \
	\
	namespace inplace_adl { \
		template< typename T > \
		constexpr void operator op(inplace<T> t) noexcept { \
			t.m_t=op tc_move_always(t.m_t); \
		} \
		\
		template< has_mem_fn_ ## name T > \
		constexpr void operator op(inplace<T> t) noexcept { \
			t.m_t.name(); \
		} \
		\
		template< typename T > \
		void operator op(inplace<std::atomic<T>> t) noexcept { \
			static_assert( !has_mem_fn_ ## name<T> ); \
			T tCurrent = t.m_t.load(std::memory_order_relaxed); \
			while (!t.m_t.compare_exchange_weak(tCurrent, op tCurrent)) \
				{} \
		} \
	} \
	\
	template<typename T> \
	constexpr T name(T t) noexcept { \
		op tc::inplace(t); \
		return t; \
	}

	DEFINE_INPLACE_OPERATOR(-, negate)
	DEFINE_INPLACE_OPERATOR(~, bitwise_not)
	DEFINE_INPLACE_OPERATOR(!, logical_not)
#pragma pop_macro("DEFINE_INPLACE_OPERATOR")
}
