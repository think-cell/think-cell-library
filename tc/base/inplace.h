
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "noncopyable.h"
#include "has_xxx.h"
#include "type_traits_fwd.h"

#define tc_inplace(expr) \
	(expr)=VERIFYINITIALIZED(expr)

TC_HAS_MEM_FN_XXX_TRAIT_DEF( negate, & )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( bitwise_not, & )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( logical_not, & )

namespace tc {
	namespace inplace_detail {
		template< typename T > requires has_mem_fn_negate<T>::value
		constexpr void negate(T& t) noexcept {
			t.negate();
		}
		template< typename T >
		constexpr void negate(T& t) noexcept {
			t=-t;
		}
		template< typename T >
		void negate(std::atomic<T>& t) noexcept {
			static_assert( !has_mem_fn_negate<T>::value );
			T tCurrent = t.load();
			while (!t.compare_exchange_weak(tCurrent, -tCurrent))
				{}
		}

		template< typename T > requires has_mem_fn_bitwise_not<T>::value
		constexpr void bitwise_not(T& t) noexcept {
			t.bitwise_not();
		}
		template< typename T >
		constexpr void bitwise_not(T& t) noexcept {
			t = ~t;
		}
		template< typename T >
		void bitwise_not(std::atomic<T>& t) noexcept {
			static_assert(!has_mem_fn_bitwise_not<T>::value);
			T tCurrent = t.load();
			while (!t.compare_exchange_weak(tCurrent, ~tCurrent))
				{}
		}

		template< typename T > requires has_mem_fn_logical_not<T>::value
		constexpr void logical_not(T& t) noexcept {
			t.logical_not();
		}
		template< typename T >
		constexpr void logical_not(T& t) noexcept {
			t = !t;
		}
		template< typename T >
		void logical_not(std::atomic<T>& t) noexcept {
			static_assert( !has_mem_fn_logical_not<T>::value );
			T tCurrent = t.load();
			while (!t.compare_exchange_weak(tCurrent, !tCurrent))
				{}
		}

		namespace no_adl {
			template< typename T >
			struct [[nodiscard]] inplace final : tc::noncopyable {
				constexpr inplace(T& t) noexcept:m_t(VERIFYINITIALIZED(t)){}
				constexpr void operator-() const& noexcept {
					negate(m_t); // allow ADL
				}
				constexpr void operator~() const& noexcept {
					bitwise_not(m_t); // allow ADL
				}
				constexpr void operator!() const& noexcept {
					logical_not(m_t); // allow ADL
				}
			private:
				T& m_t;
			};
		}
	};
	template< typename T >
	[[nodiscard]] constexpr auto inplace(T& t) noexcept{
		return tc::inplace_detail::no_adl::inplace<T>(t);
	}
}
