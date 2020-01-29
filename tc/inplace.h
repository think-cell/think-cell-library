
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "noncopyable.h"
#include "has_mem_fn.h"
#include "type_traits.h"

#define tc_inplace(expr) \
	(expr)=(expr)

TC_HAS_MEM_FN_XXX_TRAIT_DEF( negate, & )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( bitwise_not, & )

namespace tc {
	namespace inplace_detail {
		template< typename T, std::enable_if_t<has_mem_fn_negate<T>::value>* = nullptr>
		void negate(T& t) noexcept {
			t.negate();
		}
		template< typename T, std::enable_if_t<!has_mem_fn_negate<T>::value>* = nullptr>
		void negate(T& t) noexcept {
			t=-t;
		}
		template< typename T, std::enable_if_t<has_mem_fn_bitwise_not<T>::value>* = nullptr>
		void bitwise_not(T& t) noexcept {
			t.bitwise_not();
		}
		template< typename T, std::enable_if_t<!has_mem_fn_bitwise_not<T>::value>* = nullptr>
		void bitwise_not(T& t) noexcept {
			t = ~t;
		}
		namespace no_adl {
			template< typename T >
			struct [[nodiscard]] inplace final : tc::noncopyable {
				inplace(T& t) noexcept:m_t(VERIFYINITIALIZED(t)){}
				inplace const& operator-() const& noexcept {
					negate(m_t); // allow ADL
					return *this;
				}
				inplace const& operator~() const& noexcept {
					bitwise_not(m_t); // allow ADL
					return *this;
				}
				inplace const& operator!() const& noexcept {
					m_t=!m_t;
					return *this;
				}
			private:
				T& m_t;
			};
		}
	};
	template< typename T >
	auto inplace(T& t) noexcept{
		return tc::inplace_detail::no_adl::inplace<T>(t);
	}
}