#pragma once

#include "Library/Utilities/noncopyable.h"
#include "Library/Utilities/has_mem_fn.h"
#include "Library/Utilities/remove_cvref.h"

#define tc_in_place(expr) \
	(expr)=(expr)

TC_HAS_MEM_FN_XXX_TRAIT_DEF( negate )

namespace tc {
	namespace in_place_adl_barrier {
		template< typename T >
		typename std::enable_if< has_mem_fn_negate<T>::value >::type negate(T& t) {
			t.negate();
		}
		template< typename T >
		typename std::enable_if< !has_mem_fn_negate<T>::value >::type negate(T& t) {
			t=-t;
		}
		template< typename T >
		struct in_place : tc::noncopyable {
			in_place(T& t):m_t(t){}
			in_place const& operator-() const {
				negate(m_t); // allow ADL
				return *this;
			}
			in_place const& operator!() const {
				m_t=!m_t;
				return *this;
			}
			in_place const& operator~() const {
				m_t=~m_t;
				return *this;
			}
		private:
			T& m_t;
		};
	};
	template< typename T >
	in_place_adl_barrier::in_place<T> in_place(T& t){
		return in_place_adl_barrier::in_place<T>(t);
	}
}