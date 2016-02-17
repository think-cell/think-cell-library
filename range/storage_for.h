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
#endif

#include <boost/implicit_cast.hpp>

#include <type_traits>

namespace tc {

	template< typename T >
	struct storage_for_without_dtor : tc::nonmovable {
	protected:
		static_assert( tc::is_decayed< std::remove_const_t<T> >::value, "only const allowed as qualification" );
		std::aligned_storage_t<sizeof(T),std::alignment_of<T>::value> m_buffer;
	public:
		std::remove_const_t<T>* uninitialized_addressof() noexcept {
			return reinterpret_cast< std::remove_const_t<T>*>(&m_buffer);
		}
		void ctor() noexcept {
			// This check is not strict enough. The following struct is !std::is_trivially_default_constructible,
			// but ctor_default does not initialize n to 0, while ctor_value does:
			//	struct Foo {
			//		std::string s; // has user-defined default ctor
			//		int n; // has no user-defined default ctor
			//	};
			static_assert(!std::is_trivially_default_constructible<T>::value, "You must decide between ctor_default and ctor_value!");
			::new (static_cast<void*>(&m_buffer)) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		void ctor_default() noexcept {
			::new (static_cast<void*>(&m_buffer)) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		template<typename... Args> // ctor_value with non-empty argument list is useful in generic code
		void ctor_value(Args&& ... args) noexcept {
			::new (static_cast<void*>(&m_buffer)) T(std::forward<Args>(args)...); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		template<typename First, typename... Args>
		void ctor(First&& first, Args&& ... args) noexcept {
			// In C++, new T(...) is direct initialization just like T t(...).
			// For non-class types, only implicit conversions are considered, so it is equivalent to T t=...
			// For class types, explicit conversions are considered, unlike for T t=...
			::new (static_cast<void*>(&m_buffer)) T(std::forward<First>(first), std::forward<Args>(args)...); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		T const* operator->() const noexcept {
			return std::addressof(**this);
		}
		T* operator->() noexcept {
			return std::addressof(**this);
		}
		T const& operator*() const noexcept {
			static_assert( sizeof(*this)==sizeof(T), "" ); // no extra members, important for arrays
			return reinterpret_cast<T const&>(m_buffer);
		}
		T& operator*() noexcept {
			return  tc::as_mutable(*(tc::as_const(*this)));
		}
		void dtor() const noexcept {
			(**this).~T();
		}
	};

	template< typename T >
	struct storage_for : storage_for_without_dtor <T> {
#if defined(_DEBUG) && !defined(RANGE_PROPOSAL_BUILD_STANDALONE)
		storage_for() noexcept {
			tc::fill_with_dead_pattern(this->m_buffer);
		}
		~storage_for() noexcept {
			check_pattern();
		}
		void ctor() noexcept(noexcept(storage_for_without_dtor <T>::ctor())) {
			check_pattern();
			try {
				storage_for_without_dtor <T>::ctor();
			} catch (...) {
				tc::fill_with_dead_pattern(this->m_buffer);
				throw;
			}
		}
		void ctor_default() noexcept(noexcept(storage_for_without_dtor <T>::ctor_default())) {
			check_pattern();
			try {
				storage_for_without_dtor <T>::ctor_default();
			}	catch (...) {
				tc::fill_with_dead_pattern(this->m_buffer);
				throw;
			}
		}
		template<typename... Args> // ctor_value with non-empty argument list is useful in generic code
		void ctor_value(Args&& ... args) noexcept(noexcept(storage_for_without_dtor <T>::ctor_value(std::forward<Args>(args)...))) {
			check_pattern();
			try {
				storage_for_without_dtor <T>::ctor_value(std::forward<Args>(args)...);
			} catch (...) {
				tc::fill_with_dead_pattern(this->m_buffer);
				throw;
			}
		}
		template<typename First, typename... Args>
		void ctor(First&& first, Args&& ... args) noexcept(noexcept(storage_for_without_dtor <T>::ctor(std::forward<First>(first), std::forward<Args>(args)...))) {
			check_pattern();
			try {
				storage_for_without_dtor <T>::ctor(std::forward<First>(first), std::forward<Args>(args)...);
			} catch (...) {
				tc::fill_with_dead_pattern(this->m_buffer);
				throw;
			}
		}
		void dtor() const noexcept {
			storage_for_without_dtor <T>::dtor();
			tc::fill_with_dead_pattern(tc::as_mutable(this->m_buffer));
		}

	private:
		void check_pattern() const noexcept {
			// RT#12004: g_mtxSharedHeap's destructor fails this check in the Excel insider build 16.0.6568.2036
			// because this version does not realease all its locks on CTCAddInModule so that CSharedHeap::ShutDown()
			// and thus g_mtxSharedHeap.dtor() are never called. This build appears to be broken. It throws and does
			// not handle 0xC0000008 (An invalid handle was specified) while closing even without think-cell installed.
			// I could not reproduce the error in the next Excel insider build 16.0.6701.1008. -Edgar 2016-03-14
			tc::assert_dead_pattern(this->m_buffer);
		}
#endif
	};


	template< typename T >
	struct storage_for_without_dtor<T&> : tc::nonmovable {
	protected:
		static_assert(!std::is_reference<T>::value, "");
		T* m_pt;
	public:
		void ctor(T& t) noexcept {
			m_pt=std::addressof(t);
		}

		// referenece semantics == no deep constness
		T* operator->() const noexcept {
			return m_pt;
		}
		T& operator*() const noexcept {
			return *m_pt;
		}
		void dtor() const noexcept {}
	};

	template< typename T >
	struct storage_for<T&> : storage_for_without_dtor <T&> {
#if defined(_DEBUG) && !defined(RANGE_PROPOSAL_BUILD_STANDALONE)
		storage_for() noexcept {
			this->m_pt=nullptr;
		}
		~storage_for() noexcept {
			_ASSERTDEBUG(!this->m_pt);
		}
		void ctor(T& t) noexcept {
			_ASSERTDEBUG(!this->m_pt);
			storage_for_without_dtor <T&>::ctor(t);
		}
		void dtor() noexcept {
			_ASSERTDEBUG(this->m_pt);
			storage_for_without_dtor <T&>::dtor();
			this->m_pt=nullptr;
		}
#endif
	};
}