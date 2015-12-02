#pragma once

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
	#include "assert_fwd.h"
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
		void ctor() {
			// This check is not strict enough. The following struct is !std::is_trivially_default_constructible,
			// but ctor_default does not initialize n to 0, while ctor_value does:
			//	struct Foo {
			//		std::string s; // has user-defined default ctor
			//		int n; // has no user-defined default ctor
			//	};
			static_assert(!std::is_trivially_default_constructible<T>::value, "You must decide between ctor_default and ctor_value!");
			::new (static_cast<void*>(&m_buffer)) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		void ctor_default() {
			::new (static_cast<void*>(&m_buffer)) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		void ctor_value() {
			::new (static_cast<void*>(&m_buffer)) T(); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		template<typename First, typename... Args>
		void ctor(First&& first, Args&& ... args) {
			// In C++, new T(...) is direct initialization just like T t(...).
			// For non-class types, only implicit conversions are considered, so it is equivalent to T t=...
			// For class types, explicit conversions are considered, unlike for T t=...
			::new (static_cast<void*>(&m_buffer)) T(std::forward<First>(first), std::forward<Args>(args)...); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
		}
		operator T const&() const noexcept {
			static_assert( sizeof(*this)==sizeof(T), "" ); // no extra members, important for arrays
			return reinterpret_cast<T const&>(m_buffer);
		}
		operator T &() noexcept {
			return tc::make_mutable(boost::implicit_cast<T const&>(tc::make_const(*this)));
		}
		T const* operator->() const noexcept {
			return std::addressof(boost::implicit_cast<T const&>(*this));
		}
		T* operator->() noexcept {
			return std::addressof(boost::implicit_cast<T&>(*this));
		}
		T const& operator*() const noexcept {
			return boost::implicit_cast<T const&>(*this);
		}
		T& operator*() noexcept {
			return boost::implicit_cast<T&>(*this);
		}
		void dtor() const noexcept {
			boost::implicit_cast<T const&>(*this).~T();
		}
	};

	template< typename T >
	struct storage_for : storage_for_without_dtor <T> {
#if defined(_CHECKS) && !defined(RANGE_PROPOSAL_BUILD_STANDALONE)
		storage_for() {
			tc::fill_with_dead_pattern(this->m_buffer);
		}
		~storage_for() noexcept {
			check_pattern();
		}
		void ctor() {
			check_pattern();
			try {
				storage_for_without_dtor <T>::ctor();
			} catch (...) {
				tc::fill_with_dead_pattern(this->m_buffer);
				throw;
			}
		}
		void ctor_default() {
			check_pattern();
			try {
				storage_for_without_dtor <T>::ctor_default();
			}	catch (...) {
				tc::fill_with_dead_pattern(this->m_buffer);
				throw;
			}
		}
		void ctor_value() {
			check_pattern();
			try {
				storage_for_without_dtor <T>::ctor_value();
			} catch (...) {
				tc::fill_with_dead_pattern(this->m_buffer);
				throw;
			}
		}
		template<typename First, typename... Args>
		void ctor(First&& first, Args&& ... args) {
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
			tc::fill_with_dead_pattern(tc::make_mutable(this->m_buffer));
		}

	private:
		void check_pattern() const noexcept {
			tc::assert_dead_pattern(this->m_buffer);
		}
#endif
	};
}