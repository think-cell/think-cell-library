#pragma once

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
   #include "assert_fwd.h"
#endif
#include "Library/Utilities/is_constructible.h"

#include <boost/implicit_cast.hpp>

#include <type_traits>

namespace tc {

	template< typename T >
	class storage_for {
		static_assert( std::is_same< typename std::remove_const<T>::type, typename std::decay<T>::type >::value, "only const allowed as qualification" );
		typename std::aligned_storage<sizeof(T),std::alignment_of<T>::value>::type m_buffer;
	public:
		typename std::remove_const<T>::type* uninitialized_addressof() {
			return reinterpret_cast<typename std::remove_const<T>::type*>(&m_buffer);
		}
		void ctor() {
			// This check is not strict enough. The following struct is !std::is_trivially_default_constructible,
			// but ctor_default does not initialize n to 0, while ctor_value does:
			//	struct Foo {
			//		std::string n; // has user-defined default ctor
			//		int n; // has no user-defined default ctor
			//	};
			static_assert(!tc::is_trivially_default_constructible<T>::value, "You must decide between ctor_default and ctor_value!");
			new (&m_buffer) T;
		}
		void ctor_default() {
			new (&m_buffer) T;
		}
		void ctor_value() {
			new (&m_buffer) T();
		}
		template<typename First, typename... Args>
		void ctor(First&& first, Args&&... args) {
			new (&m_buffer) T(std::forward<First>(first),std::forward<Args>(args)...);
		}
		operator T const&() const {
			static_assert( sizeof(*this)==sizeof(T), "" ); // no extra members, important for arrays
			return reinterpret_cast<T const&>(m_buffer);
		}
		operator T &() {
			return tc::make_mutable(boost::implicit_cast<T const&>(tc::make_const(*this)));
		}
		T const* operator->() const {
			return std::addressof(boost::implicit_cast<T const&>(*this));
		}
		T* operator->() {
			return std::addressof(boost::implicit_cast<T&>(*this));
		}
		void dtor() const {
			boost::implicit_cast<T const&>(*this).~T();
		}
	};
}