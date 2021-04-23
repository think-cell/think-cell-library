
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "noncopyable.h"
#include "type_traits.h"
#include "casts.h"
#include "static_polymorphism.h"

#include <type_traits>

MODIFY_WARNINGS_BEGIN(((disable)(4297))) // 'function' : function assumed not to throw an exception but does.

namespace tc {
	namespace no_adl {

		template< typename Derived, typename T >
		struct [[nodiscard]] storage_for_base : tc::nonmovable {
			static constexpr bool c_bIsTrivial=std::is_trivially_default_constructible<T>::value && std::is_trivially_destructible<T>::value && std::is_trivially_assignable<T, T&&>::value;
			using this_type = storage_for_base<Derived, T>;
		protected:
			static_assert( tc::is_decayed< std::remove_const_t<T> >::value, "only const allowed as qualification" );
			std::conditional_t<
				c_bIsTrivial,
				T, // constexpr friendly storage for trivial types, no placement new 
				std::aligned_storage_t<sizeof(T),std::alignment_of<T>::value>
			> m_buffer;
		public:
			static Derived& from_content(T& t) noexcept {
				STATICASSERTEQUAL( sizeof(storage_for_base), sizeof(T) );
				return *tc::derived_cast<Derived>(reinterpret_cast<storage_for_base*>(std::addressof(t)));
			}
			template <typename = std::enable_if_t<!std::is_const<T>::value>>
			static Derived const& from_content(T const& t) noexcept {
				STATICASSERTEQUAL( sizeof(storage_for_base), sizeof(T) );
				return *tc::derived_cast<Derived>(reinterpret_cast<storage_for_base const*>(std::addressof(t)));
			}
			std::remove_const_t<T>* uninitialized_addressof() & noexcept {
				return reinterpret_cast< std::remove_const_t<T>*>(&m_buffer);
			}
			T const* uninitialized_addressof() const& noexcept {
				return reinterpret_cast<T const*>(&m_buffer);
			}
			void ctor() & noexcept(std::is_nothrow_default_constructible<T>::value) {
				// This check is not strict enough. The following struct is !std::is_trivially_default_constructible,
				// but ctor_default does not initialize n to 0, while ctor_value does:
				//	struct Foo {
				//		std::basic_string<char> s; // has user-defined default ctor
				//		int n; // has no user-defined default ctor
				//	};
				static_assert(!std::is_trivially_default_constructible<T>::value, "You must decide between ctor_default and ctor_value!");
				::new (static_cast<void*>(&m_buffer)) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
			}
			void ctor_default() & noexcept(std::is_nothrow_default_constructible<T>::value) {
				if constexpr (!c_bIsTrivial) {
					::new (static_cast<void*>(&m_buffer)) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
				} // else do nothing, already default initialized
			}
			template<typename... Args> // ctor_value with non-empty argument list is useful in generic code
			void ctor_value(Args&& ... args) & noexcept(std::is_nothrow_constructible<T, Args&&...>::value) {
				if constexpr (c_bIsTrivial) {
					m_buffer=T(std::forward<Args>(args)...);
				} else {
					::new (static_cast<void*>(&m_buffer)) T(std::forward<Args>(args)...); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
				}
			}
			template<typename First, typename... Args>
			void ctor(First&& first, Args&& ... args) & noexcept(std::is_nothrow_constructible<T, First&&, Args&&...>::value) {
				// In C++, new T(...) is direct initialization just like T t(...).
				// For non-class types, only implicit conversions are considered, so it is equivalent to T t=...
				// For class types, explicit conversions are considered, unlike for T t=...
				if constexpr (c_bIsTrivial) {
					m_buffer=T(std::forward<First>(first), std::forward<Args>(args)...);
				} else {
					::new (static_cast<void*>(&m_buffer)) T(std::forward<First>(first), std::forward<Args>(args)...); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
				}
			}
			T const* operator->() const& noexcept {
				return std::addressof(dereference());
			}
			T* operator->() & noexcept {
				return std::addressof(tc::as_mutable(dereference()));
			}
			T const& operator*() const& noexcept {
				return dereference();
			}
			T& operator*() & noexcept {
				return tc::as_mutable(dereference());
			}
			T&& operator*() && noexcept {
				return tc_move_always(tc::as_mutable(dereference()));
			}
			void dtor() & noexcept {
				if constexpr (!c_bIsTrivial) {
					(**this).T::~T();
				}
			}
		protected:
			STATIC_VIRTUAL(dereference);
			STATIC_OVERRIDE(dereference)() const& -> T const& {
				if constexpr (c_bIsTrivial) {
					return m_buffer;
				} else {
					STATICASSERTEQUAL(sizeof(*this), sizeof(T)); // no extra members, important for arrays
					return reinterpret_cast<T const&>(m_buffer);
				}
			}
		};

		template< typename Derived, typename T >
		struct [[nodiscard]] storage_for_base<Derived, T&> : tc::nonmovable {
		protected:
			static_assert(!std::is_reference<T>::value);
			using this_type = storage_for_base<Derived, T&>;
			T* m_buffer;

			STATIC_VIRTUAL(dereference);
			STATIC_OVERRIDE(dereference)() const& -> T& {
				return *m_buffer;
			}
		public:
			void ctor(T& t) & noexcept {
				m_buffer = std::addressof(t);
			}

			// reference semantics == no deep constness
			T* operator->() const& noexcept {
				return std::addressof(dereference());
			}
			T& operator*() const& noexcept {
				return dereference();
			}
			void dtor() & noexcept {}
		};

		template< typename Derived, typename T >
		struct [[nodiscard]] storage_for_base<Derived, T&&> : tc::nonmovable {
		protected:
			static_assert(!std::is_reference<T>::value);
			using this_type = storage_for_base<Derived, T&&>;
			T* m_buffer;

			STATIC_VIRTUAL(dereference);
			STATIC_OVERRIDE(dereference)() const& -> T&& {
				return static_cast<T&&>(*m_buffer);
			}
		public:
			void ctor(T&& t) & noexcept {
				m_buffer = std::addressof(t);
			}

			T&& operator*() && noexcept {
				return dereference();
			}
			void dtor() & noexcept {}
		};

		template< typename T >
		struct [[nodiscard]] storage_for : storage_for_base<storage_for<T>, T> {
			using this_type = storage_for<T>;
			using Base = storage_for_base<storage_for<T>, T>;
			
#if defined(_DEBUG) && defined(TC_PRIVATE)
			storage_for() noexcept {
				tc::fill_with_dead_pattern(this->m_buffer);
			}
			~storage_for() noexcept {
				check_dead_pattern();
			}

			void ctor() & noexcept(noexcept(Base::ctor())) {
				check_dead_pattern();
				tc::remove_dead_pattern(this->m_buffer);
				try {
					Base::ctor();
				} catch (...) {
					tc::fill_with_dead_pattern(this->m_buffer);
					throw;
				}
			}
			void ctor_default() & noexcept(noexcept(Base::ctor_default())) {
				check_dead_pattern();
				tc::remove_dead_pattern(this->m_buffer);
				try {
					Base::ctor_default();
				}	catch (...) {
					tc::fill_with_dead_pattern(this->m_buffer);
					throw;
				}
			}
			template<typename... Args> // ctor_value with non-empty argument list is useful in generic code
			void ctor_value(Args&& ... args) & noexcept(noexcept(std::declval<Base&>().ctor_value(std::forward<Args>(args)...))) {
				check_dead_pattern();
				tc::remove_dead_pattern(this->m_buffer);
				try {
					Base::ctor_value(std::forward<Args>(args)...);
				} catch (...) {
					tc::fill_with_dead_pattern(this->m_buffer);
					throw;
				}
			}
			template<typename First, typename... Args>
			void ctor(First&& first, Args&& ... args) & noexcept(noexcept(Base::ctor(std::forward<First>(first), std::forward<Args>(args)...))) {
				check_dead_pattern();
				tc::remove_dead_pattern(this->m_buffer);
				try {
					Base::ctor(std::forward<First>(first), std::forward<Args>(args)...);
				} catch (...) {
					tc::fill_with_dead_pattern(this->m_buffer);
					throw;
				}
			}
			void dtor() & noexcept {
				_ASSERTDEBUG(!tc::has_dead_pattern(this->m_buffer));
				Base::dtor();
				tc::fill_with_dead_pattern(this->m_buffer);
			}

		private:
			void check_dead_pattern() const& noexcept {
				// RT#12004: g_mtxSharedHeap's destructor fails this check in the Excel insider build 16.0.6568.2036
				// because this version does not realease all its locks on CTCAddInModule so that tc::shared_heap::shutdown()
				// and thus g_mtxSharedHeap.dtor() are never called. This build appears to be broken. It throws and does
				// not handle 0xC0000008 (An invalid handle was specified) while closing even without think-cell installed.
				// I could not reproduce the error in the next Excel insider build 16.0.6701.1008. -Edgar 2016-03-14
				_ASSERTDEBUG(tc::has_dead_pattern(this->m_buffer));
			}

			STATIC_FINAL(dereference)() const& -> decltype(auto) {
				_ASSERTDEBUG(!tc::has_dead_pattern(this->m_buffer));
				return Base::STATIC_VIRTUAL_METHOD_NAME(dereference)();
			}
#endif
		};

		template< typename T >
		struct [[nodiscard]] storage_for_without_dtor : storage_for_base<storage_for_without_dtor<T>, T>{
		};

		template< typename T >
		// TODO: inline into derived class
		struct [[nodiscard]] storage_for_dtor_at_end_of_scope : storage_for<T> {
			~storage_for_dtor_at_end_of_scope() { this->dtor(); }
		};

		template< typename T >
		struct scoped_constructor;

		template< typename T >
		struct scoped_constructor<storage_for<T>&> final : private tc::nonmovable {
			template<typename... Args>
			explicit scoped_constructor( storage_for<T>& ot, Args&&... args ) MAYTHROW
			:	m_ot(ot)
			{
				m_ot.ctor(std::forward<Args>(args)...); // MAYTHROW
			}
			~scoped_constructor() noexcept {
				m_ot.dtor();
			}
		private:
			storage_for<T>& m_ot;
		};
	}
	using no_adl::storage_for_without_dtor;
	using no_adl::storage_for;
	using no_adl::scoped_constructor;
	using no_adl::storage_for_dtor_at_end_of_scope;
}

MODIFY_WARNINGS_END

#ifdef __clang__

#define scoped_construct_0(var) tc::scoped_constructor< decltype((var)) > UNIQUE_IDENTIFIER((var));
#define scoped_construct_1(var, ...) tc::scoped_constructor< decltype((var)) > UNIQUE_IDENTIFIER((var), __VA_ARGS__ );

#define scoped_construct(...) \
	BOOST_PP_CAT(scoped_construct_, BOOST_PP_GREATER(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 1))(__VA_ARGS__)

#else

#define scoped_construct(var, ...) tc::scoped_constructor< decltype((var)) > UNIQUE_IDENTIFIER((var), __VA_ARGS__ );

#endif
