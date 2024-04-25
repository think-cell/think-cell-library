
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/assert_defs.h"
#include "base/noncopyable.h"
#include "base/type_traits.h"
#include "base/casts.h"
#include "base/static_polymorphism.h"
#include "base/renew.h"

#include <type_traits>

MODIFY_WARNINGS_BEGIN(((disable)(4297))) // 'function' : function assumed not to throw an exception but does.

namespace tc {
	namespace no_adl {
		struct uninitalized_storage_dummy final {
			constexpr uninitalized_storage_dummy() noexcept = delete; // non-trival to avoid zero-initialization
		};

		// clang does not correctly support contrained special member functions before version 15.
		template<typename T, bool = std::is_trivially_destructible<T>::value>
		struct storage_for_select_dtor_base;

		template<typename T>
		struct storage_for_select_dtor_base<T, false> {
			constexpr storage_for_select_dtor_base() noexcept {}
			constexpr ~storage_for_select_dtor_base() {}

			union {
				uninitalized_storage_dummy m_dummy;
				T m_buffer;
			};
		};

		template<typename T>
		struct storage_for_select_dtor_base<T, /*is_trivially_destructible*/true> {
			constexpr storage_for_select_dtor_base() noexcept {}
			constexpr ~storage_for_select_dtor_base() = default;

			union {
				uninitalized_storage_dummy m_dummy;
				T m_buffer;
			};
		};

		template< typename Derived, typename T >
		struct [[nodiscard]] storage_for_base : protected storage_for_select_dtor_base<std::remove_const_t<T>> {
			static_assert( !std::is_trivially_constructible<storage_for_select_dtor_base<std::remove_const_t<T>>>::value );
			STATICASSERTEQUAL(
				std::is_trivially_destructible<storage_for_select_dtor_base<std::remove_const_t<T>>>::value,
				std::is_trivially_destructible<T>::value
			);
			using this_type = storage_for_base<Derived, T>;
			static_assert( tc::decayed< std::remove_const_t<T> >, "only const allowed as qualification" );

			// Avoiding tc::nonmovable because it affects layout on clang, if T is derived from tc::nonmovable as well.
			storage_for_base& operator=(storage_for_base&&) noexcept = delete;

		public:
			[[nodiscard]] static Derived& from_content(T& t) noexcept requires (!std::is_const<T>::value) {
				STATICASSERTEQUAL( sizeof(storage_for_base), sizeof(T) );
				return *tc::derived_cast<Derived>(reinterpret_cast<storage_for_base*>(std::addressof(t)));
			}
			[[nodiscard]] static Derived const& from_content(T const& t) noexcept {
				STATICASSERTEQUAL( sizeof(storage_for_base), sizeof(T) );
				return *tc::derived_cast<Derived>(reinterpret_cast<storage_for_base const*>(std::addressof(t)));
			}
			[[nodiscard]] constexpr std::remove_const_t<T>* uninitialized_addressof() & noexcept {
				return std::addressof(this->m_buffer);
			}
			[[nodiscard]] constexpr T const* uninitialized_addressof() const& noexcept {
				return std::addressof(this->m_buffer);
			}
			void ctor() & noexcept(std::is_nothrow_default_constructible<T>::value) {
				// This check is not strict enough. The following struct is !std::is_trivially_default_constructible,
				// but ctor_default does not initialize n to 0, while ctor_value does:
				//	struct Foo {
				//		tc::string<char> s; // has user-defined default ctor
				//		int n; // has no user-defined default ctor
				//	};
				static_assert(!std::is_trivially_default_constructible<T>::value, "You must decide between ctor_default and ctor_value!");
				ctor_default();
			}

			void ctor_default() & noexcept(std::is_nothrow_default_constructible<T>::value) {
				::new (static_cast<void*>(&this->m_buffer)) T; // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
			}
			template<typename... Args> requires (0 < sizeof...(Args))
			void ctor_default(Args&&... args) & noexcept(noexcept(tc::explicit_cast<T>(std::declval<Args>()...))) {
				::new (static_cast<void*>(&this->m_buffer)) T(tc::explicit_cast<T>(tc_move_if_owned(args)...)); // :: ensures that non-class scope operator new is used, cast to void* ensures that built-in placement new is used  (18.6.1.3)
			}

			template<typename... Args> // ctor_value with non-empty argument list is useful in generic code
			constexpr void ctor_value(Args&& ... args) & noexcept(std::is_nothrow_constructible<T, Args&&...>::value) {
 				tc::ctor(this->m_buffer, tc_move_if_owned(args)...);
			}
			template<typename First, typename... Args>
			constexpr void ctor(First&& first, Args&& ... args) & noexcept(std::is_nothrow_constructible<T, First&&, Args&&...>::value) {
				tc::ctor(this->m_buffer, tc_move_if_owned(first), tc_move_if_owned(args)...);
			}
			[[nodiscard]] constexpr T const* operator->() const& noexcept {
				return std::addressof(dereference());
			}
			[[nodiscard]] constexpr T* operator->() & noexcept {
				return std::addressof(tc::as_mutable(dereference()));
			}
			[[nodiscard]] constexpr T const& operator*() const& noexcept {
				return dereference();
			}
			[[nodiscard]] constexpr T& operator*() & noexcept {
				return tc::as_mutable(dereference());
			}
			[[nodiscard]] constexpr T&& operator*() && noexcept {
				return tc_move_always(tc::as_mutable(dereference()));
			}
			constexpr void dtor() & noexcept {
				tc::dtor_static(tc::as_mutable(this->m_buffer));
			}
		protected:
			STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD(constexpr, dereference)() const& -> T const& {
				STATICASSERTEQUAL( sizeof(storage_for_base), sizeof(T) ); // no extra members, important for arrays
				return this->m_buffer;
			}
		};

		template< typename Derived, typename T >
		struct [[nodiscard]] storage_for_base<Derived, T&> : tc::nonmovable {
		protected:
			static_assert(!std::is_reference<T>::value);
			using this_type = storage_for_base<Derived, T&>;
			T* m_buffer;

			STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD(constexpr, dereference)() const& -> T& {
				return *m_buffer;
			}
		public:
			constexpr void ctor(T& t) & noexcept {
				m_buffer = std::addressof(t);
			}

			// reference semantics == no deep constness
			constexpr T* operator->() const& noexcept {
				return std::addressof(dereference());
			}
			constexpr T& operator*() const& noexcept {
				return dereference();
			}
			constexpr void dtor() & noexcept {
#if defined(_DEBUG) && defined(TC_PRIVATE)
				if( !std::is_constant_evaluated() ) tc::fill_with_dead_pattern(this->m_buffer);
#endif
			}
		};

		template< typename Derived, typename T >
		struct [[nodiscard]] storage_for_base<Derived, T&&> : tc::nonmovable {
		protected:
			static_assert(!std::is_reference<T>::value);
			using this_type = storage_for_base<Derived, T&&>;
			T* m_buffer;

			STATIC_VIRTUAL_WITH_DEFAULT_IMPL_MOD(constexpr, dereference)() const& -> T&& {
				return static_cast<T&&>(*m_buffer);
			}
		public:
			constexpr void ctor(T&& t) & noexcept {
				m_buffer = std::addressof(t);
			}

			constexpr T&& operator*() && noexcept {
				return dereference();
			}
			constexpr void dtor() & noexcept {
#if defined(_DEBUG) && defined(TC_PRIVATE)
				if( !std::is_constant_evaluated() ) tc::fill_with_dead_pattern(this->m_buffer);
#endif
			}
		};

		template< typename Derived, typename T >
		struct [[nodiscard]] storage_for_checked_access_base : storage_for_base<Derived, T> {
			using this_type = storage_for_checked_access_base<Derived, T>;
			using Base = typename storage_for_checked_access_base::storage_for_base;
			
#if defined(_DEBUG) && defined(TC_PRIVATE)
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
			void ctor_value(Args&& ... args) & noexcept(noexcept(std::declval<Base&>().ctor_value(tc_move_if_owned(args)...))) {
				check_dead_pattern();
				tc::remove_dead_pattern(this->m_buffer);
				try {
					Base::ctor_value(tc_move_if_owned(args)...);
				} catch (...) {
					tc::fill_with_dead_pattern(this->m_buffer);
					throw;
				}
			}
			template<typename First, typename... Args>
			void ctor(First&& first, Args&& ... args) & noexcept(noexcept(Base::ctor(tc_move_if_owned(first), tc_move_if_owned(args)...))) {
				check_dead_pattern();
				tc::remove_dead_pattern(this->m_buffer);
				try {
					Base::ctor(tc_move_if_owned(first), tc_move_if_owned(args)...);
				} catch (...) {
					tc::fill_with_dead_pattern(this->m_buffer);
					throw;
				}
			}
			void dtor() & noexcept {
				_ASSERTDEBUG(!tc::has_dead_pattern(this->m_buffer));
				Base::dtor();
				// tc::fill_with_dead_pattern implied by Base::dtor
			}

		protected:
			void check_dead_pattern() const& noexcept {
				// RT#12004: g_mtxSharedHeap's destructor fails this check in the Excel insider build 16.0.6568.2036
				// because this version does not realease all its locks on CTCAddInModule so that tc::shared_heap::shutdown()
				// and thus g_mtxSharedHeap.dtor() are never called. This build appears to be broken. It throws and does
				// not handle 0xC0000008 (An invalid handle was specified) while closing even without think-cell installed.
				// I could not reproduce the error in the next Excel insider build 16.0.6701.1008. -Edgar 2016-03-14
				_ASSERTDEBUG(tc::has_dead_pattern(this->m_buffer));
			}

		private:
			STATIC_OVERRIDE(dereference)() const& noexcept -> decltype(auto) {
				_ASSERTDEBUG(!tc::has_dead_pattern(this->m_buffer));
				return Base::STATIC_VIRTUAL_METHOD_NAME(dereference)();
			}
#endif
		};

		template< typename T >
		struct [[nodiscard]] storage_for : storage_for_checked_access_base<storage_for<T>, T> {
#if defined(_DEBUG) && defined(TC_PRIVATE)
			storage_for() noexcept {
				tc::fill_with_dead_pattern(this->m_buffer);
			}
			~storage_for() {
				this->check_dead_pattern();
			}
#endif
		};

		template< typename T >
		struct [[nodiscard]] storage_for_without_dtor : storage_for_base<storage_for_without_dtor<T>, T>{
		};

		template< typename T >
		// TODO: inline into derived class
		struct [[nodiscard]] storage_for_dtor_at_end_of_scope : storage_for<T> {
			constexpr ~storage_for_dtor_at_end_of_scope() { this->dtor(); }
		};

		template<typename T>
		struct members_alive_before_ctor : storage_for_base<members_alive_before_ctor<T>, T> {
			using base_ = typename members_alive_before_ctor::storage_for_base;
#if defined(_DEBUG) && defined(TC_PRIVATE)
			members_alive_before_ctor() noexcept {
				tc::fill_with_dead_pattern(*this); // no members must be alive yet
			}
#endif

			~members_alive_before_ctor() {
				this->dtor();
			}

			// Hide ctor variants from storage_for_base that may zero-initialize which would kill lifetime_begins_before_ctor member
			void ctor(auto&&...) = delete;
			void ctor_value(auto&&...) = delete;
		};

		template<typename T>
		struct lifetime_begins_before_ctor final : storage_for_checked_access_base<lifetime_begins_before_ctor<T>, T> {
#if defined(_DEBUG) && defined(TC_PRIVATE)
			lifetime_begins_before_ctor() noexcept {
				_ASSERTDEBUG( !tc::has_dead_pattern(this->m_buffer) );
			}
#endif
			~lifetime_begins_before_ctor() {
				this->dtor();
			}
		};

		template< typename T >
		struct scoped_constructor;

		template< typename T >
		struct scoped_constructor<storage_for<T>&> final : private tc::nonmovable {
			template<typename... Args>
			explicit scoped_constructor( storage_for<T>& ot, Args&&... args ) MAYTHROW
			:	m_ot(ot)
			{
				m_ot.ctor(tc_move_if_owned(args)...); // MAYTHROW
			}
			~scoped_constructor() {
				m_ot.dtor();
			}
		private:
			storage_for<T>& m_ot;
		};
	}
	using no_adl::storage_for_without_dtor;
	using no_adl::storage_for;
	using no_adl::storage_for_dtor_at_end_of_scope;
	using no_adl::members_alive_before_ctor;
	using no_adl::lifetime_begins_before_ctor;
	using no_adl::scoped_constructor;
}

MODIFY_WARNINGS_END

#define scoped_construct(var, ...) tc::scoped_constructor< decltype((var)) > UNIQUE_IDENTIFIER((var) __VA_OPT__(,) __VA_ARGS__ );
