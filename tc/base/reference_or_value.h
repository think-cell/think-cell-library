
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "renew.h"
#include "tag_type.h"
#include "casts.h"

#include <type_traits>
#include <memory>

namespace tc {
	namespace no_adl {
		template< typename T >
		struct reference_or_value {
			static_assert( !std::is_void<T>::value );
			static_assert( !std::is_reference<T>::value );
			static_assert( !std::is_const<T>::value );
			static_assert( !std::is_volatile<T>::value );

			constexpr reference_or_value() = default; // m_t may be default-constructible
			constexpr reference_or_value(reference_or_value const&) = default;
			constexpr reference_or_value(reference_or_value&&) = default;
			
			template< typename Rhs> requires tc::safely_constructible_from<T, Rhs&&>
			constexpr reference_or_value( aggregate_tag_t, Rhs&& rhs ) noexcept
				: m_t(std::forward<Rhs>(rhs))
			{}

			// reference_or_value<T> is trivially copy assignable if T is trivially copy assignable.
			constexpr reference_or_value& operator=(reference_or_value const& other) & requires std::is_trivially_copy_assignable<T>::value = default;
			// T may be a proxy with reference behavior (e.g. pair<X&,X&>): operator= must be non trivial and may not be equivalent to copy ctor.
			// operator=() with tc::renew for pointer semantics.
#ifdef __clang__ // workaround clang bug on trivially copyable: https://github.com/llvm/llvm-project/issues/63352
			template<ENABLE_SFINAE>
			constexpr reference_or_value& operator=(reference_or_value<SFINAE_TYPE(T)> const& other) & noexcept requires
#else
			constexpr reference_or_value& operator=(reference_or_value const& other) & noexcept requires
#endif
				(!std::is_trivially_copy_assignable<T>::value) &&
				std::is_copy_constructible<T>::value
			{
				_ASSERTE(this != std::addressof(other));
				tc::renew(m_t, other.m_t);
				return *this;
			}

			constexpr reference_or_value& operator=(reference_or_value&& other) & requires std::is_trivially_move_assignable<T>::value = default;
#ifdef __clang__ // workaround clang bug on trivially copyable: https://github.com/llvm/llvm-project/issues/63352
			template<ENABLE_SFINAE>
			constexpr reference_or_value& operator=(reference_or_value<SFINAE_TYPE(T)>&& other) & noexcept requires
#else
			constexpr reference_or_value& operator=(reference_or_value&& other) & noexcept requires
#endif
				(!std::is_trivially_move_assignable<T>::value) &&
				std::is_move_constructible<T>::value
			{
				_ASSERTE(this != std::addressof(other));
				tc::renew(m_t, tc_move(other).m_t);
				return *this;
			}
			
			constexpr T& best_access() const& noexcept {
				// When declaring m_t non-mutable and using const_cast here be undefined behavior in code like:
				//	reference_or_value<T> _const_ foo;
				//	foo.best_access();
				// ?
				return m_t;
			}
			constexpr T* operator->() & noexcept {
				return std::addressof(m_t);
			}
			constexpr T const* operator->() const& noexcept {
				return std::addressof(m_t);
			}
			constexpr T& operator*() & noexcept {
				return m_t;
			}
			constexpr T const& operator*() const& noexcept {
				return m_t;
			}
			constexpr T&& operator*() && noexcept {
				return std::move(m_t);
			}
			constexpr T const&& operator*() const&& noexcept {
				return std::move(tc::as_const(m_t));
			}

		private:
			// Store mutable T to return mutable reference from best_access().
			// best_access() is used to create an index object from an iterator
			// to m_t. add_index_interface<T>::index needs a mutable iterator.
			// This is because of a deficiency in the STL: If there was a way to
			// get a mutable reference from a const_iterator *and* a mutable T,
			// the index type could always take the const_iterator instead.
			T mutable m_t;
		};

		template< typename T >
		struct reference_or_value<T&> {
		private:
			T* m_pt;

		public:
			constexpr reference_or_value(aggregate_tag_t, T& t) noexcept
			:	m_pt(std::addressof(t))
			{}
			constexpr T& best_access() const& noexcept {
				return *m_pt;
			}
			constexpr T* operator->() const& noexcept {
				return m_pt;
			}
			constexpr T& operator*() const& noexcept {
				return *m_pt;
			}
		};

		template< typename T >
		struct reference_or_value<T&&> {
		private:
			T* m_pt;

		public:
			constexpr reference_or_value(aggregate_tag_t, T&& t) noexcept
				: m_pt(std::addressof(t))
			{}
			constexpr T&& best_access() const& noexcept {
				return tc_move_always(*m_pt);
			}
			constexpr T* operator->() const& noexcept { // no such thing as "pointer-to-rvalue"
				return m_pt;
			}
			constexpr T& operator*() const& noexcept {
				return *m_pt;
			}
			constexpr T&& operator*() const&& noexcept {
				return tc_move_always(*m_pt);
			}
		};

		template< typename T >
		struct TC_EMPTY_BASES empty_value {
			static_assert( tc::empty_type<T> );

			constexpr empty_value() = default;
			constexpr empty_value(aggregate_tag_t, T) noexcept {}

			static constexpr T best_access() noexcept { return T(); }
			constexpr auto operator->() const& noexcept {
				struct pointer final {
					T m_t;
					constexpr T* operator->() && noexcept { return std::addressof(m_t); }
				};
				return pointer();
			}
			constexpr T operator*() const& noexcept { return T(); }
		};
	}
	template<typename T>
	using reference_or_value = std::conditional_t<
		tc::empty_type<std::remove_cvref_t<T>>,
		no_adl::empty_value<std::remove_cvref_t<T>>,
		no_adl::reference_or_value<std::remove_cv_t<T>>
	>;
	
	template< typename T >
	[[nodiscard]] constexpr auto make_reference_or_value(T&& t) return_ctor_noexcept(
		reference_or_value<T>,
		(tc::aggregate_tag, std::forward<T>(t))
	)

	namespace no_adl {
		template< typename Func, typename... Args >
		struct stores_result_of final {
		private:
			tc::reference_or_value< decltype(std::declval<Func>()(std::declval<Args>()...)) > m_t;

		public:
			constexpr stores_result_of( Func&& func, Args&&... args ) MAYTHROW
				: m_t(aggregate_tag, std::forward<Func>(func)(std::forward<Args>(args)...))
			{}

			constexpr auto get() const& noexcept ->decltype(auto) {
				return *m_t;
			}
			constexpr auto get() & noexcept ->decltype(auto) {
				return *m_t;
			}
			constexpr auto get() && noexcept ->decltype(auto) {
				return *tc_move(m_t);
			}
			template<typename FuncTo, typename... ArgsTo>
			constexpr auto pass_to(FuncTo&& functo, ArgsTo&& ...args) const& noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(*m_t, std::forward<ArgsTo>(args)...);
			}
			template<typename FuncTo, typename... ArgsTo>
			constexpr auto pass_to(FuncTo&& functo, ArgsTo&& ...args) & noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(*m_t, std::forward<ArgsTo>(args)...);
			}
			template<typename FuncTo, typename... ArgsTo>
			constexpr auto pass_to(FuncTo&& functo, ArgsTo&& ...args) && noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(*tc_move(m_t), std::forward<ArgsTo>(args)...);
			}
		};

		template< typename Func, typename... Args > requires std::is_void< decltype(std::declval<Func>()(std::declval<Args>()...)) >::value
		struct stores_result_of<Func, Args... > final {
			constexpr stores_result_of( Func&& func, Args... args ) MAYTHROW {
				std::forward<Func>(func)(static_cast<Args>(args)...);
			}

			static constexpr void get() noexcept {}
			template<typename FuncTo, typename... ArgsTo>
			static constexpr auto pass_to(FuncTo&& functo, ArgsTo&& ...args) noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(std::forward<ArgsTo>(args)...);
			}
		};
	}
	template< typename Func, typename... Args >
	using stores_result_of=no_adl::stores_result_of<Func, Args...>;
}
