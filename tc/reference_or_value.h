
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
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
		struct reference_or_value final {
			static_assert( !std::is_void<T>::value );
			static_assert( !std::is_reference<T>::value );

			using value_type = std::remove_cv_t<T>;
			using reference = value_type&;
			using const_reference = value_type const&;

			constexpr reference_or_value() noexcept {} // m_t may be default-constructible
			constexpr reference_or_value(reference_or_value const&) = default;
			constexpr reference_or_value(reference_or_value&&) = default;
			
			template< typename Rhs, std::enable_if_t<tc::is_safely_constructible<value_type, Rhs&&>::value>* = nullptr >
			constexpr reference_or_value( aggregate_tag_t, Rhs&& rhs ) noexcept
			: m_t( std::forward<Rhs>(rhs) )
			{}

			// T may be a proxy with reference behavior (e.g. pair<X&,X&>): operator= may not be equivalent to copy ctor.
			// operator=() with tc::renew for pointer semantics.
			reference_or_value& operator=(reference_or_value const& other) & noexcept {
				_ASSERT(this != std::addressof(other));
				tc::renew(*this, aggregate_tag, other.m_t);
				return *this;
			}

			reference_or_value& operator=(reference_or_value&& other) & noexcept {
				_ASSERT(this != std::addressof(other));
				tc::renew(*this, aggregate_tag, tc_move(other).m_t);
				return *this;
			}
			
			constexpr reference best_access() const& noexcept {
				// When declaring m_t non-mutable and using const_cast here be undefined behavior in code like:
				//	reference_or_value<T> _const_ foo;
				//	foo.best_access();
				// ?
				return m_t;
			}
			constexpr value_type* operator->() & noexcept {
				return std::addressof(m_t);
			}
			constexpr value_type const* operator->() const& noexcept {
				return std::addressof(m_t);
			}
			constexpr reference operator*() & noexcept {
				return m_t;
			}
			constexpr const_reference operator*() const& noexcept {
				return m_t;
			}
			constexpr value_type&& operator*() && noexcept {
				return std::move(m_t);
			}
			value_type const&& operator*() const&& noexcept {
				return std::move(tc::as_const(m_t));
			}

		private:
			// Store mutable T to return mutable reference from best_access().
			// best_access() is used to create an index object from an iterator
			// to m_t. add_index_interface<T>::index needs a mutable iterator.
			// This is because of a deficiency in the STL: If there was a way to
			// get a mutable reference from a const_iterator *and* a mutable T,
			// the index type could always take the const_iterator instead.
			value_type mutable m_t;
		};

		template< typename T >
		struct reference_or_value<T&> final {
		private:
			T* m_pt;

		public:
			using reference = T&;
			using const_reference = reference;

			constexpr reference_or_value(aggregate_tag_t, T& t) noexcept
			:	m_pt(std::addressof(t))
			{}
			constexpr reference best_access() const& noexcept {
				return *m_pt;
			}
			constexpr T * operator->() const& noexcept {
				return m_pt;
			}
			constexpr reference operator*() const& noexcept {
				return *m_pt;
			}
		};

		template< typename T >
		struct reference_or_value<T&&> final {
		private:
			T* m_pt;

		public:
			using reference = T&&;
			using const_reference = reference;

			constexpr reference_or_value(aggregate_tag_t, T&& t) noexcept
			:	m_pt(std::addressof(t))
			{}
			constexpr reference best_access() const& noexcept {
				return tc_move_always(*m_pt);
			}
			constexpr T * operator->() const& noexcept { // no such thing as "pointer-to-rvalue"
				return m_pt;
			}
			constexpr reference operator*() const& noexcept {
				return tc_move_always(*m_pt);
			}
		};
	}
	using no_adl::reference_or_value;
	
	template< typename T >
	auto make_reference_or_value(T&& t) noexcept return_ctor(
		reference_or_value<T>,
		(tc::aggregate_tag, std::forward<T>(t))
	)

	namespace no_adl {
		template< typename Func, typename Enable, typename... Args >
		struct stores_result_of final {
		private:
			tc::reference_or_value< std::invoke_result_t< Func, Args... > > m_t;

		public:
			stores_result_of( Func&& func, Args... args ) MAYTHROW
				: m_t(aggregate_tag, std::forward<Func>(func)(static_cast<Args>(args)...))
			{}

			auto get() const& noexcept ->decltype(auto) {
				return *m_t;
			}
			auto get() & noexcept ->decltype(auto) {
				return *m_t;
			}
			auto get() && noexcept ->decltype(auto) {
				return *tc_move(m_t);
			}
			template<typename FuncTo, typename ...ArgsTo>
			auto pass_to(FuncTo&& functo, ArgsTo&& ...args) const& noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(*m_t, std::forward<ArgsTo>(args)...);
			}
			template<typename FuncTo, typename ...ArgsTo>
			auto pass_to(FuncTo&& functo, ArgsTo&& ...args) & noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(*m_t, std::forward<ArgsTo>(args)...);
			}
			template<typename FuncTo, typename ...ArgsTo>
			auto pass_to(FuncTo&& functo, ArgsTo&& ...args) && noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(*tc_move(m_t), std::forward<ArgsTo>(args)...);
			}
		};

		template< typename Func, typename... Args >
		struct stores_result_of<Func, std::enable_if_t< std::is_void< std::invoke_result_t< Func, Args... > >::value >, Args... > final {
			stores_result_of( Func&& func, Args... args ) MAYTHROW {
				std::forward<Func>(func)(static_cast<Args>(args)...);
			}

			void get() const& noexcept {}
			template<typename FuncTo, typename ...ArgsTo>
			auto pass_to(FuncTo&& functo, ArgsTo&& ...args) const& noexcept ->decltype(auto) {
				return std::forward<FuncTo>(functo)(std::forward<ArgsTo>(args)...);
			}
		};
	}
	template< typename Func, typename... Args >
	using stores_result_of=no_adl::stores_result_of<Func, void, Args...>;
}
