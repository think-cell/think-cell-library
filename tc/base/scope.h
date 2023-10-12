
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once 

#include "type_traits.h"
#include "noncopyable.h"
#include "tag_type.h"
#include "assign.h"

namespace tc {
	namespace no_adl {
		template<typename T>
		struct scope_exit_impl final : private tc::nonmovable {
			constexpr explicit scope_exit_impl(T const& exitscope) noexcept
			:	m_exitscope(exitscope)
			{}
			constexpr ~scope_exit_impl(){
				m_exitscope();
			}
		private:
			T const m_exitscope;
		};

		struct make_scope_exit_impl {
			template <typename T>
			[[nodiscard]] constexpr scope_exit_impl<T> operator->*(T const& exitscope) && noexcept {
				return scope_exit_impl<T>(exitscope);
			}
		};
	}
	using no_adl::scope_exit_impl;
	using no_adl::make_scope_exit_impl;

	DEFINE_TAG_TYPE(scoped_assign_tag)

	namespace no_adl {
		template< typename T >
		struct scoped_restorer : private tc::nonmovable {
			static_assert(!std::is_array<std::remove_reference_t<T>>::value);
			static_assert(
				std::is_lvalue_reference<T>::value,
				"There may be use cases for non-lvalue types here. "
				"But this assert has to stay at least until https://connect.microsoft.com/VisualStudio/Feedback/Details/2117239 is fixed"
			);
		protected:
			std::conditional_t< std::is_lvalue_reference<T>::value,
				T, // regular reference
				std::remove_reference_t<T> // proxy, store by value (and not as rvalue reference)
			> m_var;
			tc::decay_t<T> m_valSaved;
		public:
			constexpr scoped_restorer( T&& var ) noexcept
			:	m_var(tc_move_if_owned(var)),
				m_valSaved(VERIFYINITIALIZED(var)) {}
			constexpr scoped_restorer( T&& var, scoped_assign_tag_t ) noexcept
			:	m_var(tc_move_if_owned(var)),
				m_valSaved(tc_move_always(VERIFYINITIALIZED(var))) {}
			constexpr ~scoped_restorer() {
				m_var=tc_move(m_valSaved);
			}
			// can be used to access/change saved value
			template<typename T2>
			constexpr scoped_restorer& operator=( T2&& t ) & noexcept {
				m_valSaved=tc_move_if_owned(t);
				return *this;
			}
			constexpr operator tc::decay_t<T> const& () const& noexcept {
				return m_valSaved;
			}
		};
	}
	using no_adl::scoped_restorer;

	DEFINE_TAG_TYPE(scoped_change_tag)

	namespace no_adl {
		template< typename T >
		struct scoped_assigner
		:	scoped_restorer<T> {
			template<typename T2>
			constexpr explicit scoped_assigner( T&& var, T2&& val ) noexcept
			:	scoped_restorer<T>( tc_move_if_owned(var), scoped_assign_tag )
			{
				this->m_var=tc_move_if_owned(val);
			}
			template<typename T2>
			constexpr explicit scoped_assigner( scoped_change_tag_t, T&& var, T2&& val ) noexcept
			:	scoped_assigner( tc_move_if_owned(var), tc_move_if_owned(val) )
			{
				_ASSERT(!(this->m_var == this->m_valSaved));
			}
			using scoped_restorer<T>::operator=;
		};
	}
	using no_adl::scoped_assigner;

	namespace no_adl {
		template< typename T >
		struct scoped_assigner_better
		:	scoped_restorer<T> {
			template<typename Val, typename Better>
			constexpr explicit scoped_assigner_better( T& var, Val&& val, Better better ) noexcept
			:	scoped_restorer<T>( var )
			{
				tc::assign_better(tc_move(better), var, tc_move_if_owned(val));
			}
		};
	}
	using no_adl::scoped_assigner_better;
}

// Note about brackets:
// * in the decltype, var is parenthesized to add a reference.
// * in the constructor, var and __VA_ARGS__ is not parenthesized to disallow use of the comma operator.
// * we use braces to create the object to prevent a function declaration.
//
// We do not have a semicolon in the declaration to force the caller to add one.
#define tc_restore_after_scope(var)    tc::scoped_restorer< decltype((var)) > UNIQUE_IDENTIFIER{var}
#define tc_scoped_assign(var, ...)     tc::scoped_assigner< decltype((var)) > UNIQUE_IDENTIFIER{var, __VA_ARGS__}
#define tc_scoped_change(var, ...)	   tc::scoped_assigner< decltype((var)) > UNIQUE_IDENTIFIER{tc::scoped_change_tag, var, __VA_ARGS__}
#define tc_scoped_assign_max(var, ...) tc::scoped_assigner_better< decltype((var)) > UNIQUE_IDENTIFIER{var, __VA_ARGS__,tc::fn_greater()}

#if _MSC_VER_FULL <= 190023026
	#define tc_scoped_assign_for_baseclass_member(var, ...)  tc::scoped_assigner< decltype(var)& > UNIQUE_IDENTIFIER{var, __VA_ARGS__}
	#define tc_restore_after_scope_for_baseclass_member(var) tc::scoped_restorer< decltype(var)& > UNIQUE_IDENTIFIER{var}
#else
	#error "should be fixed: https://connect.microsoft.com/VisualStudio/Feedback/Details/2117239"
#endif

// tc_scope_exit must not throw in order to avoid double throws. If exceptions are needed, implement scope_success/scope_failure.
#define tc_scope_exit auto UNIQUE_IDENTIFIER = tc::make_scope_exit_impl{} ->* [&]() noexcept -> void

// the empty namespace ensures the macro is only used at file scope
#define tc_file_scope_exit \
	namespace {} \
	static constinit auto UNIQUE_IDENTIFIER = tc::make_scope_exit_impl{} ->* []() noexcept -> void
