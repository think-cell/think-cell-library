
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
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
	}
	using no_adl::scope_exit_impl;

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
				m_valSaved=std::forward<T2>(t);
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
				this->m_var=std::forward<T2>(val);
			}
			template<typename T2>
			constexpr explicit scoped_assigner( scoped_change_tag_t, T&& var, T2&& val ) noexcept
			:	scoped_assigner( std::forward<T>(var), std::forward<T2>(val) )
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
				tc::assign_better(tc_move(better), var, std::forward<Val>(val));
			}
		};
	}
	using no_adl::scoped_assigner_better;
}

#define restore_after_scope(var) tc::scoped_restorer< decltype((var)) > UNIQUE_IDENTIFIER(var);
#define scoped_assign(var,value) tc::scoped_assigner< decltype((var)) > UNIQUE_IDENTIFIER((var),(value));
#define scoped_assign_better(var, value, better) tc::scoped_assigner_better< decltype((var)) > UNIQUE_IDENTIFIER((var),(value),(better));
#define scoped_assign_max(var, value) scoped_assign_better((var),(value),tc::fn_greater());
#define scoped_change(var,value) tc::scoped_assigner< decltype((var)) > UNIQUE_IDENTIFIER(tc::scoped_change_tag, (var),(value));

#if _MSC_VER_FULL <= 190023026
	#define scoped_assign_for_baseclass_member(var,value) tc::scoped_assigner< decltype(var)& > UNIQUE_IDENTIFIER((var),(value));
	#define restore_after_scope_for_baseclass_member(var) tc::scoped_restorer< decltype(var)& > UNIQUE_IDENTIFIER(var);
#else
	#error "should be fixed: https://connect.microsoft.com/VisualStudio/Feedback/Details/2117239"
#endif

// scope_exit must not throw in order to avoid double throws. If exceptions are needed, implement scope_success/scope_failure.
#define scope_exit_counter( unique, ... ) \
	auto BOOST_PP_CAT(scope_exit_lambda_, unique) = [&]() noexcept { __VA_ARGS__; }; \
	tc::scope_exit_impl< decltype( BOOST_PP_CAT(scope_exit_lambda_, unique) ) > \
	BOOST_PP_CAT(scope_exit_struct_, unique)( BOOST_PP_CAT(scope_exit_lambda_, unique) );

#define scope_exit( ... ) scope_exit_counter( __COUNTER__, __VA_ARGS__ )

#define file_scope_exit_counter( unique, ... ) \
	namespace { struct BOOST_PP_CAT(file_scope_exit_, unique) final { \
		~ BOOST_PP_CAT(file_scope_exit_, unique) () { \
			__VA_ARGS__; \
		} \
	} BOOST_PP_CAT(file_scope_exit_instance, unique); }

#define file_scope_exit( ... ) file_scope_exit_counter( __COUNTER__, __VA_ARGS__ )
