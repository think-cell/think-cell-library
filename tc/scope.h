
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once 

#include "type_traits.h"
#include "noncopyable.h"
#include "tag_type.h"
#include "assign.h"

namespace tc {
	template<typename T>
	struct scope_exit_impl final : private tc::nonmovable {
		explicit scope_exit_impl(T const& exitscope) noexcept
		:	m_exitscope(exitscope)
		{}
		~scope_exit_impl(){
			NOEXCEPT( m_exitscope() );
		}
	private:
		T const& m_exitscope; 
	};
}

DEFINE_TAG_TYPE(scoped_assign_tag)

template< typename T >
struct scoped_restorer : private tc::nonmovable {
	static_assert(!std::is_array<std::remove_reference_t<T>>::value);
	static_assert(
		std::is_lvalue_reference<T>::value,
		"There may be use cases for non-lvalue types here. "
		"But this assert has to stay at least until https://connect.microsoft.com/VisualStudio/Feedback/Details/2117239 is fixed"
	);
private:
	std::conditional_t< std::is_lvalue_reference<T>::value,
		T, // regular reference
		std::remove_reference_t<T> // proxy, store by value (and not as rvalue reference)
	> m_data;
	tc::decay_t<T> m_dataSaved;
public:
	scoped_restorer( T&& data ) noexcept
	:	m_data(tc_move_if_owned(data)),
		m_dataSaved(VERIFYINITIALIZED(data)) {}
	scoped_restorer( T&& data, scoped_assign_tag_t ) noexcept
	:	m_data(tc_move_if_owned(data)),
		m_dataSaved(tc_move_always(VERIFYINITIALIZED(data))) {}
	~scoped_restorer() {
		m_data=tc_move_always(m_dataSaved);
	}
	// can be used to access/change saved value
	template<typename T2>
	scoped_restorer& operator=( T2&& t ) & noexcept {
		m_dataSaved=std::forward<T2>(t);
		return *this;
	}
	operator tc::decay_t<T> const& () const& noexcept {
		return m_dataSaved;
	}
};

template< typename T >
struct scoped_assigner
:	scoped_restorer<T> {
	template<typename T2>
	explicit scoped_assigner( T&& data, T2&& dataSet ) noexcept
	:	scoped_restorer<T>( tc_move_if_owned(data), scoped_assign_tag )
	{
		data=std::forward<T2>(dataSet);
	}
	using scoped_restorer<T>::operator=;
};

template< typename T >
struct scoped_assigner_better
:	scoped_restorer<T> {
	template<typename Val, typename Better>
	explicit scoped_assigner_better( T& data, Val&& val, Better better ) noexcept
	:	scoped_restorer<T>( data )
	{
		tc::assign_better(data, std::forward<Val>(val), tc_move(better));
	}
};


#define restore_after_scope(var) scoped_restorer< decltype((var)) > UNIQUE_IDENTIFIER(var);
#define scoped_assign(var,value) scoped_assigner< decltype((var)) > UNIQUE_IDENTIFIER((var),(value));
#define scoped_assign_better(var, value, better) scoped_assigner_better< decltype((var)) > UNIQUE_IDENTIFIER((var),(value),(better));
#define scoped_assign_max(var, value) scoped_assign_better((var),(value),tc::fn_greater());

#if _MSC_VER_FULL <= 190023026
	#define scoped_assign_for_baseclass_member(var,value) scoped_assigner< decltype(var)& > UNIQUE_IDENTIFIER((var),(value));
	#define restore_after_scope_for_baseclass_member(var) scoped_restorer< decltype(var)& > UNIQUE_IDENTIFIER(var);
#else
	#error "should be fixed: https://connect.microsoft.com/VisualStudio/Feedback/Details/2117239"
#endif

// scope_exit must not throw in order to avoid double throws. If exceptions are needed, implement scope_success/scope_failure.
#define scope_exit_counter( unique, ... ) \
auto BOOST_PP_CAT(scope_exit_lambda_, unique) = [&]() noexcept { __VA_ARGS__ ; }; \
tc::scope_exit_impl< decltype( BOOST_PP_CAT(scope_exit_lambda_, unique) ) > \
BOOST_PP_CAT(scope_exit_struct_, unique)( BOOST_PP_CAT(scope_exit_lambda_, unique) );

#define scope_exit( ... ) scope_exit_counter( __COUNTER__, __VA_ARGS__ )
