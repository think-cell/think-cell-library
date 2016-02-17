//-----------------------------------------------------------------------------------------------------------------------------
// think-cell range library
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

template< typename T, bool bArray=std::is_array<T>::value >
struct scoped_restorer;

template< typename T >
struct scoped_restorer< T, false >: private tc::nonmovable {
private:
	T& m_data;
	T m_dataSaved;
public:
	scoped_restorer( T& data ) noexcept
	:	m_data(data), 
		m_dataSaved(VERIFYINITIALIZED(data)) {}
	scoped_restorer(T&& data) noexcept
	:	m_data(data), 
		m_dataSaved(VERIFYINITIALIZED(tc_move(data))) {}
	~scoped_restorer() {
		m_data=tc_move_always(m_dataSaved);
	}
	// can be used to access/change saved value
	T& operator=( T const& t ) & noexcept {
		return m_dataSaved=t;
	}
	T& operator=(T&& t) & noexcept {
		return m_dataSaved=tc_move(t);
	}
	operator T const&() const noexcept {
		return m_dataSaved;
	}
};

template< typename T >
struct scoped_restorer< T, true >: private tc::nonmovable {
private:
	T& m_data;
	T m_dataSaved;
public:
	scoped_restorer( T& data ) noexcept
	:	m_data(data) 
	{
		tc::cont_assign( m_dataSaved, data );
	}
	scoped_restorer(T&& data) noexcept
	:	m_data(data) 
	{
		tc::cont_assign( m_dataSaved, tc_move(data) );
	}
	~scoped_restorer() {
		tc::cont_assign( m_data, m_dataSaved );
	}
};

template< typename T >
struct scoped_assigner
:	scoped_restorer<T> {
	scoped_assigner( T& data, T const& dataSet ) noexcept
	:	scoped_restorer<T>( tc_move_always(data) )
	{
		data=dataSet;
	}
	T& operator=( T const& t ) & noexcept {
		return scoped_restorer<T>::operator=( t );
	}
	T& operator=(T&& t) & noexcept {
		return scoped_restorer<T>::operator=( tc_move(t) );
	}
};

#define restore_after_scope(var) scoped_restorer< std::remove_reference_t<decltype((var))> > UNIQUE_IDENTIFIER(var);
#define scoped_assign(var,value) scoped_assigner< std::remove_reference_t<decltype((var))> > UNIQUE_IDENTIFIER((var),(value));

#define scope_exit_counter( unique, ... ) \
auto BOOST_PP_CAT(scope_exit_lambda_, unique) = [&] { __VA_ARGS__ ; }; \
tc::scope_exit_impl< decltype( BOOST_PP_CAT(scope_exit_lambda_, unique) ) > \
BOOST_PP_CAT(scope_exit_struct_, unique)( BOOST_PP_CAT(scope_exit_lambda_, unique) );

#define scope_exit( ... ) scope_exit_counter( __COUNTER__, __VA_ARGS__ )
