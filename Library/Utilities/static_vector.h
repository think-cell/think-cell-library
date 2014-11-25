#pragma once

#include <boost/preprocessor/enum.hpp>
#include "Library/ErrorReporting/storage_for.h"
#include "renew.h"

template< typename T, unsigned int N >
class static_vector {
	unsigned int m_iEnd;
	tc::storage_for<T> m_at[N];

	template<typename It>
	void append(It itBegin, It itEnd) {
		for( ; itBegin!=itEnd; ++itBegin ) {
			emplace_back( *itBegin );
		}
	}	

public:
	static_vector()
	:	m_iEnd( 0 )
	{}
	
	template<typename It>
	static_vector(It itBegin, It itEnd )
	:	m_iEnd( 0 )
	{
		append( itBegin, itEnd );
	}

	typedef T& reference;
	typedef T const& const_reference;
	typedef T* iterator;
	typedef T const* const_iterator;
	typedef unsigned int size_type;
	typedef int difference_type;
	typedef T value_type;
	typedef T* pointer;
	typedef T const* const_pointer;

	// iterators
	iterator begin() {
		return std::addressof(boost::implicit_cast<T&>(m_at[0]));
	}
	const_iterator begin() const {
		return std::addressof(boost::implicit_cast<T const&>(m_at[0]));
	}
	iterator end() {
		return std::addressof(boost::implicit_cast<T&>(m_at[m_iEnd]));
	}
	const_iterator end() const {
		return std::addressof(boost::implicit_cast<T const&>(m_at[m_iEnd]));
	}

	// query state
	bool empty() const {
		return 0==m_iEnd;
	}
	bool full() const {
		return N == m_iEnd;
	}
	size_type size() const {
		return m_iEnd;
	}

	// access
	T & operator[]( size_type i ) {
		_ASSERT( i<size() );
		return m_at[i];
	}
	T const& operator[]( size_type i ) const {
		_ASSERT( i<size() );
		return m_at[i];
	}
	T & front() {
		_ASSERT( !empty() );
		return m_at[0];
	}
	T const& front() const {
		_ASSERT( !empty() );
		return m_at[0];
	}
	T & back() {
		_ASSERT( !empty() );
		return m_at[m_iEnd - 1];
	}
	T const& back() const {
		_ASSERT( !empty() );
		return m_at[m_iEnd - 1];
	}

	// modify
	template<typename... Args>
	void emplace_back(Args&&... args) {
		_ASSERT(!full());
		++m_iEnd;
		new (end() - 1) T(std::forward<Args>(args)...);
	}

	void pop_back() {
		_ASSERT( !empty() );
		--m_iEnd;
		tc::dtor(boost::implicit_cast<T&>(m_at[m_iEnd]));
	}

	void clear() {
		while( !empty() ) pop_back();
	}

	template<typename It>
	void assign(It itBegin, It itEnd ) {
		clear();
		append( itBegin, itEnd );
	}

	template<typename It>
	void insert(iterator itPos, It itBegin, It itEnd) { // required by boost::push_back - TODO: support for itPos != end()
		_ASSERT(itPos==end());
		append( itBegin, itEnd );
	}

	iterator erase(iterator itBegin, iterator itEnd) { // required by erase_tail (e.g, via tc::unique_erase) - TODO: support for itEnd != end()
		_ASSERT(itEnd==end());
		for ( ;itBegin!=end(); pop_back()); // works because itBegin is just a pointer
		return end();
	}

	~static_vector() {
		clear();
	}
};
