#pragma once

#include "Library/Utilities/perfect_forward.h"
#include "Library/Utilities/casts.h"

#include <type_traits>

template< typename T >
class storage_for {
	typename std::aligned_storage<sizeof(T),std::alignment_of<T>::value>::type m_buffer;
public:
	void ctor() {
		new (&m_buffer) T();
	}
#define PART1() \
	template<
#define PART2() \
	> void ctor(
#define PART3() ) { \
		new (&m_buffer) T(
#define PART4() ); \
	}
PERFECT_FORWARD
#undef PART1
#undef PART2
#undef PART3
#undef PART4
	operator T const&() const {
		static_assert( sizeof(storage_for<T>)==sizeof(m_buffer), "" ); // no extra members, important for arrays
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
