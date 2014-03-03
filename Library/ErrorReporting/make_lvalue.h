#pragma once

#include <boost/implicit_cast.hpp>

template< typename T >
T const& make_lvalue( T const& t ) {
	return t;
}

template< typename T >
T & make_lvalue( T & t ) {
	return t;
}

template< typename T >
T & make_lvalue( T && t ) {
	return boost::implicit_cast<T&>(t);
}

#define USE_RVALUE_AS_LVALUE(T) \
public: \
	operator T&() const { \
		/*return make_lvalue(*this); // possible if we can overload on && */ \
		return tc::make_mutable(*this); \
	}
