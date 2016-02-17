#pragma once

#include "result_of.h"
#include "return_decltype.h"

template< typename Func >
struct lazy_impl {
private:
	std::decay_t<Func> m_func;
public:
	lazy_impl(Func&& func)
	:	m_func(std::forward<Func>(func))
	{}
	operator tc::result_of_t< std::decay_t<Func>() > () const {
		return m_func();
	}
	tc::result_of_t< std::decay_t<Func>() > operator()() const {
		return m_func();
	}
};

template< typename Func >
auto make_lazy(Func&& func)
	return_ctor( lazy_impl<Func>, ( std::forward<Func>(func) ) )

#define MAKE_LAZY( ... ) make_lazy( [&] { return (__VA_ARGS__); } )
