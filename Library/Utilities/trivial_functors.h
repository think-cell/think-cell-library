#pragma once

struct noop {
	typedef void result_type;
	template<typename ...Args> void operator()(Args const&...) const {}
};

template<typename T, T tValue>
struct constexpr_function {
	// typedef * argument_type; cannot be provided without giving up convenience of templated operator()
	typedef T result_type; // like std::unary_function	

	template< typename ...Args > T operator()( Args const&... ) const { return tValue; }
};

#define MAKE_CONSTEXPR_FUNCTION(val) constexpr_function< decltype(val), (val) >()

struct identity {
	template< typename T >
	T&& operator()( T&& t ) const {
		return std::forward<T>(t);
	}
};

