#pragma once

namespace tc {
	template<typename Func> struct result_of;

	template<typename Func, typename ...Args>
	struct result_of<Func(Args...)> {
		using type = typename std::result_of< typename std::remove_reference<Func>::type(Args...) >::type;
	};

	template< typename T >
	using result_of_t=typename result_of<T>::type;

	template< typename T >
	using result_value_of_t=std::decay_t< tc::result_of_t< T > >;
}
