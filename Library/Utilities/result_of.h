#pragma once

namespace tc {
	template<typename Func> struct result_of;

	template<typename Func, typename ...Args>
	struct result_of<Func(Args...)> {
		using type = typename std::result_of< typename std::remove_reference<Func>::type(Args...) >::type;
	};
}
