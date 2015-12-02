#pragma once

namespace tc {
	struct noop {
		using result_type = void;
		template<typename ...Args> void operator()(Args const&...) const {}
	};

	template<typename T=void>
	struct never_called {
		template<typename ...Args> T operator()(Args const&...) const {_ASSERTFALSE; return {}; }
	};

	template<>
	struct never_called<void> {
		template<typename ...Args> void operator()(Args const&...) const {_ASSERTFALSE; }
	};

	template<typename T, T tValue>
	struct constexpr_function {
		// using argument_type = *; cannot be provided without giving up convenience of templated operator()
		using result_type = T; // like std::unary_function	

		template< typename ...Args > T operator()( Args const&... ) const { return tValue; }
	};

	#define MAKE_CONSTEXPR_FUNCTION(val) tc::constexpr_function< decltype(val), (val) >()

	struct identity {
		template< typename T >
		T&& operator()(T&& t) const noexcept {
			return std::forward<T>(t);
		}
	};
}