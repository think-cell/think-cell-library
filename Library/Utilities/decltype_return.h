#pragma once
#include <type_traits>

namespace tc {
	template< typename Var, typename Expr >
	struct return_decltype_retval {
		static_assert( std::is_same< Var, Expr >::value, "Do not use return_decltype for variables" );
		static_assert( !std::is_rvalue_reference< Expr >::value, "Return rvalue by reference or value?");
		using type=typename std::conditional<
			std::is_reference<Expr>::value,
			Expr,
			typename std::decay<Expr>::type
		>::type;
	};
	template< typename Var, typename Expr >
	typename tc::return_decltype_retval< Var, Expr >::type return_decltype_retval_avoid_typename();

	template< typename Var, typename Expr >
	struct return_by_val_retval {
		using type=typename std::decay<Expr>::type;
	};
	template< typename Var, typename Expr >
	typename tc::return_by_val_retval< Var, Expr >::type return_by_val_retval_avoid_typename();

	template< typename Var, typename Expr >
	struct return_variable_by_ref_retval {
		static_assert( std::is_lvalue_reference< Expr >::value, "use return_variable_by_ref only for variables");
		using type=Expr; // rvalue references behave like lvalues
		/* DOES NOT COMPILE
			void foo(int&& n) {}
 
			int m;
			int&& n=std::move(m);
			foo(n);
		*/
	};
	template< typename Var, typename Expr >
	typename tc::return_variable_by_ref_retval< Var, Expr >::type return_variable_by_ref_retval_avoid_typename();

	template< typename Var, typename Expr >
	struct return_decltype_rvalue_by_ref_retval {
		static_assert( std::is_same< Var, Expr >::value, "do not use return_decltype for variables" );
		using type=typename std::conditional<
			std::is_reference<Expr>::value,
			Expr,
			typename std::decay<Expr>::type // lambdas return always by value and std::decay their return type, so for values, we do the same
		>::type;
	};
	template< typename Var, typename Expr >
	typename tc::return_decltype_rvalue_by_ref_retval< Var, Expr >::type return_decltype_rvalue_by_ref_retval_avoid_typename();

	template< typename Var, typename Expr=Var >
	struct return_decltype_rvalue_by_val_retval {
		static_assert( std::is_same< Var, Expr >::value, "do not use return_decltype for variables" );
		using type=typename std::conditional<
			std::is_lvalue_reference<Expr>::value,
			Expr,
			typename std::decay<Expr>::type // lambdas return always by value and std::decay their return type, so for values, we do the same
		>::type;
	};
	template< typename Var, typename Expr >
	typename tc::return_decltype_rvalue_by_val_retval< Var, Expr >::type return_decltype_rvalue_by_val_retval_avoid_typename();
}

// Neither decltype((x)) nor decltype(x) determine the right type in all cases.
// So we static_assert that they agree, and if not, let the user disambiguate.
namespace {
	struct A{
		int a;
		void access_a() {
			static_assert( std::is_same<decltype(a),int>::value, "");
			static_assert( std::is_same<decltype((a)),int&>::value, "");
		}
		int& b;
		void access_b() {
			static_assert( std::is_same<decltype(b),int&>::value, "");
			static_assert( std::is_same<decltype((b)),int&>::value, "");
		}
		int&& c;
		void access_c() {
			static_assert( std::is_same<decltype(c),int&&>::value, "");
			static_assert( std::is_same<decltype((b)),int&>::value, "");
		}
	};
}

#define return_decltype(...) ->decltype( tc::return_decltype_retval_avoid_typename< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) >() ) { \
	return (__VA_ARGS__); \
}
#define return_by_val(...) ->decltype( tc::return_by_val_retval_avoid_typename< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) >() ) { \
	return (__VA_ARGS__); \
}
#define return_variable_by_ref(...) ->decltype( tc::return_variable_by_ref_retval_avoid_typename< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) >() ) { \
	return (__VA_ARGS__); \
}
#define return_decltype_rvalue_by_ref(...) ->decltype( tc::return_decltype_rvalue_by_ref_retval_avoid_typename< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) >() ) { \
	return (__VA_ARGS__); \
}
#define enable_if_return_decltype(cond,...) ->typename std::enable_if< (cond), typename tc::return_decltype_retval< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) >::type >::type { \
	return (__VA_ARGS__); \
}
#define enable_if_return_decltype_rvalue_by_ref(cond,...) -> typename std::enable_if< (cond), typename tc::return_decltype_rvalue_by_ref_retval< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) >::type >::type { \
	return (__VA_ARGS__); \
}
#define code_return_decltype(code,...) ->decltype( tc::return_decltype_retval_avoid_typename< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) >() ) { \
	{code} \
	return (__VA_ARGS__); \
}
#define return_ctor(T,params) ->T { return T params ; }
#define enable_if_decltype_ctor(cond,T,params) -> typename std::enable_if< (cond), T >::type { return T params ; }
