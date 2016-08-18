//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
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
#include "type_traits.h"

namespace tc {
	template< typename Var, typename Expr>
	struct return_decltype_retval final {
		static_assert( std::is_same< Var, Expr >::value, "choose between return_variable_by_ref and return_by_val" );
		static_assert( !std::is_rvalue_reference< Expr >::value, "choose between return_decltype_rvalue_by_ref and return_decltype_rvalue_by_val");
		using type=Expr;
	};
	template< typename Var, typename Expr>
	using return_decltype_retval_t = typename tc::return_decltype_retval<Var, Expr>::type;

#define return_decltype(...) -> tc::return_decltype_retval_t< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) > { \
	return (__VA_ARGS__); \
}
#define code_return_decltype(code,...) -> tc::return_decltype_retval_t< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) > { \
	code \
	return (__VA_ARGS__); \
}

#define return_by_val(...) -> tc::decay_t<decltype(__VA_ARGS__)> { \
	return (__VA_ARGS__); \
}

	template< typename Var, typename Expr >
	struct return_variable_by_ref_retval final {
		static_assert( std::is_lvalue_reference< Expr >::value, "use return_variable_by_ref only for variables");
		using type=Expr; // rvalue references behave like lvalues
		/* DOES NOT COMPILE
			void foo(int&& n) & noexcept {}
 
			int m;
			int&& n=std::move(m);
			foo(n);
		*/
	};
	template< typename Var, typename Expr >
	using return_variable_by_ref_retval_t = typename tc::return_variable_by_ref_retval< Var, Expr >::type;

#define return_variable_by_ref(...) -> tc::return_variable_by_ref_retval_t< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) > { \
	return (__VA_ARGS__); \
}

	template< typename Var, typename Expr >
	struct return_decltype_rvalue_by_ref_retval final {
		static_assert( std::is_same< Var, Expr >::value, "choose between return_variable_by_ref and return_by_val" );
		using type=Expr;
	};
	template< typename Var, typename Expr >
	using return_decltype_rvalue_by_ref_retval_t = typename tc::return_decltype_rvalue_by_ref_retval< Var, Expr >::type;

#define return_decltype_rvalue_by_ref(...) -> tc::return_decltype_rvalue_by_ref_retval_t< decltype(__VA_ARGS__),decltype((__VA_ARGS__)) > { \
	return (__VA_ARGS__); \
}

	template< typename Expr >
	using lvalue_or_decay_t =
		std::conditional_t<
			std::is_lvalue_reference<Expr>::value,
			Expr,
			tc::decay_t<Expr>
		>;

#ifndef __clang__
	// workaround for compiler bug https://connect.microsoft.com/VisualStudio/feedback/details/1657760
#define return_decltype_rvalue_by_val_variable_by_ref(...) -> decltype(auto) {\
	return boost::implicit_cast<tc::lvalue_or_decay_t< decltype((__VA_ARGS__)) >>((__VA_ARGS__)); \
}
#else
#define return_decltype_rvalue_by_val_variable_by_ref(...) -> tc::lvalue_or_decay_t< decltype((__VA_ARGS__)) > { \
	return (__VA_ARGS__); \
}
#endif
}

namespace decltype_return_test {
	struct A{
		int a;
		void access_a() & noexcept {
			static_assert( std::is_same<decltype(a),int>::value, "");
			static_assert( std::is_same<decltype((a)),int&>::value, "");
		}
		int& b;
		void access_b() & noexcept {
			static_assert( std::is_same<decltype(b),int&>::value, "");
			static_assert( std::is_same<decltype((b)),int&>::value, "");
		}
		int&& c;
		void access_c() & noexcept {
			static_assert( std::is_same<decltype(c),int&&>::value, "");
			static_assert( std::is_same<decltype((b)),int&>::value, "");
		}
	};
}

#define return_ctor(T,...) ->T { return T __VA_ARGS__ ; }
#define code_return_ctor(code, T, ...) ->T { \
	code \
	return T __VA_ARGS__ ; \
}
