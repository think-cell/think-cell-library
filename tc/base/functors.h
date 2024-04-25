
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "invoke.h"

#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/control/if.hpp>

#include <cmath>
#include <cstdlib>
#include <algorithm>

////////////////////////////////
// functor equivalents for operators, free functions and member functions

#define DEFINE_FN2( func, name ) \
	struct [[nodiscard]] name { \
		template< typename... Args > \
		constexpr auto operator()( Args&& ... args) /*no &*/ const \
			return_decltype_allow_xvalue_MAYTHROW( func(tc_move_if_owned(args)...) ) \
		using is_transparent=void; \
	};

#define DEFINE_MEM_FN_BODY_( ... ) \
	template< typename O, typename... __A > \
	constexpr auto operator()( O&& o, __A&& ... __a ) const \
		return_decltype_allow_xvalue_MAYTHROW( tc_move_if_owned(o) __VA_ARGS__ ( tc_move_if_owned(__a)... ) )

// std::mem_fn (C++11 standard 20.8.2) knows the type it can apply its member function pointer to and
// dereferences via operator* until it reaches that something of that type. We cannot do that because
// we have no type. Merely checking for the presence of the member function name is dangerous because
// changes in otherwise unrelated types (the iterator and its pointee) would influence the lookup. 
// Instead we distinguish between shallow member access, implemented in dot_mem_fn, and deep 
// member access via operator->, with the small addition that if operator-> does not exist, we use the
// the dot operator, in the spirit of dereferencing as much as possible.

#define DEFINE_MEM_FN_AUTO_BODY_( ... ) \
	template< typename O, typename... __A, std::enable_if_t<tc::has_operator_arrow<O>::value>* = nullptr > \
	constexpr auto operator()( O&& o, __A&& ... __a ) const \
		return_decltype_allow_xvalue_MAYTHROW( tc_move_if_owned(o)-> __VA_ARGS__ ( tc_move_if_owned(__a)... ) ) \
	template< typename O, typename... __A, std::enable_if_t<!tc::has_operator_arrow<O>::value>* = nullptr > \
	constexpr auto operator()( O&& o, __A&& ... __a ) const \
		return_decltype_allow_xvalue_MAYTHROW( tc_move_if_owned(o). __VA_ARGS__ ( tc_move_if_owned(__a)... ) )

// When a functor must be declared in class-scope, e.g., to access a protected member function,
// you should use DEFINE_MEM_FN instead of tc_define_fn. 
// tc_define_fn always has to define a function named "void func(define_fn_dummy_t)" which may
// shadow inherited members named func.
#define DEFINE_MEM_FN( func ) \
	struct [[nodiscard]] dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .func ) \
	}; \
	struct [[nodiscard]] mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( func ) \
	};

#define tc_define_fn( func ) \
	static void func(tc::define_fn_dummy_t) noexcept = delete; \
	DEFINE_FN2( func, fn_ ## func ) \
	DEFINE_MEM_FN( func )

#define MEM_FN_TEMPLATE_DECLARATION(r, data, i, elem) BOOST_PP_COMMA_IF(i) elem T##i
#define MEM_FN_TEMPLATE_INVOCATION(r, data, i, elem) BOOST_PP_COMMA_IF(i) T##i

#define DEFINE_FN2_TMPL( func, tmpl ) \
    template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
    DEFINE_FN2( func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) >, fn_ ## func )

// See comments above for DEFINE_MEM_FN
#define DEFINE_MEM_FN_TMPL( func, tmpl ) \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct [[nodiscard]] dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	}; \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct [[nodiscard]] mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	};

#define DEFINE_FN_TMPL( func, tmpl ) \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	static void func(tc::define_fn_dummy_t) noexcept = delete; \
	DEFINE_FN2_TMPL( func, tmpl ) \
	DEFINE_MEM_FN_TMPL( func, tmpl )


#define tc_fn(...) ([](auto&&... args) return_decltype_allow_xvalue_MAYTHROW(__VA_ARGS__(tc_move_if_owned(args)...)))

namespace tc {
	constexpr bool is_chained_member_access_expression(char const* const strExpr) noexcept {
		// This is a heuristic with false positives (e.g. complex template parameters).
		auto nArrowCount = 0;
		for (auto it = strExpr; *it; ++it) {
			if ((')' == it[0] || ']' == it[0]) && ('.' == it[1] || ('-' == it[1] && '>' == it[2]) || '[' == it[1] || '(' == it[1])) {
				// We access the result of a function call, which might be a prvalue.
				return true;
			} else if ('-' == it[0] && '>' == it[1]) {
				++nArrowCount;
				if (1 < nArrowCount) {
					// Chaining arrows is problematic, as an overloaded operator-> might create a temporary.
					// (Chaining dot is fine as it's not overloadable).
					return true;
				}
			}
		}
		return false;
	}

	namespace no_adl {
		template <typename Return, bool bIsChainedMemberAccessExpression, typename Obj>
		struct mem_fn_return_type {
			using type = Return;
		};
		template <typename Return, bool bIsChainedMemberAccessExpression, typename Obj>
		struct mem_fn_return_type<Return&&, bIsChainedMemberAccessExpression, Obj> {
			static_assert(!bIsChainedMemberAccessExpression, "tc_mem_fn/tc_member cannot contain chained member function calls that ultimately return an xvalue");
			using type = tc::rewrap_temporary_t<Return&&, Obj>;
		};
	}
	template <typename Return, bool bIsChainedMemberAccessExpression, typename Obj>
	using mem_fn_return_type_t = typename no_adl::mem_fn_return_type<Return, bIsChainedMemberAccessExpression, Obj>::type;

#define tc_mem_fn_impl(bIsChained, Obj, ...) noexcept(noexcept(__VA_ARGS__)) -> tc::mem_fn_return_type_t<decltype((__VA_ARGS__)), bIsChained, Obj> { \
	return __VA_ARGS__; \
}
}

#define tc_mem_fn(...) ([](auto&& _, auto&&... args) tc_mem_fn_impl(tc::is_chained_member_access_expression(#__VA_ARGS__), decltype(_), \
	tc_unwrap_temporary(tc_move_if_owned(_))__VA_ARGS__(tc_move_if_owned(args)...) \
))

#define tc_member(...) ([](auto&& _) tc_mem_fn_impl(tc::is_chained_member_access_expression(#__VA_ARGS__), decltype(_), \
	tc_unwrap_temporary(tc_move_if_owned(_))__VA_ARGS__ \
))

namespace tc {
	namespace no_adl {
		template <typename... F>
		struct [[nodiscard]] TC_EMPTY_BASES overload : std::remove_cvref_t<F>... {
			using std::remove_cvref_t<F>::operator()...;
			constexpr overload(F&&... f) noexcept : std::remove_cvref_t<F>(tc_move_if_owned(f))... {}
		};
	}
	using no_adl::overload;

	template <typename... F>
	[[nodiscard]] constexpr auto make_overload(F&&... f) noexcept {
		return overload<F...>(tc_move_if_owned(f)...);
	}
}

DEFINE_FN2( operator delete, fn_operator_delete )

// Cannot use DEFINE_FN2_TMPL here, since casts are build in and the compiler
//(both clang and MSVC) does not accept passing a parameter pack to *_cast
#pragma push_macro("DEFINE_CAST_")
#define DEFINE_CAST_(name, constexpr_) \
	namespace no_adl { \
		template <typename To> \
		struct [[nodiscard]] fn_ ## name final { \
			template<typename From> \
			constexpr_ auto operator()(From&& from) const return_decltype_allow_xvalue_MAYTHROW( tc_rewrap_temporary(From, name<To>(tc_unwrap_temporary(tc_move_if_owned(from)))) )  \
		}; \
	} \
	using no_adl::fn_ ## name;

namespace tc {
	DEFINE_CAST_(static_cast, constexpr)
	DEFINE_CAST_(reinterpret_cast, /*constexpr*/)
	DEFINE_CAST_(const_cast, constexpr)
}

#pragma pop_macro("DEFINE_CAST_")

namespace tc {
	namespace no_adl {
		struct [[nodiscard]] fn_subscript final {
			template <typename Lhs, typename Rhs>
			constexpr auto operator()( Lhs&& lhs, Rhs&& rhs ) const&
				return_decltype_allow_xvalue_MAYTHROW( tc_rewrap_temporary(Lhs, tc_unwrap_temporary(tc_move_if_owned(lhs))[tc_unwrap_temporary(tc_move_if_owned(rhs))]) )
		};
	}
	using no_adl::fn_subscript;

#pragma push_macro("PREFIX_FN_")
#define PREFIX_FN_( name, op ) \
	namespace no_adl { \
		struct [[nodiscard]] fn_ ## name final { \
			template <typename T> \
			constexpr auto operator()(T&& t ) const \
				return_decltype_allow_xvalue_MAYTHROW( tc_rewrap_temporary(T, op tc_unwrap_temporary(tc_move_if_owned(t))) ) \
		}; \
	} \
	using no_adl::fn_ ## name;

	PREFIX_FN_( logical_not, ! )
	PREFIX_FN_( indirection, * )
	PREFIX_FN_( increment, ++ )
	PREFIX_FN_( decrement, -- )

#pragma pop_macro("PREFIX_FN_")

#pragma push_macro("INFIX_FN_")
#define INFIX_FN_( name, op ) \
	namespace no_adl { \
		struct [[nodiscard]] fn_ ## name final { \
			template <typename Lhs, typename Rhs> \
			constexpr auto operator()(Lhs&& lhs, Rhs&& rhs ) const& \
				return_decltype_allow_xvalue_MAYTHROW( tc_rewrap_temporary(TC_FWD(Lhs, Rhs), tc_unwrap_temporary(tc_move_if_owned(lhs)) op tc_unwrap_temporary(tc_move_if_owned(rhs))) ) \
		}; \
	} \
	using no_adl::fn_ ## name;

	INFIX_FN_( bit_or, | )
	INFIX_FN_( bit_and, & )
	INFIX_FN_( bit_xor, ^ )
	INFIX_FN_( plus, + )
	INFIX_FN_( minus, - )
	INFIX_FN_( mul, * )
	INFIX_FN_( logical_or, || )
	INFIX_FN_( logical_and, && )
	// tc::fn_div defined in round.h

#pragma pop_macro("INFIX_FN_")

#pragma push_macro("ASSIGN_FN_")
#define ASSIGN_FN_( name, op ) \
	namespace no_adl { \
		struct [[nodiscard]] fn_ ## name final { \
			template <typename Lhs, typename Rhs> \
			constexpr auto operator()(Lhs&& lhs, Rhs&& rhs ) const& \
				return_decltype_allow_xvalue_MAYTHROW( tc_rewrap_temporary(Lhs, tc_unwrap_temporary(tc_move_if_owned(lhs)) op tc_unwrap_temporary(tc_move_if_owned(rhs))) ) \
		}; \
	} \
	using no_adl::fn_ ## name;

	ASSIGN_FN_( assign_bit_or, |= )
	ASSIGN_FN_( assign_bit_and, &= )
	ASSIGN_FN_( assign_bit_xor, ^= )
	ASSIGN_FN_( assign_plus, += )
	ASSIGN_FN_( assign_minus, -= )
	ASSIGN_FN_( assign_mul, *= )
	// tc::fn_assign_div defined in round.h
	ASSIGN_FN_( assign, = )

#pragma pop_macro("ASSIGN_FN_")

#pragma push_macro("FN_LOGICAL_")
#define FN_LOGICAL_(name, op) \
	namespace no_adl { \
		struct [[nodiscard]] fn_assign_logical_ ## name { \
			template <typename Lhs, typename Rhs> \
			constexpr decltype(auto) operator()(Lhs&& lhs, Rhs&& rhs) const& noexcept(noexcept(tc_unwrap_temporary(lhs) = tc_unwrap_temporary(tc_move_if_owned(lhs)) op tc_unwrap_temporary(tc_move_if_owned(rhs)))) { \
				tc_unwrap_temporary(lhs) = tc_unwrap_temporary(tc_move_if_owned(lhs)) op tc_unwrap_temporary(tc_move_if_owned(rhs)); \
				return tc_move_if_owned(lhs); \
			} \
		}; \
	} \
	using no_adl::fn_assign_logical_ ## name;

	FN_LOGICAL_(or, ||)
	FN_LOGICAL_(and, &&)

#pragma pop_macro("FN_LOGICAL_")
}
