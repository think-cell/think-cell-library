//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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

#include "range_defines.h"

////////////////////////////////
// functor equivalents for operators, free functions and member functions

#include "tc_move.h"
#include "return_decltype.h"

#include <boost/type_traits/has_dereference.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/range/begin.hpp>


#include <cmath>
#include <cstdlib>

#pragma warning(push)
#pragma warning( disable: 4267 )
// warning C4267 : 'argument' : conversion from 'size_t' to 'int', possible loss of data
// _Median(...) causes warning C4267 when difference_type is int and size_t is 64 bit. 
// Stephan T. Lavavej [stl@exchange.microsoft.com] agrees this is a bug and filed DevDiv#1213041 
// "<algorithm>: _Median() doesn't handle fancy difference types" to track the problem.
#include <algorithm>
#pragma warning(pop)

// DEFINE_FN(func) always defines a function void func(define_fn_dummy)
// If that function did not exist, -> decltype( func(...) ) would not be
// a valid statement and clang complains about that.
struct define_fn_dummy final {};

#define DEFINE_FN2( func, name )                                                                      \
	struct name {                                                                                     \
		template< typename A0, typename ...Args >     /*require at least one argument*/               \
		auto operator()( A0&& a0, Args&& ... args) const                                            \
			return_decltype_rvalue_by_ref( func(std::forward<A0>(a0), std::forward<Args>(args)...) ) \
	};

#define DEFINE_MEM_FN_BODY_( ... )                                                               \
	template< typename O, typename ...__A >                                                           \
	auto operator()( O&& o, __A&& ... __a ) const                                                   \
		return_decltype_rvalue_by_ref( std::forward<O>(o) __VA_ARGS__ ( std::forward<__A>(__a)... ) )

// std::mem_fn (C++11 standard 20.8.2) knows the type it can apply its member function pointer to and
// dereferences via operator* until it reaches that something of that type. We cannot do that because
// we have no type. Merely checking for the presence of the member function name is dangerous because
// changes in otherwise unrelated types (the iterator and its pointee) would influence the lookup. 
// Instead we distinguish between shallow member access, implemented in dot_mem_fn, and deep 
// member access via operator->, with the small addition that if operator-> does not exist, we use the
// the dot operator, in the spirit of dereferencing as much as possible.

#define DEFINE_MEM_FN_AUTO_BODY_( ... )                                                                      \
	template< typename O, typename ...__A, std::enable_if_t<boost::has_dereference<O>::value>* = nullptr >   \
	auto operator()( O&& o, __A&& ... __a ) const                                                          \
		return_decltype_rvalue_by_ref( std::forward<O>(o)-> __VA_ARGS__ ( std::forward<__A>(__a)... ) )      \
	template< typename O, typename ...__A, std::enable_if_t<!boost::has_dereference<O>::value>* = nullptr >  \
	auto operator()( O&& o, __A&& ... __a ) const                                                          \
		return_decltype_rvalue_by_ref( std::forward<O>(o). __VA_ARGS__ ( std::forward<__A>(__a)... ) )

// When a functor must be declared in class-scope, e.g., to access a protected member function,
// you should use DEFINE_MEM_FN instead of DEFINE_FN. 
// DEFINE_FN always has to define a function named "void func(define_fn_dummy)" which may
// shadow inherited members named func.
#define DEFINE_MEM_FN( func ) \
	struct dot_member_ ## func { \
		template< typename O > auto operator()( O & o ) const return_variable_by_ref( o.func )       \
		template< typename O > auto operator()( O const& o ) const return_variable_by_ref( o.func )  \
		template< typename O, std::enable_if_t<!std::is_reference<O>::value>* = nullptr >            \
		auto operator()( O&& o ) const return_decltype_rvalue_by_ref( (tc_move(o).func) )     \
	}; \
	std::true_type returns_reference_to_argument(dot_member_ ## func); /*mark as returning reference to argument*/ \
	struct dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .func ) \
	}; \
	struct mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( func ) \
	};

#define DEFINE_FN( func ) \
	static void func(define_fn_dummy) noexcept; \
	DEFINE_FN2( func, fn_ ## func ) \
	DEFINE_MEM_FN( func )

#define MEM_FN_TEMPLATE_DECLARATION(r, data, i, elem) BOOST_PP_IF(i,BOOST_PP_COMMA,BOOST_PP_EMPTY)() elem T##i
#define MEM_FN_TEMPLATE_INVOCATION(r, data, i, elem) BOOST_PP_IF(i,BOOST_PP_COMMA,BOOST_PP_EMPTY)() T##i

#define DEFINE_FN2_TMPL( func, tmpl ) \
    template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
    DEFINE_FN2( func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) >, fn_ ## func )

// See comments above for DEFINE_MEM_FN
#define DEFINE_MEM_FN_TMPL( func, tmpl ) \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	}; \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	};

#define DEFINE_FN_TMPL( func, tmpl ) \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	static void func(define_fn_dummy) noexcept; \
	DEFINE_FN2_TMPL( func, tmpl ) \
	DEFINE_MEM_FN_TMPL( func, tmpl )

DEFINE_FN2( std::abs, fn_std_abs );
DEFINE_FN2( operator delete, fn_operator_delete )

DEFINE_FN( first );
DEFINE_FN( second );

DEFINE_FN2( boost::begin, fn_boost_begin );
template<int N> DEFINE_FN2( std::get<N>, fn_std_get );

// Cannot use DEFINE_FN2_TMPL here, since casts are build in and the compiler
//(both clang and MSVC) does not accept passing a parameter pack to *_cast
#pragma push_macro("DEFINE_CAST_")
#define DEFINE_CAST_(name)                                          \
	template <typename To>                                          \
	struct fn_ ## name {                                            \
		template<typename From> auto operator()(From&& from) const \
			return_decltype( name <To>(std::forward<From>(from)))   \
	};

DEFINE_CAST_(static_cast)
DEFINE_CAST_(reinterpret_cast)
DEFINE_CAST_(const_cast)

#pragma pop_macro("DEFINE_CAST_")

namespace tc {
	struct fn_subscript final {
		template<typename Lhs, typename Rhs>
		auto operator()( Lhs&& lhs, Rhs&& rhs ) const& noexcept
			return_decltype_rvalue_by_ref( std::forward<Lhs>(lhs)[std::forward<Rhs>(rhs)] )
	};

	/* indirection operator, aka dereference operator */
	struct fn_indirection final {
		template<typename Lhs>
		auto operator()( Lhs&& lhs ) const& noexcept
			return_decltype_rvalue_by_ref( *std::forward<Lhs>(lhs))
	};
	std::true_type returns_reference_to_argument(fn_indirection) noexcept; // mark as returning reference to argument

	struct fn_logical_not final {
		template<typename Lhs>
		auto operator()( Lhs&& lhs ) const
			return_decltype_rvalue_by_ref( !std::forward<Lhs>(lhs))
	};

	namespace overload_adl_barrier {
		template<typename Result, typename F1, typename... Fs>
		struct overload : tc::decay_t<F1>, overload<Result, Fs...>
		{
			using tc::decay_t<F1>::operator();
			using overload<Result, Fs...>::operator();

			overload(F1&& f1, Fs&& ... fs) noexcept : tc::decay_t<F1>(std::forward<F1>(f1)), overload<Result, Fs...>(std::forward<Fs>(fs)...) {}
		};

		template<typename Result, typename F1>
		struct overload<Result, F1> : tc::decay_t<F1>
		{
			// Define result_type instead of deriving from boost::static_visitor<Result> to avoid ADL for boost.
			using result_type = Result;

			using tc::decay_t<F1>::operator();

			overload(F1&& f1) noexcept : tc::decay_t<F1>(std::forward<F1>(f1)) {}
		};
	}

	// C++17 deduction guide needed for class template argument deduction as replacement for make_overload. Note: ctor overload(F1&& f1) is not a universal reference by default.
	// template<typename Func>
	// overload(Func&&) noexcept -> overload<Func>;

	template <typename Result, typename... F>
	overload_adl_barrier::overload<Result, F...> make_overload(F&& ... f) noexcept {
		return overload_adl_barrier::overload<Result, F...>(std::forward<F>(f)...) ;
	}
}

#pragma push_macro("INFIX_FN_")
#define INFIX_FN_( name, op ) \
	struct fn_ ## name { \
		template<typename Lhs, typename Rhs> \
		auto operator()( Lhs&& lhs, Rhs&& rhs ) const \
			return_decltype_rvalue_by_ref(std::forward<Lhs>(lhs) op std::forward<Rhs>(rhs)) \
	};

INFIX_FN_( bit_or, | )
INFIX_FN_( bit_and, & )
INFIX_FN_( assign_bit_or, |= )
INFIX_FN_( assign_bit_and, &= )
INFIX_FN_( plus, + )
INFIX_FN_( minus, - )
INFIX_FN_( assign_plus, += )
INFIX_FN_( assign_minus, -= )
INFIX_FN_( assign, = )

#pragma pop_macro("INFIX_FN_")

#define ALLOW_NOEXCEPT( ... ) \
	decltype(static_cast<__VA_ARGS__>(nullptr))