#pragma once

////////////////////////////////
// functor equivalents for operators, free functions and member functions

#include <cstdlib>

#include <boost/type_traits/has_dereference.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/control/if.hpp>
#include "Library/Utilities/decltype_return.h"

#include "tc_move.h"

// DEFINE_FN(func) always defines a function void func(define_fn_dummy)
// If that function did not exist, -> decltype( func(...) ) would not be
// a valid statement and clang complains about that.
struct define_fn_dummy {};

#define DEFINE_FN2( func, name )                                                                      \
	struct name {                                                                                     \
		name& operator=(name const&){ return *this; } /*needed for use as deleter in unique_ptr*/     \
		template< typename A0, typename ...Args >     /*require at least one argument*/               \
		auto operator()( A0 && a0, Args && ... args) const                                            \
			return_decltype( func(std::forward<A0>(a0), std::forward<Args>(args)...) )                \
};

#define DEFINE_MEM_FN_BODY_( ... )                                                               \
	template< typename O, typename ...__A >                                                           \
	auto operator()( O && o, __A && ... __a ) const                                                   \
		return_decltype( std::forward<O>(o) __VA_ARGS__ ( std::forward<__A>(__a)... ) )

// boost::mem_fn (C++11 standard 20.8.2) knows the type it can apply its member function pointer to and
// dereferences via operator* until it reaches that something of that type. We cannot do that because
// we have no type. Merely checking for the presence of the member function name is dangerous because
// changes in otherwise unrelated types (the iterator and its pointee) would influence the lookup. 
// Instead we distinguish between shallow member access, implemented in dot_mem_fn, and deep 
// member access via operator->, with the small addition that if operator-> does not exist, we use the
// the dot operator, in the spirit of dereferencing as much as possible.

#define DEFINE_MEM_FN_AUTO_BODY_( ... )                                                             \
	template< typename O, typename ...__A >                                                           \
	auto operator()( O && o, __A && ... __a ) const                                                   \
		enable_if_return_decltype( boost::has_dereference<O>::value,                                  \
								   std::forward<O>(o)-> __VA_ARGS__ ( std::forward<__A>(__a)... ) )           \
	template< typename O, typename ...__A >                                                           \
	auto operator()( O && o, __A && ... __a ) const                                                   \
		enable_if_return_decltype( !boost::has_dereference<O>::value,                                 \
								   std::forward<O>(o). __VA_ARGS__ ( std::forward<__A>(__a)... ) )

// When a functor must be declared in class-scope, e.g., to access a protected member function,
// you should use DEFINE_MEM_FN instead of DEFINE_FN. 
// DEFINE_FN always has to define a function named "void func(define_fn_dummy)" which may
// shadow inherited members named func.
#define DEFINE_MEM_FN( func ) \
	struct dot_member_ ## func { \
		template< typename O > auto operator()( O & o ) const return_decltype_ref( o.func ) \
		template< typename O > auto operator()( O const& o ) const return_decltype_ref( o.func ) \
		template< typename O > auto operator()( O&& o ) const enable_if_return_decltype( !std::is_reference<O>::value, tc_move(tc_move(o).func) ) \
	}; \
	struct dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .func ) \
	}; \
	struct mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( func ) \
	};

#define DEFINE_FN( func ) \
	static void func(define_fn_dummy); \
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
	static void func(define_fn_dummy); \
	DEFINE_FN2_TMPL( func, tmpl ) \
	DEFINE_MEM_FN_TMPL( func, tmpl )

DEFINE_FN2( std::max, fn_std_max );
DEFINE_FN2( std::min, fn_std_min );
DEFINE_FN2( std::abs, fn_std_abs );
template<typename T> DEFINE_FN2( T, fn_ctor );
DEFINE_FN2( operator delete, fn_operator_delete )

DEFINE_FN( first );
DEFINE_FN( second );

DEFINE_FN(emplace_back);

// Cannot use DEFINE_FN2_TMPL here, since casts are build in and the compiler
//(both clang and MSVC) does not accept passing a parameter pack to *_cast
#define DEFINE_CAST_(name)                                          \
	template <typename To>                                          \
	struct fn_ ## name {                                            \
		template<typename From> auto operator()(From && from) const \
			return_decltype( name <To>(std::forward<From>(from)))   \
	};

DEFINE_CAST_(static_cast)
DEFINE_CAST_(reinterpret_cast)
DEFINE_CAST_(const_cast)

#undef DEFINE_CAST_

namespace tc {
	struct fn_subscript {
		template<typename Lhs, typename Rhs>
		auto operator()( Lhs && lhs, Rhs && rhs ) const
			return_decltype( std::forward<Lhs>(lhs)[std::forward<Rhs>(rhs)] )
	};
}

struct fn_indirection {
	template<typename Lhs>
	auto operator()( Lhs && lhs ) const
		return_decltype( *std::forward<Lhs>(lhs))
};

namespace tc {
	struct fn_logical_not {
		template<typename Lhs>
		auto operator()( Lhs && lhs ) const
			return_decltype( !std::forward<Lhs>(lhs))
	};
}

#define INFIX_FN_( name, op ) \
	struct fn_ ## name { \
		template<typename Lhs, typename Rhs> \
		auto operator()( Lhs && lhs, Rhs && rhs ) const \
			return_decltype(std::forward<Lhs>(lhs) op std::forward<Rhs>(rhs)) \
	};

INFIX_FN_( bit_or, | )
INFIX_FN_( bit_and, & )
INFIX_FN_( assign_bit_or, |= )
INFIX_FN_( assign_bit_and, &= )
INFIX_FN_( plus, + )
INFIX_FN_( minus, - )
INFIX_FN_( assign_plus, += )
INFIX_FN_( assign_minus, -= )

#undef INFIX_FN_
