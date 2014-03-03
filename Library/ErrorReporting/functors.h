#pragma once

////////////////////////////////
// functor equivalents for operators, free functions and member functions

#include <boost/type_traits/has_dereference.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/arithmetic/inc.hpp>
#include <boost/preprocessor/repetition/enum_binary_params.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/control/if.hpp>
#include "Library/Utilities/decltype_return.h"

#define MAX_DEFINE_FUNCTOR_PARAMS 10

#define DEFINE_FUNCTOR_FORWARD_ARG_(z, n, _) std::forward< __A ## n >( __a ## n )

#define DEFINE_FN2_OPERATOR_(z, n, func) \
	template< BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename __A) > \
	auto operator()( BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), __A, && __a) ) const \
		-> decltype( func( BOOST_PP_ENUM(BOOST_PP_INC(n), DEFINE_FUNCTOR_FORWARD_ARG_, _) ) ) \
	{ \
		return func( BOOST_PP_ENUM(BOOST_PP_INC(n), DEFINE_FUNCTOR_FORWARD_ARG_, _) ); \
	}

#define DEFINE_FN2( func, name ) \
	struct name { \
		name& operator=(name const&){ return *this; } /*needed for use as deleter in unique_ptr*/ \
		BOOST_PP_REPEAT( MAX_DEFINE_FUNCTOR_PARAMS, DEFINE_FN2_OPERATOR_, func ) \
	};

#define DEFINE_MEM_FN_OPERATOR_(z, n, op_memfn) \
	template< typename O, BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename __A) > \
	auto operator()( O && o, BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), __A, && __a) ) const \
		return_decltype( std::forward<O>(o)op_memfn( BOOST_PP_ENUM(BOOST_PP_INC(n), DEFINE_FUNCTOR_FORWARD_ARG_, _) ) )

#define DEFINE_MEM_FN_BODY_( op_memfn ) \
	template< typename O > auto operator()( O && o ) const return_decltype(std::forward<O>(o)op_memfn()) \
	BOOST_PP_REPEAT( MAX_DEFINE_FUNCTOR_PARAMS, DEFINE_MEM_FN_OPERATOR_, op_memfn )

// std::mem_fn (C++11 standard 20.8.2) knows the type it can apply its member function pointer to and
// dereferences via operator* until it reaches that something of that type. We cannot do that because
// we have no type. Merely checking for the presence of the member function name is dangerous because
// changes in otherwise unrelated types (the iterator and its pointee) would influence the lookup. 
// Instead we distinguish between shallow member access, implemented in dot_mem_fn, and deep 
// member access via operator->, with the small addition that if operator-> does not exist, we use the
// the dot operator, in the spirit of dereferencing as much as possible.
#define DEFINE_MEM_FN_AUTO_(z, n, memfn) \
	template< typename O, BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename __A) > \
	auto operator()( O && o, BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), __A, && __a) ) const \
		enable_if_decltype_return( boost::has_dereference<O>::value, std::forward<O>(o)->memfn( BOOST_PP_ENUM(BOOST_PP_INC(n), DEFINE_FUNCTOR_FORWARD_ARG_, _) ) ) \
	template< typename O, BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename __A) > \
	auto operator()( O && o, BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), __A, && __a) ) const \
		enable_if_decltype_return( !boost::has_dereference<O>::value, std::forward<O>(o).memfn( BOOST_PP_ENUM(BOOST_PP_INC(n), DEFINE_FUNCTOR_FORWARD_ARG_, _) ) )

#define DEFINE_MEM_FN_AUTO_BODY_( memfn ) \
	template< typename O > auto operator()( O && o ) const enable_if_decltype_return( boost::has_dereference<O>::value, std::forward<O>(o)->memfn()) \
	template< typename O > auto operator()( O && o ) const enable_if_decltype_return( !boost::has_dereference<O>::value, std::forward<O>(o).memfn()) \
	BOOST_PP_REPEAT( MAX_DEFINE_FUNCTOR_PARAMS, DEFINE_MEM_FN_AUTO_, memfn )

#define DEFINE_FN( func ) \
	DEFINE_FN2( func, fn_ ## func ) \
	struct dot_member_ ## func { \
		template< typename O > auto operator()( O & o ) const decltype_return_ref( o.func ) \
		template< typename O > auto operator()( O const& o ) const decltype_return_ref( o.func ) \
	}; \
	struct dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .func ) \
	}; \
	struct mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( func ) \
	};

#define MEM_FN_TEMPLATE_DECLARATION(r, data, i, elem) BOOST_PP_IF(i,BOOST_PP_COMMA,BOOST_PP_EMPTY)() elem T##i
#define MEM_FN_TEMPLATE_INVOCATION(r, data, i, elem) BOOST_PP_IF(i,BOOST_PP_COMMA,BOOST_PP_EMPTY)() T##i

#define DEFINE_FN2_TMPL( func, tmpl ) \
    template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
    DEFINE_FN2( func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) >, fn_ ## func )

#define DEFINE_FN_TMPL( func, tmpl ) \
	DEFINE_FN2_TMPL( func, tmpl ) \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	}; \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	};

DEFINE_FN2( std::max, fn_std_max );
DEFINE_FN2( std::min, fn_std_min );
DEFINE_FN2( std::abs, fn_std_abs );
template<typename T> DEFINE_FN2( T, fn_ctor );
DEFINE_FN2( operator delete, fn_operator_delete )

DEFINE_FN( assign_max );

DEFINE_FN( assign_min );
DEFINE_FN( assign_better );

namespace tc{
	DEFINE_FN( size )
	DEFINE_FN( strlen )
	DEFINE_FN( toasciilower )
	DEFINE_FN( toasciiupper )
	DEFINE_FN( isasciidigit )
	DEFINE_FN( isasciilower )
	DEFINE_FN( isasciiupper )
}

DEFINE_FN( first );
DEFINE_FN( second );
namespace tc{
	DEFINE_FN( compare );
	DEFINE_FN( lexicographical_compare_3way );
}
DEFINE_FN2_TMPL( static_cast, (typename) );
DEFINE_FN2_TMPL( reinterpret_cast, (typename) );
DEFINE_FN2_TMPL( const_cast, (typename) );

DEFINE_FN( emplace_back );

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
