#pragma once

#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/preprocessor/arithmetic/inc.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/repetition/enum_binary_params.hpp>
#include <boost/preprocessor/seq/elem.hpp>

// REMOVE_BRACKET is a helper to strip an enclosing pair of brackets from a macro argument, for example:
//   #define FOO(X) REMOVE_BRACKET X
//   FOO((some text))
// expands to "some text".
// This is in particular useful to temporarily protect arguments against expansion of BOOST_PP_COMMA() in recursive macros.
#define REMOVE_BRACKET(...) __VA_ARGS__

// The above is wrong you need to do this (STRIP_PARENS)
#define CAT(x, y) CAT_I(x, y) 
#define CAT_I(x, y) x ## y 
#define APPLY(macro, args) APPLY_I(macro, args) 
#define APPLY_I(macro, args) macro args 
#define STRIP_PARENS(x) EVAL((STRIP_PARENS_I x), x) 
#define STRIP_PARENS_I(...) 1,1 
#define EVAL(test, x) EVAL_I(test, x) 
#define EVAL_I(test, x) MAYBE_STRIP_PARENS(TEST_ARITY test, x) 
#define TEST_ARITY(...) APPLY(TEST_ARITY_I, (__VA_ARGS__, 2, 1)) 
#define TEST_ARITY_I(a,b,c,...) c 
#define MAYBE_STRIP_PARENS(cond, x) MAYBE_STRIP_PARENS_I(cond, x) 
#define MAYBE_STRIP_PARENS_I(cond, x) CAT(MAYBE_STRIP_PARENS_, cond)(x) 
#define MAYBE_STRIP_PARENS_1(x) x 
#define MAYBE_STRIP_PARENS_2(x) APPLY(MAYBE_STRIP_PARENS_2_I, x) 
#define MAYBE_STRIP_PARENS_2_I(...) __VA_ARGS__ 

#define MAX_INHERIT_CTOR_PARAMS 10

#define INHERIT_CTORS_FORWARD_ARG_(z, n, text) std::forward< T ## n >( t ## n )

#define INHERIT_CTORS_CTOR_(z, n, seq_derived_and_base)                                                                       \
	template<BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename T) >                                                                 \
	explicit                                                                                  \
	/*TDerived*/STRIP_PARENS( BOOST_PP_SEQ_ELEM(0, seq_derived_and_base) )                                                    \
	(BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), T, && t) ) :                                                                \
	/*TBase*/ STRIP_PARENS ( BOOST_PP_SEQ_ELEM(1, seq_derived_and_base) )                                                     \
	(BOOST_PP_ENUM(BOOST_PP_INC(n), INHERIT_CTORS_FORWARD_ARG_, _) ) {}

// expands to:
//   template< typename T0 > explicit TDerived( T0&& t0): TBase( std::forward<T0>(t0) ) {};
//   template< typename T0, typename T1 > TDerived( T0&& t0, T1&& t1 ): TBase( std::forward<T0>(t0), std::forward<T1>(t1) ) {};
//   ...
// change MAX_INHERIT_CTOR_PARAMS if you need to forward more parameters
#define INHERIT_CTORS( TDerived, TBase ) \
	template< typename T0 > \
	TDerived( T0 && t0, typename std::enable_if< \
		( std::is_same< TBase, typename std::decay<T0>::type >::value || \
		!std::is_base_of< TBase, typename std::decay<T0>::type >::value ) && \
		std::is_convertible< T0 &&, TBase >::value \
	, unused_arg >::type=unused_arg() ) \
	:	TBase( std::forward<T0>(t0) ) {} \
	template< typename T0 > \
	explicit TDerived( T0 && t0, typename std::enable_if< \
		( std::is_same< TBase, typename std::decay<T0>::type >::value || \
		!std::is_base_of< TBase, typename std::decay<T0>::type >::value ) && \
		!std::is_convertible< T0 &&, TBase >::value \
	, unused_arg >::type=unused_arg() ) \
	:	TBase( std::forward<T0>(t0) ) {} \
	template< typename T0, typename T1 > \
	explicit TDerived( T0 && t0, T1 && t1 ) \
	:	TBase( std::forward<T0>(t0),std::forward<T1>(t1) ) {} \
	template< typename T0, typename T1, typename T2 > \
	explicit TDerived( T0 && t0, T1 && t1, T2 && t2 ) \
	:	TBase( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<T2>(t2) ) {} \
	template< typename T0, typename T1, typename T2, typename T3 > \
	explicit TDerived( T0 && t0, T1 && t1, T2 && t2, T3 && t3 ) \
	:	TBase( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<T2>(t2),std::forward<T3>(t3) ) {} \
	template< typename T0, typename T1, typename T2, typename T3, typename T4 > \
	explicit TDerived( T0 && t0, T1 && t1, T2 && t2, T3 && t3, T4 && t4 ) \
	:	TBase( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<T2>(t2),std::forward<T3>(t3),std::forward<T4>(t4) ) {} \
	template< typename T0, typename T1, typename T2, typename T3, typename T4, typename T5 > \
	explicit TDerived( T0 && t0, T1 && t1, T2 && t2, T3 && t3, T4 && t4, T5 && t5 ) \
	:	TBase( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<T2>(t2),std::forward<T3>(t3),std::forward<T4>(t4),std::forward<T5>(t5) ) {} \
	template< typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6 > \
	explicit TDerived( T0 && t0, T1 && t1, T2 && t2, T3 && t3, T4 && t4, T5 && t5, T6 && t6 ) \
	:	TBase( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<T2>(t2),std::forward<T3>(t3),std::forward<T4>(t4),std::forward<T5>(t5),std::forward<T6>(t6) ) {}

#define INHERIT_ASSIGN( TDerived, TBase ) \
	template<typename T> \
	typename std::enable_if< \
		( std::is_same< TBase, typename std::decay<T>::type >::value || \
		!std::is_base_of< TBase, typename std::decay<T>::type >::value ) \
		&& sizeof(TBase)==sizeof(TDerived) \
	, TDerived& >::type operator=( T && t ){ \
		TBase::operator=( std::forward<T>(t) ); \
		return *this; \
	}

#define DEFAULT_MOVE_CTOR( TDerived, TBase ) \
	public: \
		TDerived( TDerived && t ) \
		:	TBase(tc::base_cast<TBase>(tc_move(t))) { \
			static_assert(sizeof(TDerived)==sizeof(TBase),"move other members?"); \
			static_assert( !std::is_base_of< tc::nonmovable, TDerived >::value, "nonmovable" ); \
		} \
	private: \
		TDerived( TDerived const& t ); \
		TDerived& operator=( TDerived && t ); \
		TDerived& operator=( TDerived const& t ); \
	public:

#define DEFAULT_MOVE_CTOR_AND_ASSIGN( TDerived, TBase ) \
	public: \
		TDerived( TDerived && t ) \
		:	TBase(tc::base_cast<TBase>(tc_move(t))) { \
			static_assert(sizeof(TDerived)==sizeof(TBase),"move other members?"); \
			static_assert( !std::is_base_of< tc::nonmovable, TDerived >::value, "nonmovable" ); \
		} \
		TDerived& operator=( TDerived && t ) { \
			static_assert(sizeof(TDerived)==sizeof(TBase),"move-assign other members?"); \
			static_assert( !std::is_base_of< tc::nonmovable, TDerived >::value, "nonmovable" ); \
			TBase::operator=(tc::base_cast<TBase>(tc_move(t))); \
			return *this; \
		} \
	private: \
		TDerived( TDerived const& t ); \
		TDerived& operator=( TDerived const& t ); \
	public:

#define NONMOVABLE( TDerived, TBase ) \
	private: \
		TDerived( TDerived && t ); \
		TDerived( TDerived const& t ); \
		TDerived& operator=( TDerived && t ); \
		TDerived& operator=( TDerived const& t ); \
	public:
