#pragma once

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
	// prevent wrong include order
	#include "Library/ErrorReporting/assert_fwd.h"
	#include "Library/Utilities/assign.h"
	#include "Library/ErrorReporting/functors.h"
#endif

#include "Library/Utilities/perfect_forward.h"
#include "Library/Utilities/inherit_ctors.h"
#include "Library/Utilities/decltype_return.h"
#include "Library/Utilities/casts.h"

#include <boost/utility/enable_if.hpp>

#include <utility>

////////////////////////////
// fundamental_base

template< typename Base, typename Enable=void >
struct fundamental_base;

template<>
struct fundamental_base<void> {};

template<typename Base>
struct fundamental_base<Base,typename boost::disable_if< std::is_class< Base > >::type> {
	operator Base const&() const {
		return _get();
	}

	operator Base &() {
		return _get();
	}

private:
	Base m_base;

public:
	auto operator!() const
		return_decltype( !m_base )

protected:
	fundamental_base() {}

	template< typename A1 >
	fundamental_base(A1&& a1)
	:	m_base(std::forward<A1>(a1))
	{}

	Base const& _get() const {
		return m_base;
	}

	Base & _get() {
		return m_base;
	}
};

template<typename Base>
struct fundamental_base<Base,typename boost::enable_if< std::is_class< Base > >::type>
	: public Base
{
protected:
	Base const& _get() const {
		return tc::base_cast<Base>(*this);
	}

	Base & _get() {
		return tc::base_cast<Base>(*this);
	}

public:
	fundamental_base() {}
	INHERIT_CTORS(fundamental_base, Base);
};

template<typename Value, typename Accumulate>
struct FAccumulator : fundamental_base< typename std::decay<Value>::type > {
	typedef fundamental_base< typename std::decay<Value>::type > fundamental_base;
	typedef void result_type;

	FAccumulator(Value && value, Accumulate && accumulate)
	:	fundamental_base( std::forward<Value>(value) )
	,	m_accumulate(std::forward<Accumulate>(accumulate))
	{}

	#define PART1() \
		template<
	#define PART2() \
		> void operator() (
	#define PART3() ) { \
			m_accumulate( this->_get(), 
	#define PART4() ); \
		}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4

private:
	typename std::decay<Accumulate>::type m_accumulate;
};

///////////////////////////////////////////////
// accumulators

template<typename Value, typename Accumulate>
auto make_accumulator(Value && value, Accumulate && accumulate)
	return_ctor( FAccumulator<Value BOOST_PP_COMMA() Accumulate>, (std::forward<Value>(value),std::forward<Accumulate>(accumulate)) )

///////////////////////////////////
// chaining N-ary function to unary function,
// add operator() with more arguments if you need it

template<typename R, typename Func, typename Transform>
class chain_impl
	: public fundamental_base< typename std::decay<Func>::type >
{
private:
	typedef fundamental_base< typename std::decay<Func>::type > fundamental_base;
	typename std::decay<Transform>::type const m_transform;
public:
	typedef R result_type; // TODO: replace by return_decltype as soon as compiler supports it

	chain_impl(Func&& func, Transform&& transform)
	:	fundamental_base(std::forward<Func>(func)),
		m_transform(std::forward<Transform>(transform))
	{}

#define PART1() \
	template<
#define PART2() \
	> auto operator()(
#define PART3() ) const \
		->result_type \
	{ \
		return this->_get()( m_transform(
#define PART4() ) ); \
	}
PERFECT_FORWARD
#undef PART1
#undef PART2
#undef PART3
#undef PART4

#define PART1() \
	template<
#define PART2() \
	> auto operator()(
#define PART3() ) \
		->result_type \
	{ \
		return this->_get()( m_transform(
#define PART4() ) ); \
	}
PERFECT_FORWARD
#undef PART1
#undef PART2
#undef PART3
#undef PART4
};

template< typename result_type, typename Func, typename Transform >
chain_impl<result_type, Func, Transform> chain( Func&& func, Transform&& transform ) {
	return chain_impl<result_type, Func, Transform>( std::forward<Func>(func), std::forward<Transform>(transform) );
}

