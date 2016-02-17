#pragma once

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
	// prevent wrong include order
	#include "assert_fwd.h"
	#include "assign.h"
	#include "functors.h"
#endif

#include "inherit_ctors.h"
#include "return_decltype.h"
#include "casts.h"

#include <utility>

////////////////////////////
// fundamental_base

template< typename Base, typename Enable=void >
struct fundamental_base;

template<>
struct fundamental_base<void> {};

template<typename Base>
struct fundamental_base<Base,typename std::enable_if<!std::is_class< Base >::value >::type> {
	operator Base const&() const {
		return _get();
	}

	operator Base &() {
		return _get();
	}

private:
	Base m_base;

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
struct fundamental_base<Base,typename std::enable_if< std::is_class< Base >::value >::type>
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
struct FAccumulator : fundamental_base< std::decay_t<Value> > {
	using fundamental_base = fundamental_base< std::decay_t<Value> >;
	using result_type = void;

	FAccumulator(Value&& value, Accumulate&& accumulate)
	:	fundamental_base( std::forward<Value>(value) )
	,	m_accumulate(std::forward<Accumulate>(accumulate))
	{}

	template<typename... Args>
	void operator() (Args&& ... args) {
		m_accumulate(this->_get(), std::forward<Args>(args)...);
	}

private:
	std::decay_t<Accumulate> m_accumulate;
};

///////////////////////////////////////////////
// accumulators

template<typename Value, typename Accumulate>
auto make_accumulator(Value&& value, Accumulate&& accumulate)
	return_ctor( FAccumulator<Value BOOST_PP_COMMA() Accumulate>, (std::forward<Value>(value),std::forward<Accumulate>(accumulate)) )

///////////////////////////////////
// chaining N-ary function to unary function,
// add operator() with more arguments if you need it

template<typename R, typename Func, typename Transform>
struct chain_impl
	: fundamental_base< std::decay_t<Func> >
{
private:
	using fundamental_base = fundamental_base< std::decay_t<Func> >;
	std::decay_t<Transform> const m_transform;
public:
	using result_type = R; // TODO: replace by return_decltype as soon as compiler supports it

	chain_impl(Func&& func, Transform&& transform)
	:	fundamental_base(std::forward<Func>(func)),
		m_transform(std::forward<Transform>(transform))
	{}

	template<typename... Args>
	auto operator()(Args&& ... args) const->result_type
	{
		return this->_get()( m_transform(std::forward<Args>(args)...) );
	}

	template<typename... Args>
	auto operator()(Args&& ... args)->result_type {
		return this->_get()( m_transform(std::forward<Args>(args)...) );
	}
};

template< typename result_type, typename Func, typename Transform >
chain_impl<result_type, Func, Transform> chain(Func&& func, Transform&& transform) {
	return chain_impl<result_type, Func, Transform>( std::forward<Func>(func), std::forward<Transform>(transform) );
}

