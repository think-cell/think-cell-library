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

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
	// prevent wrong include order
	#include "Library/ErrorReporting/assert_fwd.h"
	#include "assign.h"
	#include "functors.h"
#endif

#include "inherit_ctors.h"
#include "return_decltype.h"
#include "casts.h"

#include <utility>

namespace tc {

////////////////////////////
// fundamental_base

template< typename Base, typename Enable=void >
struct fundamental_base;

template<>
struct fundamental_base<void> {};

template<typename Base>
struct fundamental_base<Base,std::enable_if_t<!std::is_class< Base >::value >> {
	operator Base const&() const& noexcept {
		return _get();
	}

	operator Base &() & noexcept {
		return _get();
	}

private:
	Base m_base;

public:
	fundamental_base() noexcept {}

protected:
	template< typename A1 >
	fundamental_base(A1&& a1) noexcept
	:	m_base(std::forward<A1>(a1))
	{}

	Base const& _get() const& noexcept {
		return m_base;
	}

	Base & _get() & noexcept {
		return m_base;
	}
};

template<typename Base>
struct fundamental_base<Base,std::enable_if_t< std::is_class< Base >::value >>
	: public Base
{
protected:
	Base const& _get() const& noexcept {
		return tc::base_cast<Base>(*this);
	}

	Base & _get() & noexcept {
		return tc::base_cast<Base>(*this);
	}

public:
	fundamental_base() noexcept {}
	INHERIT_CTORS(fundamental_base, Base);
};

template<typename Value, typename Accumulate>
struct FAccumulator final : fundamental_base< tc::decay_t<Value> > {
	using fundamental_base = fundamental_base< tc::decay_t<Value> >;

	FAccumulator(Value&& value, Accumulate&& accumulate) noexcept
	:	fundamental_base( std::forward<Value>(value) )
	,	m_accumulate(std::forward<Accumulate>(accumulate))
	{}

	template<typename... Args>
	void operator() (Args&& ... args) & noexcept {
		m_accumulate(this->_get(), std::forward<Args>(args)...);
	}

private:
	tc::decay_t<Accumulate> m_accumulate;
};

///////////////////////////////////////////////
// accumulators

template<typename Value, typename Accumulate>
auto make_accumulator(Value&& value, Accumulate&& accumulate) noexcept
	return_ctor( FAccumulator<Value BOOST_PP_COMMA() Accumulate>, (std::forward<Value>(value),std::forward<Accumulate>(accumulate)) )

} // namespace tc