
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "assign.h"
#include "functors.h"
#include "inherit_ctors.h"
#include "return_decltype.h"
#include "casts.h"
#include "derivable.h"

#include <utility>

namespace tc {

template<typename Value, typename Accumulate>
struct FAccumulator final : tc::derivable_t<tc::decay_t<Value>> {
	using value_t = tc::decay_t<Value>;

	FAccumulator(Value&& value, Accumulate&& accumulate) noexcept
	:	tc::derivable_t<value_t>(std::forward<Value>(value))
	,	m_accumulate(std::forward<Accumulate>(accumulate))
	{}

	template<typename... Args>
	void operator() (Args&& ... args) & noexcept {
		m_accumulate( tc::base_cast<value_t>(*this), std::forward<Args>(args)... );
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