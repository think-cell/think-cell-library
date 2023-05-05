
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/functors.h"
#include "../base/return_decltype.h"
#include "../base/casts.h"
#include "../base/derivable.h"
#include "../base/assign.h"

#include <utility>

namespace tc {

template<typename Value, typename Accumulate>
struct [[nodiscard]] FAccumulator final : tc::derivable_t<tc::decay_t<Value>> {
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
auto make_accumulator(Value&& value, Accumulate&& accumulate)
	return_ctor_noexcept( TC_FWD(FAccumulator<Value, Accumulate>), (std::forward<Value>(value),std::forward<Accumulate>(accumulate)) )

} // namespace tc
