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
#include "assign.h"
#include "functors.h"
#include "inherit_ctors.h"
#include "return_decltype.h"
#include "casts.h"

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