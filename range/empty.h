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

#include <utility>
#include "trivial_functors.h"

#include "break_or_continue.h"
#include "range_defines.h"
#include "container_traits.h"

#include <functional>

#include "index_range.h"

namespace tc {
	template<typename Rng, std::enable_if_t<has_mem_fn_empty<Rng const>::value>* = nullptr>
	bool empty(Rng const& rng) noexcept {
		return rng.empty();
	}

	template<typename Rng, std::enable_if_t<
		!has_mem_fn_empty<Rng const>::value &&
		is_range_with_iterators<Rng const>::value &&
		!has_index<Rng>::value>* = nullptr>
	bool empty(Rng const& rng) noexcept {
		return boost::begin(rng)==boost::end(rng);
	}

	template<typename Rng, std::enable_if_t<
		!has_mem_fn_empty<Rng const>::value &&
		is_range_with_iterators<Rng const>::value &&
		has_index<Rng>::value>* = nullptr>
	bool empty(Rng const& rngidx) noexcept {
		return rngidx.at_end_index(rngidx.begin_index());
	}

	template<typename Rng, std::enable_if_t<
		!has_mem_fn_empty<Rng const>::value &&
		!is_range_with_iterators<Rng const>::value>* = nullptr>
	bool empty(Rng const& rng) noexcept {
		return continue_==tc::for_each( rng, MAKE_CONSTEXPR_FUNCTION(break_) );
	}

	DEFINE_FN2(tc::empty, fn_empty) // Prevent ADL of std::empty (C++17)
}
