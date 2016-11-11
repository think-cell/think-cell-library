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

#include "range_defines.h"
#include "range_fwd.h"

#include "range_adaptor.h"
#include "meta.h"
#include "minmax.h"

#include "tc_move.h"

namespace tc {
	template<typename Rng, typename Func>
	auto transform(Rng&& rng, Func&& func) MAYTHROW
		return_ctor( transform_adaptor<tc::decay_t<Func> BOOST_PP_COMMA() view_by_value_t<Rng> >, (std::forward<Rng>(rng),std::forward<Func>(func)) )

	template<typename Rng>
	auto transform_asciiupper(Rng&& rng) noexcept
		return_decltype( transform( std::forward<Rng>(rng), tc::fn_toasciiupper() ))

	template<typename Rng>
	auto transform_asciilower(Rng&& rng) noexcept
		return_decltype( transform( std::forward<Rng>(rng), tc::fn_toasciilower() ))
}

