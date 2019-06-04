
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

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
		return_ctor( transform_adaptor<tc::decay_t<Func> BOOST_PP_COMMA() Rng >, (std::forward<Rng>(rng),std::forward<Func>(func)) )
}

