#pragma once

#include "range_defines.h"
#include "range_fwd.h"

#include "range_adaptor.h"
#include "meta.h"

#include "Library/ErrorReporting/tc_move.h"

namespace RANGE_PROPOSAL_NAMESPACE {
	template<typename Rng, typename Func>
	auto transform( Rng && rng, Func && func )
		return_ctor( transform_adaptor<typename std::decay<Func>::type BOOST_PP_COMMA() Rng>, (std::forward<Rng>(rng),std::forward<Func>(func)) )
}

