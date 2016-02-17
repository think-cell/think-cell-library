#pragma once

#include "range_defines.h"
#include "range_fwd.h"

#include "range_adaptor.h"
#include "meta.h"

#include "tc_move.h"

namespace RANGE_PROPOSAL_NAMESPACE {
	template<typename Rng, typename Func>
	auto transform(Rng&& rng, Func&& func)
		return_ctor( transform_adaptor<std::decay_t<Func> BOOST_PP_COMMA() typename range_by_value<Rng>::type >, (std::forward<Rng>(rng),std::forward<Func>(func)) )

	template<typename Rng>
	auto transform_asciiupper(Rng const& rng)
		return_decltype( transform( rng, tc::fn_toasciiupper() ))

	template<typename Rng>
	auto transform_asciilower(Rng const& rng)
		return_decltype( transform( rng, tc::fn_toasciilower() ))
}

