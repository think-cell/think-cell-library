#include "../../../base/assert_defs.h"
#include "grammar.hpp"

auto const grammar_def = x3::int_;

BOOST_SPIRIT_DEFINE(grammar)
BOOST_SPIRIT_INSTANTIATE(grammar_type, char const*, TC_FWD(
	x3::context<
		x3::expectation_failure_tag,
		std::optional<x3::expectation_failure<char const*>>,
		x3::unused_type
	>
))
