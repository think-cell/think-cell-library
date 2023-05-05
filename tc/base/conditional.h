
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits_fwd.h"

// used in an expression, prvalue is forwarded as xvalue

#define TYPED_CONDITIONAL(b, type, lhs, rhs) \
	(tc::explicit_cast<bool>(b) ? static_cast<type>(lhs) : static_cast<type>(rhs)) // static_cast needed for conversion from const& to const&&

#define CONDITIONAL_RVALUE_AS_REF(b, lhs, rhs) \
	TYPED_CONDITIONAL(TC_FWD(b), TC_FWD(tc::common_reference_xvalue_as_ref_t<decltype((lhs))&&, decltype((rhs))&&>), TC_FWD(lhs), TC_FWD(rhs))

#define CONDITIONAL_PRVALUE_AS_VAL(b, lhs, rhs) \
	TYPED_CONDITIONAL(TC_FWD(b), TC_FWD(tc::common_reference_prvalue_as_val_t<decltype((lhs)), decltype((rhs))>), TC_FWD(lhs), TC_FWD(rhs))
