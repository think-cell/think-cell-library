
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits_fwd.h"

// used in an expression, prvalue is forwarded as xvalue

#define tc_conditional_impl(b, type, lhs, rhs) \
	(tc::explicit_cast<bool>(b) ? static_cast<type>(lhs) : static_cast<type>(rhs)) // static_cast needed for conversion from const& to const&&

#define tc_conditional_typed(b, lhstype, lhs, rhstype, rhs) \
	tc_conditional_impl(TC_FWD(b), TC_FWD(tc::common_reference_t<lhstype, rhstype>), TC_FWD(lhs), TC_FWD(rhs))

#define tc_conditional_rvalue_as_ref(b, lhs, rhs) \
	tc_conditional_typed(b, decltype((lhs))&&, TC_FWD(lhs), decltype((rhs))&&, TC_FWD(rhs))

#define tc_conditional_prvalue_as_val(b, lhs, rhs) \
	tc_conditional_typed(b, decltype((lhs)), TC_FWD(lhs), decltype((rhs)), TC_FWD(rhs))
