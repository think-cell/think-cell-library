
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_fwd.h"
#include "type_traits.h"

#define CONDITIONAL(b, lhs, rhs) \
	(b) ? static_cast<tc::common_reference_t<decltype((lhs)), decltype((rhs))>>(lhs) : static_cast<tc::common_reference_t<decltype((lhs)), decltype((rhs))>>(rhs) // static_cast needed for conversion from const& to const&&
