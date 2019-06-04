
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_fwd.h"
#include "type_traits.h"

#ifdef __clang__

// used in an expression, prvalue is forwarded as xvalue
#define CONDITIONAL_RVALUE_AS_REF(b, lhs, rhs) \
	(b) ? static_cast<tc::common_reference_xvalue_as_ref_t<decltype((lhs))&&, decltype((rhs))&&>>(lhs) : static_cast<tc::common_reference_xvalue_as_ref_t<decltype((lhs))&&, decltype((rhs))&&>>(rhs) // static_cast needed for conversion from const& to const&&

#define CONDITIONAL_PRVALUE_AS_VAL(b, lhs, rhs) \
	(b) ? static_cast<tc::common_reference_prvalue_as_val_t<decltype((lhs)), decltype((rhs))>>(lhs) : static_cast<tc::common_reference_prvalue_as_val_t<decltype((lhs)), decltype((rhs))>>(rhs) // static_cast needed for conversion from const& to const&&

#else
// Visual Studio has problem deducing the type of conditional operator
// https://developercommunity.visualstudio.com/content/problem/387444/ternary-operator-with-xvalue-operands.html

// with this lambda workaround in Visual Studio, CONDITIONAL_RVALUE_AS_REF only supports xvalue_as_ref
// prvalues don't have a tc::common_reference_xvalue_as_ref_t, so it won't compile
// prvalue ptr_range might have a tc::common_reference_xvalue_as_ref_t of prvalue ptr_range, not a ptr_range reference
#define CONDITIONAL_RVALUE_AS_REF(b, lhs, rhs) \
	[&]() -> tc::common_reference_xvalue_as_ref_t<decltype((lhs)), decltype((rhs))> { \
		if(b) { \
			return static_cast<tc::common_reference_xvalue_as_ref_t<decltype((lhs)), decltype((rhs))>>(lhs); \
		} else { \
			return static_cast<tc::common_reference_xvalue_as_ref_t<decltype((lhs)), decltype((rhs))>>(rhs); \
		} \
	}()

#define CONDITIONAL_PRVALUE_AS_VAL(b, lhs, rhs) \
	[&]() -> tc::common_reference_prvalue_as_val_t<decltype((lhs)), decltype((rhs))> { \
		if(b) { \
			return static_cast<tc::common_reference_prvalue_as_val_t<decltype((lhs)), decltype((rhs))>>(lhs); \
		} else { \
			return static_cast<tc::common_reference_prvalue_as_val_t<decltype((lhs)), decltype((rhs))>>(rhs); \
		} \
	}()

#endif
