//-----------------------------------------------------------------------------------------------------------------------------
// Include all header for tc ranges. See the individual headers under Range/ for details.
//-----------------------------------------------------------------------------------------------------------------------------
// 
// This also forms the basis for our iso c++ standard proposal. (current proposal N3752)
//
// Design choices:
// - To avoid fat iterators when stacking range adaptors, in particular range filters, all per-range and not per-iterator data is stored in the range adaptor object.
// - Thus, range adaptor iterator lifetime is limited to the lifetime of the range. All range adaptor iterators are slim, consisting of a pointer to their range and the "index",
//   which is usually the (slim) iterator of the very-base container.
// - All usual iterator operations are defined in terms of the range and the "index" (similar to cursor from http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2005/n1873.html ).
// - Below the range category providing input iterators, there is an even simpler range category which can only plug their elements into a functor passed to operator(). They have no iterators.
// - If the functor returns break_or_continue, the simpler range category is expected to a) heed this and b) return this break_or_continue from its operator() when the enumeration is finished.
// - If the functor does not return break_or_continue, the simpler range category is expected to run over all elements and not to return break_. It may return continue_ or another type or void.
// - Range content constness = base range constness || range adaptor object constness, so an function with argument template< typename Range > Range const& cannot modify the range's elements.
// - Adaptors created around a range rvalue aggregate their base range, e.g., contain it by value.

#pragma once

#include "Range/range_defines.h"

#include "Range/meta.h"
#include "Range/break_or_continue.h"
#include "Range/for_each.h"

#include "Range/index_range.h"
#include "Range/range_adaptor.h"
#include "Range/sub_range.h"
#include "Range/filter_range.h"
#include "Range/transform_range.h"
#include "Range/algorithm.h"
#include "Range/equal.h"

#include "casts.h"
#include "reference_or_value.h"

//-----------------------------------------------------------------------------------------------------------------------------
