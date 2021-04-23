
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "meta.h"
#include "has_mem_fn.h"

#include <boost/mpl/has_xxx.hpp>

namespace tc {
	// Todo: move those below into namespace
}

TC_HAS_MEM_FN_XXX_TRAIT_DEF( sort, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( reverse, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( splice, &, std::declval<typename T::const_iterator>(), std::declval<T>() ) // pretty good indication that datastructure is list-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_TRAIT_DEF( splice_after, &, std::declval<typename T::const_iterator>(), std::declval<T>() ) // pretty good indication that datastructure is forward_list-like
TC_HAS_MEM_FN_XXX_TRAIT_DEF( lower_bound, const&, std::declval<tc::range_value_t<T> const&>() ) // pretty good indication that datastructure is tree-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_TRAIT_DEF( assign, &, std::declval<tc::range_value_t<T> const*>(), std::declval<tc::range_value_t<T> const*>() )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( size, const&)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( empty, const&)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( data, const&)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( reserve, &, 1 )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( clear, & )
TC_HAS_MEM_FN_XXX_TRAIT_DEF(push_back, &, std::declval<tc::range_value_t<T>>())
TC_HAS_MEM_FN_XXX_TRAIT_DEF(emplace_back, &, std::declval<tc::range_value_t<T>>())
TC_HAS_MEM_FN_XXX_TRAIT_DEF(pop_front, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF(pop_back, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( hash_function, const&) // indicate the datastructure is a hashset/hashtable
TC_HAS_MEM_FN_XXX_TRAIT_DEF(capacity, const&)

BOOST_MPL_HAS_XXX_TRAIT_DEF(efficient_erase)


