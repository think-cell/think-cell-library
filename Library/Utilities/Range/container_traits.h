#pragma once

#include "range_defines.h"

#include "Library/Utilities/has_mem_fn.h"

#include <boost/mpl/has_xxx.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {
	// Todo: move those below into namespace
}

TC_HAS_MEM_FN_XXX_TRAIT_DEF( sort )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( reverse )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( splice ) // pretty good indication that datastructure is list-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_TRAIT_DEF( splice_after ) // pretty good indication that datastructure is forward_list-like
TC_HAS_MEM_FN_XXX_TRAIT_DEF( lower_bound ) // pretty good indication that datastructure is tree-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_TRAIT_DEF( assign )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( size )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( empty )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( data )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( reserve )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( emplace_back )
TC_HAS_MEM_FN_XXX_TRAIT_DEF(pop_front)
TC_HAS_MEM_FN_XXX_TRAIT_DEF(pop_back)

BOOST_MPL_HAS_XXX_TRAIT_DEF(efficient_erase)


