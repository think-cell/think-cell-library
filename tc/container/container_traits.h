
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/has_xxx.h"
#include "../range/meta.h"

#include <boost/mpl/has_xxx.hpp>

namespace tc {
	namespace no_adl {
		template <typename Cont, typename... T>
		struct has_emplace_back : std::false_type {};

		template <typename Cont, typename... T> requires requires (Cont& cont, T&&... ts) { cont.emplace_back(tc_move_if_owned(ts)...); }
		struct has_emplace_back<Cont, T...> : std::true_type {};
	}
	using no_adl::has_emplace_back;

	// Todo: move those below into namespace
}

TC_HAS_MEM_FN_XXX_CONCEPT_DEF( sort, &)
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( reverse, &)
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( splice, &, std::declval<tc::iterator_t<T const>>(), std::declval<T>() ) // pretty good indication that datastructure is list-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( splice_after, &, std::declval<tc::iterator_t<T const>>(), std::declval<T>() ) // pretty good indication that datastructure is forward_list-like
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( lower_bound, const&, std::declval<typename T::key_type const&>() ) // pretty good indication that datastructure is tree-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( assign, &, std::declval<tc::range_value_t<T&> const*>(), std::declval<tc::range_value_t<T&> const*>() )
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( size, /*not const&, files might flush*/ )
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( empty, /*not const&, files might flush*/ )
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( data, const&)
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( reserve, &, 1 )
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( clear, & )
TC_HAS_MEM_FN_XXX_CONCEPT_DEF(push_back, &, std::declval<tc::range_value_t<T&>>())
TC_HAS_MEM_FN_XXX_CONCEPT_DEF(pop_front, &)
TC_HAS_MEM_FN_XXX_CONCEPT_DEF(pop_back, &)
TC_HAS_MEM_FN_XXX_CONCEPT_DEF( hash_function, const&) // indicate the datastructure is a hashset/hashtable
TC_HAS_MEM_FN_XXX_CONCEPT_DEF(capacity, const&)

BOOST_MPL_HAS_XXX_TRAIT_DEF(efficient_erase)


