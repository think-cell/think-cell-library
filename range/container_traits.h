//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"

#include "has_mem_fn.h"

#include <boost/mpl/has_xxx.hpp>

namespace tc {
	// Todo: move those below into namespace
}

TC_HAS_MEM_FN_XXX_TRAIT_DEF( sort, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( reverse, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( splice, &, std::declval<typename T::const_iterator>(), std::declval<T>() ) // pretty good indication that datastructure is list-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_TRAIT_DEF( splice_after, &, std::declval<typename T::const_iterator>(), std::declval<T>() ) // pretty good indication that datastructure is forward_list-like
TC_HAS_MEM_FN_XXX_TRAIT_DEF( lower_bound, const&, std::declval<typename T::value_type const&>() ) // pretty good indication that datastructure is tree-like: assume erase(itBegin,itEnd) is cheap and preserves iterators
TC_HAS_MEM_FN_XXX_TRAIT_DEF( assign, &, std::declval<typename T::value_type const*>(), std::declval<typename T::value_type const*>() )
TC_HAS_MEM_FN_XXX_TRAIT_DEF( size, const&)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( empty, const&)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( data, const&)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( reserve, &, 1 )

template<typename T, typename... Args>
struct has_mem_fn_emplace_back_with_args {
private:
	template<typename U> static auto test(int) -> decltype(std::declval<U&>().emplace_back( std::declval<Args>()... ), std::true_type());
	template<typename> static std::false_type test(...);
public:
	static constexpr bool value = std::is_same<decltype(test<T>(0)), std::true_type>::value;
};

TC_HAS_MEM_FN_XXX_TRAIT_DEF(push_back, &, std::declval<typename T::value_type>())
TC_HAS_MEM_FN_XXX_TRAIT_DEF(emplace_back, &, std::declval<typename T::value_type>())
TC_HAS_MEM_FN_XXX_TRAIT_DEF(pop_front, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF(pop_back, &)
TC_HAS_MEM_FN_XXX_TRAIT_DEF( hash_function, const&) // indicate the datastructure is a hashset/hashtable
TC_HAS_MEM_FN_XXX_TRAIT_DEF(capacity, const&)

BOOST_MPL_HAS_XXX_TRAIT_DEF(efficient_erase)


