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
#ifdef TC_PRIVATE
#include "Library/HashAppend/hash_append.h"
#endif
#include "assign.h"
#include "type_traits.h"
#include <vector>
#include <memory>
#include <stack>
#include <unordered_map>
#include <unordered_set>

namespace tc {
	template<typename T, typename Alloc=std::allocator<T> >
	using vector=std::vector<T,Alloc>;

	// std::vector<bool>::const_refernce is bool (C++ standard)
	template<>
	struct decay< tc::vector<bool>::reference > {
		using type = bool;
	};

	template<typename T, typename Alloc=std::allocator<T> >
	using simple_stack=std::stack<T, vector<T, Alloc> >;

#ifdef TC_PRIVATE
	template<typename Key, typename Hash=xstd::uhash<>, typename KeyEqual=tc::fn_equal_to, typename Allocator=std::allocator<Key>>
	using unordered_set=std::unordered_set<Key, Hash, KeyEqual, Allocator>;

	template<typename Key, typename T, typename Hash=xstd::uhash<>, typename KeyEqual=tc::fn_equal_to, typename Allocator=std::allocator<std::pair<const Key, T>>>
	using unordered_map=std::unordered_map<Key, T, Hash, KeyEqual, Allocator>;
#else
	using std::unordered_set;
	using std::unordered_map;
#endif
}
