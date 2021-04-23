
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "assert_defs.h"
#include "assign.h"
#include "type_traits.h"
#include "compare.h"
#include <vector>
#include <memory>
#include <stack>
#include <set>
#include <map>
#include <unordered_map>
#include <unordered_set>

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/identity.hpp>
#include <boost/multi_index/hashed_index.hpp>

namespace tc {
	template<typename T, typename Alloc=std::allocator<T> >
	using vector=std::vector<T,Alloc>;

	namespace no_adl {
		// std::vector<bool>::const_refernce is bool (C++ standard)
		template<bool bPreventSlicing>
		struct decay< tc::vector<bool>::reference, bPreventSlicing > {
			using type = bool;
		};
	}

	template<typename T, typename Alloc=std::allocator<T> >
	using simple_stack=std::stack<T, vector<T, Alloc> >;

	template<typename Char, typename Compare=decltype(tc::lessfrom3way(tc::fn_lexicographical_compare_3way())), typename Alloc=std::allocator<std::basic_string<Char>>>
	using set_string=std::set<std::basic_string<Char>, Compare, Alloc>;

#ifdef TC_PRIVATE
	template<typename Result, typename T>
	struct fn_hash_range;

	template<typename Char, typename Hash=tc::fn_hash_range<std::size_t, Char>, typename KeyEqual=decltype(tc::equalfrom3way(tc::fn_lexicographical_compare_3way())), typename Alloc=std::allocator<std::basic_string<Char>>>
	using unordered_set_string=boost::multi_index_container<
		std::basic_string<Char>,
		boost::multi_index::indexed_by<boost::multi_index::hashed_unique<boost::multi_index::identity<std::basic_string<Char>>, Hash, KeyEqual>>,
		Alloc
	>;

	template<typename Result, typename T>
	struct fn_hash;

	template<typename Key, typename Hash=tc::fn_hash<std::size_t, Key>, typename KeyEqual=tc::fn_equal_to, typename Alloc=std::allocator<Key>>
	using unordered_set=std::unordered_set<Key, Hash, KeyEqual, Alloc>;

	template<typename Key, typename T, typename Hash=tc::fn_hash<std::size_t, Key>, typename KeyEqual=tc::fn_equal_to, typename Alloc=std::allocator<std::pair<const Key, T>>>
	using unordered_map=std::unordered_map<Key, T, Hash, KeyEqual, Alloc>;
#else
	template<typename Char, typename Hash=std::hash<std::basic_string<Char>>, typename KeyEqual=tc::fn_equal_to, typename Alloc=std::allocator<std::basic_string<Char>>>
	using unordered_set_string=std::unordered_set<std::basic_string<Char>, Hash, KeyEqual, Alloc>;

	using std::unordered_set;
	using std::unordered_map;
#endif

	namespace less_key_adl {
		DEFINE_ADL_TAG_TYPE(less_key_tag)
	}

	namespace no_adl {
		template<typename Lhs, typename Rhs, typename Enable=void>
		struct has_adl_less_key_helper final: std::false_type {};

		template<typename Lhs, typename Rhs>
		struct has_adl_less_key_helper<Lhs, Rhs, decltype(less_key_helper(std::declval<Lhs const&>(), std::declval<Rhs const&>()), void())> final: std::true_type{};

		template<typename Lhs, typename Rhs, typename Enable=void>
		struct has_adl_tag_less_key_helper final: std::false_type {};

		template<typename Lhs, typename Rhs>
		struct has_adl_tag_less_key_helper<Lhs, Rhs, decltype(less_key_helper(tc::less_key_adl::less_key_tag, std::declval<Lhs const&>(), std::declval<Rhs const&>()), void())> final: std::true_type{};

		struct less_key final {
			template<typename Lhs, typename Rhs, std::enable_if_t<!has_adl_less_key_helper<Lhs, Rhs>::value && !has_adl_tag_less_key_helper<Lhs, Rhs>::value>* = nullptr>
			bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
				return std::less<>()(lhs, rhs);
			}

			template<typename Lhs, typename Rhs, std::enable_if_t<has_adl_less_key_helper<Lhs, Rhs>::value>* = nullptr>
			bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
				return less_key_helper(lhs, rhs);
			}

			template<typename Lhs, typename Rhs, std::enable_if_t<has_adl_tag_less_key_helper<Lhs, Rhs>::value>* = nullptr>
			bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
				return less_key_helper(tc::less_key_adl::less_key_tag, lhs, rhs);
			}

			using is_transparent = void;
		};
	}
	using no_adl::less_key;

	template<typename Key, typename Compare=tc::less_key, typename Alloc=std::allocator<Key>>
	using set=std::set<Key, Compare, Alloc>;

	template<typename Key, typename T, typename Compare=tc::less_key, typename Alloc=std::allocator<std::pair<Key const, T>>>
	using map=std::map<Key, T, Compare, Alloc>;

	namespace no_adl {
		BOOST_MPL_HAS_XXX_TRAIT_DEF(mapped_type)
	}
	using no_adl::has_mapped_type;
}
