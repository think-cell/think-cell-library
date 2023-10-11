
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "../base/assert_defs.h"
#include "../base/type_traits.h"
#include "../base/assign.h"
#include "../algorithm/compare.h"
#include <vector>
#include <memory>
#include <stack>
#include <set>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <list>

#if defined(TC_PRIVATE) && !defined(__cpp_lib_generic_unordered_lookup)
	#include <boost/multi_index_container.hpp>
	#include <boost/multi_index/identity.hpp>
	#include <boost/multi_index/hashed_index.hpp>
#endif

namespace tc {
	template<typename T, typename Alloc=std::allocator<T> >
	using vector=std::vector<T,Alloc>;

	template<typename T, typename Alloc=std::allocator<T> >
	using simple_stack=std::stack<T, vector<T, Alloc> >;

	template<typename Rng, typename Compare=decltype(tc::lessfrom3way(tc::fn_lexicographical_compare_3way())), typename Alloc=std::allocator<Rng>>
	using set_range=std::set<Rng, Compare, Alloc>;

#ifdef TC_PRIVATE
	template<typename Result, typename T>
	struct fn_hash_range;

#ifdef __cpp_lib_generic_unordered_lookup
	template<
		typename Rng,
		typename Hash=tc::fn_hash_range<std::size_t, tc::range_value_t<Rng&>>,
		typename KeyEqual=tc::fn_equal,
		typename Alloc=std::allocator<Rng>
	>
	using unordered_set_range=std::unordered_set<Rng, Hash, KeyEqual, Alloc>;
#else // missing on apple clang
	template<
		typename Rng,
		typename Hash=tc::fn_hash_range<std::size_t, tc::range_value_t<Rng&>>,
		typename KeyEqual=tc::fn_equal,
		typename Alloc=std::allocator<Rng>
	>
	using unordered_set_range=boost::multi_index_container<
		Rng,
		boost::multi_index::indexed_by<boost::multi_index::hashed_unique<boost::multi_index::identity<Rng>, Hash, KeyEqual>>,
		Alloc
	>;
#endif
	template<typename Rng, typename T, typename Compare=decltype(tc::lessfrom3way(tc::fn_lexicographical_compare_3way())), typename Alloc=std::allocator<std::pair<Rng const, T>>>
	using map_range=std::map<Rng, T, Compare, Alloc>;

	// no equivalent interface with multi_index_container
	template<
		typename Rng,
		typename T,
		typename Hash=tc::fn_hash_range<std::size_t, tc::range_value_t<Rng&>>,
		typename KeyEqual=tc::fn_equal,
		typename Alloc=std::allocator<std::pair<Rng const, T>>
	>
	using unordered_map_range=std::unordered_map<Rng, T, Hash, KeyEqual, Alloc>;

	template<typename Result, typename T>
	struct fn_hash;

	template<typename Key, typename Hash=tc::fn_hash<std::size_t, Key>, typename KeyEqual=tc::fn_equal_to, typename Alloc=std::allocator<Key>>
	using unordered_set=std::unordered_set<Key, Hash, KeyEqual, Alloc>;

	template<typename Key, typename T, typename Hash=tc::fn_hash<std::size_t, Key>, typename KeyEqual=tc::fn_equal_to, typename Alloc=std::allocator<std::pair<Key const, T>>>
	using unordered_map=std::unordered_map<Key, T, Hash, KeyEqual, Alloc>;
#else
	template<
		typename Rng,
		typename Hash=std::hash<Rng>,
		typename KeyEqual=tc::fn_equal,
		typename Alloc=std::allocator<Rng>
	>
	using unordered_set_range = std::unordered_set<Rng, Hash, KeyEqual, Alloc>;

	template<
		typename Rng,
		typename T,
		typename Hash=std::hash<Rng>,
		typename KeyEqual=tc::fn_equal,
		typename Alloc=std::allocator<std::pair<Rng const, T>>
	>
	using unordered_map_range = std::unordered_map<Rng, T, Hash, KeyEqual, Alloc>;

	using std::unordered_set;
	using std::unordered_map;
#endif

	namespace less_key_adl {
		DEFINE_ADL_TAG_TYPE(less_key_tag)
	}

	namespace no_adl {
		template<typename Lhs, typename Rhs, typename Enable=void>
		struct has_adl_less_key_helper final: tc::constant<false> {};

		template<typename Lhs, typename Rhs>
		struct has_adl_less_key_helper<Lhs, Rhs, decltype(less_key_helper(std::declval<Lhs const&>(), std::declval<Rhs const&>()), void())> final: tc::constant<true>{};

		template<typename Lhs, typename Rhs, typename Enable=void>
		struct has_adl_tag_less_key_helper final: tc::constant<false> {};

		template<typename Lhs, typename Rhs>
		struct has_adl_tag_less_key_helper<Lhs, Rhs, decltype(less_key_helper(tc::less_key_adl::less_key_tag, std::declval<Lhs const&>(), std::declval<Rhs const&>()), void())> final: tc::constant<true>{};

		struct less_key final {
			template<typename Lhs, typename Rhs> requires (!has_adl_less_key_helper<Lhs, Rhs>::value) && (!has_adl_tag_less_key_helper<Lhs, Rhs>::value)
			bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
				return tc::less(lhs, rhs);
			}

			template<typename Lhs, typename Rhs> requires has_adl_less_key_helper<Lhs, Rhs>::value
			bool operator()(Lhs const& lhs, Rhs const& rhs) const& noexcept {
				return less_key_helper(lhs, rhs);
			}

			template<typename Lhs, typename Rhs> requires has_adl_tag_less_key_helper<Lhs, Rhs>::value
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
