
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/explicit_cast.h"
#include "../range/meta.h"
#include "../algorithm/element.h"
#include "../algorithm/for_each.h"
#include "container.h"
#include "container_traits.h"

#include <boost/range/iterator.hpp>
#include <boost/intrusive/set.hpp>

namespace tc {
	// std::set/map returns pair with bool=inserted?
	template< typename It >
	It&& verify_inserted(std::pair<It,bool>&& pairitb) noexcept {
		_ASSERT(pairitb.second);
		return tc_move(pairitb.first);
	}

	// std::multiset/multimap always inserts and thus returns only iterator
	template< typename It >
	It&& verify_inserted(It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It>
	It&& verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It> requires is_instance<std::multiset,Cont>::value || is_instance<std::multimap,Cont>::value || is_instance<boost::intrusive::multiset,Cont>::value
	It&& verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
#ifdef _DEBUG
		/* standard says: the inserted element has to be placed at upper bound */
		auto itNext = modified(it, ++_);
		_ASSERTDEBUG(tc::end(cont) == itNext || cont.value_comp()(*it, *itNext));
#endif
		return std::forward<It>(it);
	}

	template< typename Cont, typename TValue > // use extra template parameter instead of Cont::value_type to have both move and copy semantics
	auto intrusive_cont_must_insert(Cont& cont, TValue& val) MAYTHROW {
		return verify_inserted( verify_at_upper_bound( cont, NOBADALLOC(cont.insert(val)) ) );
	}

	template< typename Cont, typename... Args> requires tc::is_explicit_castable<tc::range_value_t<Cont&>, Args&& ... >::value
	auto cont_must_emplace_before(Cont& cont, tc::iterator_t<Cont const> itHint, Args&& ... args) MAYTHROW {
	#ifdef _CHECKS
		auto const c=cont.size();
	#endif
		auto it = tc::with_lazy_explicit_cast<tc::range_value_t<Cont&>>(
			[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace_hint(itHint, tc_move_if_owned(args2)...)); },
			std::forward<Args>(args)...
		); // MAYTHROW
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERTEQUAL(modified(it, ++_), itHint);
		return it;
	}

	namespace cont_emplace_back_detail {
		template<typename RangeReturn, typename Cont, typename... T>
		constexpr auto cont_emplace_back_impl(Cont& cont, T&& ... value) noexcept {
			if constexpr (has_mem_fn_lower_bound<Cont>::value) {
				return [&]() return_decltype_MAYTHROW(
					RangeReturn::pack_element(tc::cont_must_emplace_before(cont, tc::end(cont), std::forward<T>(value)...), cont)
				);
			} else if constexpr (0 == sizeof...(T) || tc::is_safely_constructible<tc::range_value_t<Cont&>, T&& ... >::value) {
				return [&]() noexcept(std::is_nothrow_constructible<tc::range_value_t<Cont&>, T...>::value) -> decltype(auto) {
					if constexpr (has_emplace_back<Cont, T...>::value) {
						NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) );
					} else {
						if constexpr (1 == sizeof...(T)) {
							NOBADALLOC( cont.push_back(std::forward<T>(value)...) );
						} else {
							NOBADALLOC( cont.push_back(tc::range_value_t<Cont&>(std::forward<T>(value)...)) );
						}
					}
					return tc::back<RangeReturn>(cont);
				};
			} else if constexpr (tc::is_explicit_castable<tc::range_value_t<Cont&>, T&&...>::value) {
				return [&]() return_decltype_MAYTHROW(
					cont_emplace_back_impl<RangeReturn>(cont, tc::lazy_explicit_cast<tc::range_value_t<Cont&>>(std::forward<T>(value)...))()
				);
			}
		}
	}

	template<typename RangeReturn, typename Cont, typename... T>
	constexpr auto cont_emplace_back(Cont& cont, T&& ... value) return_decltype_MAYTHROW(
		cont_emplace_back_detail::cont_emplace_back_impl<RangeReturn>(cont, std::forward<T>(value)...)()
	)

	template<int = 0, typename Cont, typename... T> // Make first template argument non-type to avoid instantiation error
	constexpr auto cont_emplace_back(Cont& cont, T&& ... value) return_decltype_MAYTHROW(
		*cont_emplace_back_detail::cont_emplace_back_impl<tc::return_element>(cont, std::forward<T>(value)...)()
	)

	template <typename Cont>
	void emplace_back_by_index( Cont& cont, typename boost::range_size<Cont>::type n ) noexcept {
		tc::cont_emplace_back( cont, MAKE_LAZY( tc::at(cont, n) ) );
	}

	template< typename Cont, typename... Args >
	auto cont_must_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		return verify_inserted( verify_at_upper_bound(
			cont,
			tc::with_lazy_explicit_cast<tc::range_value_t<Cont&>>(
				[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace(tc_move_if_owned(args2)...)); },
				std::forward<Args>(args)...
			) // MAYTHROW
		));
	}

	template< typename Cont, typename Rng >
	void cont_must_insert_range(Cont& cont, Rng&& rng) MAYTHROW {
		tc::for_each(
			std::forward<Rng>(rng),
			[&](auto&& _) MAYTHROW { tc::cont_must_emplace(cont, tc_move_if_owned(_)); }
		);
	}

	template< typename Cont, typename... Args >
	auto cont_try_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		auto const pairitb = tc::with_lazy_explicit_cast<tc::range_value_t<Cont&>>(
			[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace(tc_move_if_owned(args2)...)); },
			std::forward<Args>(args)...
		); // MAYTHROW
		STATICASSERTSAME(decltype(pairitb.second), bool);
		return pairitb;
	}

	template< typename Cont, typename Rng >
	void cont_try_insert_range(Cont& cont, Rng&& rng) MAYTHROW {
		/*
			It might be more efficient for a container to insert a range at once
			with insert(begin(rng),end(rng)), but on the other hand, it is more
			efficient to access a range as generator range: If filters and costly
			transforms are involved, a generator range needs to dereference only once.
		*/
		tc::for_each(
			std::forward<Rng>(rng),
			[&](auto&& _) MAYTHROW { tc::cont_try_emplace(cont, tc_move_if_owned(_)); }
		);
	}

	// std::map::try_emplace enforces eager construction of the key_type object
	template<typename Key, typename Val, typename Compare, typename Alloc, typename K, typename... Args>
	auto map_try_emplace(tc::map<Key, Val, Compare, Alloc>& map, K&& key, Args&& ...args) MAYTHROW -> std::pair<typename tc::map<Key, Val, Compare, Alloc>::iterator, bool> {
		if (auto it = map.lower_bound(key); tc::end(map) == it || map.key_comp()(key, it->first)) {
			// MSVC 19.29 does not correctly parse the following statement, if tc_move is used instead of tc_move_always.
			return std::make_pair( tc::cont_must_emplace_before(map, tc_move_always(it), std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<Args>(args)...)), true );
		} else {
			return std::make_pair(tc_move(it), false);
		}
	}

	template<typename Key, typename Val, typename Compare, typename Alloc, typename K, typename ...Args>
	void map_emplace_or_assign(tc::map<Key, Val, Compare, Alloc>& map, K&& key, Args&& ...args) MAYTHROW {
		if (auto it = map.lower_bound(key); tc::end(map) == it || map.key_comp()(key, it->first)) {
			// MSVC 19.29 does not correctly parse the following statement, if tc_move is used instead of tc_move_always.
			tc::cont_must_emplace_before(map, tc_move_always(it), std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<Args>(args)...));
		} else {
			tc::renew( it->second, std::forward<Args>(args)... );
		}
	}

	template<typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename ...Args, typename K>
	void map_emplace_or_assign(tc::unordered_map<Key, T, Hash, KeyEqual, Allocator>& map, K&& key, Args&& ...args) MAYTHROW {
		if (auto const pairitb = map.try_emplace(tc::reluctant_explicit_cast<std::remove_reference_t<decltype(map)>::key_type>(tc_move_if_owned(key)), tc_move_if_owned(args)...); !pairitb.second ) {
			tc::renew( pairitb.first->second, std::forward<Args>(args)... );
		}
	}
}
