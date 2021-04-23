
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "meta.h"
#include "container_traits.h"
#include "explicit_cast.h"
#include "for_each.h"
#include "make_lazy.h"
#include "container.h"

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

	template< typename Cont, typename It, std::enable_if_t<!is_instance<std::multiset,Cont>::value && !is_instance<std::multimap,Cont>::value && !is_instance<boost::intrusive::multiset,Cont>::value>* = nullptr >
	It&& verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It, std::enable_if_t<is_instance<std::multiset,Cont>::value || is_instance<std::multimap,Cont>::value || is_instance<boost::intrusive::multiset,Cont>::value>* = nullptr >
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

	template< typename Cont, typename... Args >
	auto cont_must_emplace_before(Cont& cont, typename boost::range_iterator<Cont const>::type itHint, Args&& ... args) MAYTHROW {
		static_assert(tc::is_explicit_castable<tc::range_value_t<Cont>, Args&& ... >::value);
	#ifdef _CHECKS
		auto const c=cont.size();
	#endif
		auto it = tc::with_lazy_explicit_cast<tc::range_value_t<Cont>>(
			[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace_hint(itHint, std::forward<decltype(args2)>(args2)...)); },
			std::forward<Args>(args)...
		); // MAYTHROW
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERTEQUAL(modified(it, ++_), itHint);
		return it;
	}

	template <typename Cont, typename... T, std::enable_if_t<
		has_mem_fn_emplace_back<Cont>::value && (0==sizeof...(T) || tc::is_safely_constructible<tc::range_value_t<Cont>, T&& ... >::value)
	>* = nullptr>
	constexpr decltype(auto) cont_emplace_back(Cont& cont, T&& ... value) noexcept(noexcept(cont.emplace_back(std::forward<T>(value)...))) {
		if constexpr (std::is_void<decltype(cont.emplace_back(std::forward<T>(value)...))>::value) {
			NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) ); // MAYTHROW
			return tc::back(cont);
		} else {
			return NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) ); // MAYTHROW
		}
	}

	template <typename Cont, typename... T, std::enable_if_t<
		has_mem_fn_emplace_back<Cont>::value && !(0==sizeof...(T) || tc::is_safely_constructible<tc::range_value_t<Cont>, T&& ... >::value) &&
		tc::is_explicit_castable<tc::range_value_t<Cont>, T&&...>::value
	>* = nullptr>
	constexpr auto cont_emplace_back(Cont& cont, T&& ... value) return_decltype_MAYTHROW(
		cont_emplace_back(cont, tc::lazy_explicit_cast<tc::range_value_t<Cont>>(std::forward<T>(value)...))
	)

	template <typename Cont, typename... T, std::enable_if_t<
		has_mem_fn_lower_bound<Cont>::value && (0==sizeof...(T) || tc::is_explicit_castable<tc::range_value_t<Cont>, T&& ... >::value)
	>* = nullptr>
	constexpr auto cont_emplace_back(Cont& cont, T&& ... value) return_decltype_MAYTHROW(
		*tc::cont_must_emplace_before(cont, tc::end(cont), std::forward<T>(value)...) // tc::cont_must_emplace_before is not SFINAE friendly
	)

	template <typename Cont, typename T0, typename T1, typename... Ts, std::enable_if_t<
		!has_mem_fn_emplace_back<Cont>::value && !has_mem_fn_lower_bound<Cont>::value &&
		tc::is_explicit_castable<tc::range_value_t<Cont>, T0&&, T1&&, Ts&&...>::value
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T0&& v0, T1&& v1, Ts&& ... vs) noexcept {
		NOBADALLOC( cont.push_back(tc::explicit_cast<tc::range_value_t<Cont>>(std::forward<T0>(v0), std::forward<T1>(v1), std::forward<Ts>(vs)...)) );
		return tc::back(cont);
	}

	template <typename Cont, typename T0, std::enable_if_t<
		!has_mem_fn_emplace_back<Cont>::value && !has_mem_fn_lower_bound<Cont>::value &&
		tc::is_explicit_castable<tc::range_value_t<Cont>, T0&&>::value
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T0&& v0) noexcept {
		if constexpr (tc::is_safely_constructible<tc::range_value_t<Cont>, T0&& >::value) {
			NOBADALLOC( cont.push_back(std::forward<T0>(v0)) );
		} else {
			NOBADALLOC( cont.push_back(tc::explicit_cast<tc::range_value_t<Cont>>(std::forward<T0>(v0))) );
		}
		return tc::back(cont);
	}

	template <typename Cont, std::enable_if_t<
		!has_mem_fn_emplace_back<Cont>::value && !has_mem_fn_lower_bound<Cont>::value
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont) noexcept {
		NOBADALLOC( cont.push_back(tc::range_value_t<Cont>()) );
		return tc::back(cont);
	}

	template <typename Cont>
	void emplace_back_by_index( Cont& cont, typename boost::range_size<Cont>::type n ) noexcept {
		tc::cont_emplace_back( cont, MAKE_LAZY( tc::at(cont, n) ) );
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
			[&](auto&& _) MAYTHROW { cont.insert(std::forward<decltype(_)>(_)); }
		);
	}

	template< typename Cont, typename... Args >
	auto cont_must_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		return verify_inserted( verify_at_upper_bound(
			cont,
			tc::with_lazy_explicit_cast<tc::range_value_t<Cont>>(
				[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace(std::forward<decltype(args2)>(args2)...)); },
				std::forward<Args>(args)...
			) // MAYTHROW
		));
	}

	template< typename Cont, typename Rng >
	void cont_must_insert_range(Cont& cont, Rng&& rng) MAYTHROW {
		tc::for_each(
			std::forward<Rng>(rng),
			[&](auto&& _) MAYTHROW { tc::cont_must_emplace(cont, std::forward<decltype(_)>(_)); }
		);
	}

	template< typename Cont, typename... Args >
	auto cont_try_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		auto const pairitb = tc::with_lazy_explicit_cast<tc::range_value_t<Cont>>(
			[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace(std::forward<decltype(args2)>(args2)...)); },
			std::forward<Args>(args)...
		); // MAYTHROW
		STATICASSERTSAME(decltype(pairitb.second), bool);
		return pairitb;
	}

	// std::map::try_emplace enforces eager construction of the key_type object
	template<typename Key, typename Val, typename Compare, typename Alloc, typename K, typename ...Args>
	auto map_try_emplace(tc::map<Key, Val, Compare, Alloc>& map, K&& key, Args&& ...args) noexcept -> std::pair<typename tc::map<Key, Val, Compare, Alloc>::iterator, bool> {
		if (auto it = map.lower_bound(key); tc::end(map) == it || map.key_comp()(key, it->first)) {
			return std::make_pair( NOEXCEPT( tc::cont_must_emplace_before(map, tc_move_always(it), std::piecewise_construct, std::forward_as_tuple(std::forward<K>(key)), std::forward_as_tuple(std::forward<Args>(args)...)) ), true );
		} else {
			return std::make_pair(tc_move(it), false);
		}
	}
}
