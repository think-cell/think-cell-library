
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "container_traits.h"
#include "explicit_cast.h"
#include "for_each.h"
#include "make_lazy.h"

#include <boost/range/iterator.hpp>
#include <boost/next_prior.hpp>
#include <boost/intrusive/set.hpp>

#include <set>
#include <map>

namespace tc {
	// std::set/map returns pair with bool=inserted?
	template< typename It >
	It && verify_inserted(std::pair<It,bool>&& pairitb) noexcept {
		_ASSERT(pairitb.second);
		return tc_move(pairitb.first);
	}

	// std::multiset/multimap always inserts and thus returns only iterator
	template< typename It >
	It && verify_inserted(It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It, std::enable_if_t<!is_instance<std::multiset,Cont>::value && !is_instance<std::multimap,Cont>::value && !is_instance<boost::intrusive::multiset,Cont>::value>* = nullptr >
	It && verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
		return std::forward<It>(it);
	}

	template< typename Cont, typename It, std::enable_if_t<is_instance<std::multiset,Cont>::value || is_instance<std::multimap,Cont>::value || is_instance<boost::intrusive::multiset,Cont>::value>* = nullptr >
	It && verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
#ifdef _DEBUG
		/* standard says: the inserted element has to be placed at upper bound */
		auto itNext = boost::next(it);
		_ASSERTDEBUG(tc::end(cont) == itNext || cont.value_comp()(*it, *itNext));
#endif
		return std::forward<It>(it);
	}

	template< typename Cont, typename TValue > // use extra template parameter instead of Cont::value_type to have both move and copy semantics
	auto cont_must_insert(Cont& cont, TValue&& val) MAYTHROW {
		return verify_inserted( verify_at_upper_bound( cont, NOBADALLOC(cont.insert(std::forward<TValue>(val))) ) );
	}

	DEFINE_FN(insert);
	DEFINE_FN(cont_must_insert);

	template< typename Cont, typename... Args >
	auto cont_must_emplace_before(Cont& cont, typename boost::range_iterator<Cont const>::type itHint, Args&& ... args) MAYTHROW {
		static_assert(tc::is_safely_constructible<tc::range_value_t<Cont>, Args&& ... >::value);
	#ifdef _CHECKS
		auto const c=cont.size();
	#endif
		auto it = NOBADALLOC(cont.emplace_hint(itHint, std::forward<Args>(args)...)); // MAYTHROW
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERTEQUAL(boost::next(it), itHint);
		return it;
	}

	template <typename Cont, typename... T, std::enable_if_t<
		has_mem_fn_emplace_back<Cont>::value && (0==sizeof...(T) || tc::is_safely_constructible<tc::range_value_t<Cont>, T&& ... >::value)
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T&& ... value) MAYTHROW {
		if constexpr (std::is_void<decltype(cont.emplace_back(std::forward<T>(value)...))>::value) {
			NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) ); // MAYTHROW
			return tc_back(cont);
		} else {
			return NOBADALLOC( cont.emplace_back(std::forward<T>(value)...) ); // MAYTHROW
		}
	}

	template <typename Cont, typename... T, std::enable_if_t<
		has_mem_fn_emplace_back<Cont>::value && !(0==sizeof...(T) || tc::is_safely_constructible<tc::range_value_t<Cont>, T&& ... >::value) &&
		tc::is_explicit_castable<tc::range_value_t<Cont>, T&&...>::value
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T&& ... value) MAYTHROW {
		return cont_emplace_back(cont, tc::explicit_cast<tc::range_value_t<Cont>>(std::forward<T>(value)...));
	}

	template <typename Cont, typename... T, std::enable_if_t<
		has_mem_fn_lower_bound<Cont>::value && (0==sizeof...(T) || tc::is_safely_constructible<tc::range_value_t<Cont>, T&& ... >::value)
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T&& ... value) MAYTHROW {
		return *tc::cont_must_emplace_before(cont, tc::end(cont), std::forward<T>(value)...); // tc::cont_must_emplace_before is not SFINAE friendly
	}

	template <typename Cont, typename T0, typename T1, typename... Ts, std::enable_if_t<
		!has_mem_fn_emplace_back<Cont>::value && !has_mem_fn_lower_bound<Cont>::value &&
		tc::is_explicit_castable<tc::range_value_t<Cont>, T0&&, T1&&, Ts&&...>::value
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont, T0&& v0, T1&& v1, Ts&& ... vs) noexcept {
		NOBADALLOC( cont.push_back(tc::explicit_cast<tc::range_value_t<Cont>>(std::forward<T0>(v0), std::forward<T1>(v1), std::forward<Ts>(vs)...)) );
		return tc_back(cont);
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
		return tc_back(cont);
	}

	template <typename Cont, std::enable_if_t<
		!has_mem_fn_emplace_back<Cont>::value && !has_mem_fn_lower_bound<Cont>::value
	>* = nullptr>
	decltype(auto) cont_emplace_back(Cont& cont) noexcept {
		NOBADALLOC( cont.push_back(tc::range_value_t<Cont>()) );
		return tc_back(cont);
	}

	template <typename Cont>
	void emplace_back_by_index( Cont& cont, typename boost::range_size<Cont>::type n ) noexcept {
		tc::cont_emplace_back( cont, MAKE_LAZY( tc_at(cont, n) ) );
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

	template< typename Cont, typename Rng >
	void cont_must_insert_range(Cont& cont, Rng&& rng) MAYTHROW {
		tc::for_each(
			std::forward<Rng>(rng),
			[&](auto&& _) MAYTHROW { tc::cont_must_insert(cont, std::forward<decltype(_)>(_)); }
		);
	}

	template< typename Cont, typename... Args >
	auto cont_must_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		return verify_inserted( verify_at_upper_bound(
			cont,
			NOBADALLOC(cont.emplace(std::forward<Args>(args)...)) // MAYTHROW
		));
	}

	template< typename Cont, typename... Args >
	auto cont_try_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		return NOBADALLOC(cont.emplace(std::forward<Args>(args)...)); // MAYTHROW
	}

	template<typename... MapArgs, typename K, typename V, typename Better>
	void map_try_emplace_better(std::map<MapArgs...>& map, K&& key, V&& val, Better&& better) noexcept {
		auto it = map.lower_bound(key);
		if (tc::end(map) == it || map.key_comp()(key, it->first)) {
			NOEXCEPT( tc::cont_must_emplace_before(map, tc_move(it), std::forward<K>(key), std::forward<V>(val)) );
		} else {
			tc::assign_better(it->second, std::forward<V>(val), std::forward<Better>(better));
		}
	}
}
