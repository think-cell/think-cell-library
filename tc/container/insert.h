
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
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
		return tc_move(pairitb).first;
	}

	// std::multiset/multimap always inserts and thus returns only iterator
	template< typename It >
	It&& verify_inserted(It&& it) noexcept {
		return tc_move_if_owned(it);
	}

	template< typename Cont, typename It>
	It&& verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
		return tc_move_if_owned(it);
	}

	template< typename Cont, typename It> requires tc::instance<Cont, std::multiset> || tc::instance<Cont, std::multimap> || tc::instance<Cont, boost::intrusive::multiset>
	It&& verify_at_upper_bound(Cont const& cont, It&& it) noexcept {
#ifdef _DEBUG
		/* standard says: the inserted element has to be placed at upper bound */
		auto itNext = tc_modified(it, ++_);
		_ASSERTDEBUG(tc::end(cont) == itNext || cont.value_comp()(*it, *itNext));
#endif
		return tc_move_if_owned(it);
	}

	template< typename Cont, typename TValue > // use extra template parameter instead of Cont::value_type to have both move and copy semantics
	auto intrusive_cont_must_insert(Cont& cont, TValue& val) MAYTHROW {
		return verify_inserted( verify_at_upper_bound( cont, NOBADALLOC(cont.insert(val)) ) );
	}

	template< typename Cont, typename... Args> requires tc::explicit_castable_from<tc::range_value_t<Cont&>, Args&& ... >
	auto cont_must_emplace_before(Cont& cont, tc::iterator_t<Cont const> itHint, Args&& ... args) MAYTHROW {
	#ifdef _CHECKS
		auto const c=cont.size();
	#endif
		auto it = tc::with_lazy_explicit_cast<tc::range_value_t<Cont&>>(
			[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace_hint(itHint, tc_move_if_owned(args2)...)); },
			tc_move_if_owned(args)...
		); // MAYTHROW
		_ASSERTEQUAL( cont.size(), c+1 );
		_ASSERTEQUAL(tc_modified(it, ++_), itHint);
		return it;
	}

	namespace cont_emplace_back_detail {
		template<typename RangeReturn, typename Cont, typename... T>
		constexpr auto cont_emplace_back_impl(Cont& cont, T&& ... value) noexcept {
			if constexpr (has_mem_fn_lower_bound<Cont>) {
				return [&]() return_decltype_MAYTHROW(
					RangeReturn::pack_element(tc::cont_must_emplace_before(cont, tc::end(cont), tc_move_if_owned(value)...), cont)
				);
			} else if constexpr (0 == sizeof...(T) || tc::safely_constructible_from<tc::range_value_t<Cont&>, T&& ... >) {
				return [&]() noexcept(std::is_nothrow_constructible<tc::range_value_t<Cont&>, T...>::value) -> decltype(auto) {
					if constexpr (has_emplace_back<Cont, T...>::value) {
						NOBADALLOC( cont.emplace_back(tc_move_if_owned(value)...) );
					} else {
						if constexpr (1 == sizeof...(T)) {
							NOBADALLOC( cont.push_back(tc_move_if_owned(value)...) );
						} else {
							NOBADALLOC( cont.push_back(tc::range_value_t<Cont&>(tc_move_if_owned(value)...)) );
						}
					}
					return tc::back<RangeReturn>(cont);
				};
			} else if constexpr (tc::explicit_castable_from<tc::range_value_t<Cont&>, T&&...>) {
				return [&]() return_decltype_MAYTHROW(
					cont_emplace_back_impl<RangeReturn>(cont, tc::lazy_explicit_cast<tc::range_value_t<Cont&>>(tc_move_if_owned(value)...))()
				);
			}
		}
	}

	template<typename RangeReturn, typename Cont, typename... T>
	constexpr auto cont_emplace_back(Cont& cont, T&& ... value) return_decltype_MAYTHROW(
		cont_emplace_back_detail::cont_emplace_back_impl<RangeReturn>(cont, tc_move_if_owned(value)...)()
	)

	template<int = 0, typename Cont, typename... T> // Make first template argument non-type to avoid instantiation error
	constexpr auto cont_emplace_back(Cont& cont, T&& ... value) return_decltype_MAYTHROW(
		*cont_emplace_back_detail::cont_emplace_back_impl<tc::return_element>(cont, tc_move_if_owned(value)...)()
	)

	template <typename Cont>
	void emplace_back_by_index( Cont& cont, typename boost::range_size<Cont>::type n ) noexcept {
		tc::cont_emplace_back( cont, tc_lazy( tc::at(cont, n) ) );
	}

	template< typename Cont, typename... Args >
	auto cont_must_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		return verify_inserted( verify_at_upper_bound(
			cont,
			tc::with_lazy_explicit_cast<tc::range_value_t<Cont&>>(
				[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace(tc_move_if_owned(args2)...)); },
				tc_move_if_owned(args)...
			) // MAYTHROW
		));
	}

	namespace no_adl {
		template<typename Cont>
		struct fn_cont_must_emplace { // MSVC workaround: not a lambda for shorter symbol names
			Cont& m_cont;
			void operator()(auto&& u) const& MAYTHROW {
				tc::cont_must_emplace(m_cont, tc_move_if_owned(u));
			}
		};
	}


	template< typename Cont, typename Rng >
	void cont_must_insert_range(Cont& cont, Rng&& rng) MAYTHROW {
		tc::for_each(tc_move_if_owned(rng), no_adl::fn_cont_must_emplace<Cont>{cont});
	}

	template< typename Cont, typename... Args >
	auto cont_try_emplace(Cont& cont, Args&& ... args) MAYTHROW {
		auto const pairitb = tc::with_lazy_explicit_cast<tc::range_value_t<Cont&>>(
			[&](auto&&... args2) MAYTHROW -> decltype(auto) { return NOBADALLOC(cont.emplace(tc_move_if_owned(args2)...)); },
			tc_move_if_owned(args)...
		); // MAYTHROW
		STATICASSERTSAME(decltype(pairitb.second), bool);
		return pairitb;
	}

	namespace no_adl {
		template<typename Cont>
		struct fn_cont_try_emplace { // MSVC workaround: not a lambda for shorter symbol names
			Cont& m_cont;
			void operator()(auto&& u) const& MAYTHROW {
				tc::cont_try_emplace(m_cont, tc_move_if_owned(u));
			}
		};
	}

	template< typename Cont, typename Rng >
	void cont_try_insert_range(Cont& cont, Rng&& rng) MAYTHROW {
		/*
			It might be more efficient for a container to insert a range at once
			with insert(begin(rng),end(rng)), but on the other hand, it is more
			efficient to access a range as generator range: If filters and costly
			transforms are involved, a generator range needs to dereference only once.
		*/
		tc::for_each(tc_move_if_owned(rng), no_adl::fn_cont_try_emplace<Cont>{cont});
	}

	// std::map::try_emplace enforces eager construction of the key_type object
	template<typename Key, typename Val, typename Compare, typename Alloc>
	auto map_try_emplace(tc::map<Key, Val, Compare, Alloc>& map, auto&& key, auto&& val) MAYTHROW
		-> std::pair<tc::iterator_t<tc::map<Key, Val, Compare, Alloc>>, bool>
	{
		if (auto it = map.lower_bound(key); tc::end(map) == it || map.key_comp()(key, it->first)) {
			return {
				tc::cont_must_emplace_before(map, tc_move(it), tc_move_if_owned(key), tc_move_if_owned(val)),
				true
			};
		} else {
			return {tc_move(it), false};
		}
	}

	template<typename Key, typename Val, typename Compare, typename Alloc>
	void map_emplace_or_assign(tc::map<Key, Val, Compare, Alloc>& map, auto&& key, auto&& val) MAYTHROW {
		if (auto it = map.lower_bound(key); tc::end(map) == it || map.key_comp()(key, it->first)) {
			tc::cont_must_emplace_before(map, tc_move(it), tc_move_if_owned(key), tc_move_if_owned(val)); // MAYTHROW
		} else {
			tc::assign_explicit_cast(it->second, tc_move_if_owned(val)); // MAYTHROW
		}
	}

	template<typename Key, typename T, typename Hash, typename KeyEqual, typename Allocator, typename ...Args, typename K>
	void map_emplace_or_assign(tc::unordered_map<Key, T, Hash, KeyEqual, Allocator>& map, K&& key, Args&& ...args) MAYTHROW {
		if (auto const pairitb = map.try_emplace(tc::reluctant_explicit_cast<std::remove_reference_t<decltype(map)>::key_type>(tc_move_if_owned(key)), tc_move_if_owned(args)...); !pairitb.second ) { // MAYTHROW
			tc::assign_explicit_cast( pairitb.first->second, tc_move_if_owned(args)... ); // MAYTHROW
		}
	}
}
