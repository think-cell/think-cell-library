
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../range/meta.h"
#include "../algorithm/algorithm.h"
#include "spirit.h"

namespace tc {
	template<typename RangeReturn, typename Rng, tc::derived_from<x3::parser_base> Expr>
	[[nodiscard]] decltype(auto) starts_with_expr(Rng&& rng, Expr const& expr) noexcept {
		auto itEndParse = tc::begin(rng);
		if (tc::parse_iterator(itEndParse, tc::end(rng), expr)) {
			return RangeReturn::pack_border(tc_move(itEndParse), tc_move_if_owned(rng));
		}
		return RangeReturn::pack_no_border(tc_move_if_owned(rng));
	}

	// 1 or more elements from rngWhat are required to match each element from rngWhere
	template<typename RangeReturn, typename RngWhere, typename RngWhat, typename Pred>
	[[nodiscard]] decltype(auto) search_first_impl(RngWhere&& rngWhere, RngWhat const& rngWhat, Pred predConsume) noexcept {
		auto const itWhereEnd = tc::end(rngWhere);
		auto const itWhatBegin = tc::begin(rngWhat);
		auto const itWhatEnd = tc::end(rngWhat);
		for (auto itWhere = tc::begin(rngWhere);; ++itWhere) {
			auto itWhere2 = itWhere;
			auto itWhat = itWhatBegin;
			for (;;) {
				if (itWhatEnd == itWhat) {
					return RangeReturn::pack_view(tc_move_if_owned(rngWhere), tc_move(itWhere), tc_move(itWhere2));
				}
				if (itWhereEnd == itWhere2) {
					return RangeReturn::pack_no_element(tc_move_if_owned(rngWhere));
				}
				if (!predConsume(tc::as_const(*itWhere2), itWhat, itWhatEnd)) break;
				++itWhere2;
			}
		}
	}

	template<typename RangeReturn, typename RngWhere, typename RngWhat, typename Pred> requires (!tc::derived_from<RngWhat, x3::parser_base>)
	[[nodiscard]] decltype(auto) search_first(RngWhere&& rngWhere, RngWhat const& rngWhat, Pred pred) noexcept {
		return search_first_impl<RangeReturn>(tc_move_if_owned(rngWhere), rngWhat, [&](auto const& valWhere, auto& itWhat, auto const& /*itWhatEnd*/) noexcept {
			return pred(valWhere, tc::as_const(*itWhat)) && (++itWhat, true);
		});
	}

	template<typename RangeReturn, typename RngWhere, typename RngWhat>
	[[nodiscard]] decltype(auto) search_first(RngWhere&& rngWhere, RngWhat const& rngWhat) noexcept {
		return tc::search_first<RangeReturn>(tc_move_if_owned(rngWhere), rngWhat, tc::fn_equal_to_or_parse_match());
	}

	template<typename RangeReturn, typename Rng, tc::derived_from<x3::parser_base> Expr>
	decltype(auto) search_first(Rng&& rng, Expr const& expr) noexcept {
		auto const itEnd = tc::end(rng);
		for (auto it = tc::begin(rng); it != itEnd; ++it) {
			auto itEndParse = it;
			if (tc::parse_iterator(itEndParse, itEnd, expr)) {
				return RangeReturn::pack_view(tc_move_if_owned(rng), tc_move(it), tc_move(itEndParse));
			}
		}
		return RangeReturn::pack_no_element(tc_move_if_owned(rng));
	}

	template<typename RangeReturn, typename RngWhere, typename What>
	decltype(auto) search_unique(RngWhere&& rngWhere, What const& what) noexcept {
		if(auto const orng=tc::search_first<tc::return_view_or_none>(rngWhere, what)) {
			auto itEnd=VERIFYPRED(tc::end(*orng), !tc::search_first<tc::return_bool>(tc::drop(rngWhere, _), what)); // do not inline, rngWhere is forwarded
			return RangeReturn::pack_view(tc_move_if_owned(rngWhere), tc::begin(*orng), tc_move(itEnd));
		} else {
			return RangeReturn::pack_no_element(tc_move_if_owned(rngWhere));
		}
	}

	// cannot use list::remove because T may not be list::value_type
	// cannot use key-based lookup for set/map because T may not be Cont::value_type and !Cont::predicate()(a,b) && !Cont::predicate()(b,a) may not be the same as ==
	template<typename Cont, typename T> requires (!tc::derived_from<T, x3::parser_base>)
	void remove_inplace(Cont& cont, T const& t) noexcept {
		tc::filter_inplace( cont, [&](auto const& _) noexcept { return !tc::equal_to(_, t); } );
	}

	template<typename Cont, tc::derived_from<x3::parser_base> Expr>
	void remove_inplace(Cont& cont, tc::iterator_t<Cont> it, Expr const& expr) noexcept {
		for(auto const itEnd = tc::end(cont); it != itEnd; ++it) {
			auto itBegin=it;
			if(tc::parse_iterator(it, itEnd, expr)) {
				tc::range_filter< tc::decay_t<Cont> > rngfilter(cont, itBegin);
				while(it != itEnd) {
					if(!tc::parse_iterator(it, itEnd, expr)) {
						rngfilter.keep(it++);  // may invalidate it, so move away first
					}
				}
				break;
			}
		}
	}

	template<typename Cont, tc::derived_from<x3::parser_base> Expr>
	void remove_inplace(Cont& cont, Expr const& expr) noexcept {
		tc::remove_inplace(cont, tc::begin(cont), expr);
	}

	// removes all original occurrences of rng within cont, but not occurrences that have been created during the process
	// so remove_all_inplace("aabb", "ab") -> "ab"
	template<typename Cont, typename Rng>
	void remove_all_inplace(Cont& cont, Rng const& rng) noexcept {
		_ASSERT(!tc::empty(rng)); // What does it mean to remove all empty subranges? Causes an infinite loop down below.
		auto const itEnd = tc::end(cont);
		auto it = tc::begin(cont);
		tc::range_filter< tc::decay_t<Cont> > rngfilter(cont, it);
		while (itEnd != it) {
			if (auto const itBorder = tc::starts_with<tc::return_border_or_null>(tc::slice(cont, it, itEnd), rng)) {
				it = itBorder;
			} else {
				rngfilter.keep(it++); // may invalidate it, so move away first
			}
		}
	}
}
