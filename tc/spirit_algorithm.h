
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "algorithm.h"
#include "spirit.h"

namespace tc {
	template<template<typename> class RangeReturn, typename RngWhere, typename RngWhat, typename Pred, std::enable_if_t<!std::is_base_of<x3::parser_base, RngWhat>::value>* = nullptr>
	decltype(auto) search(RngWhere&& rngWhere, RngWhat const& rngWhat, Pred pred) noexcept {
		auto const itWhereEnd = tc::end(rngWhere);
		auto const itWhatBegin = tc::begin(rngWhat);
		auto const itWhatEnd = tc::end(rngWhat);
		for (auto itWhere = tc::begin(rngWhere);; ++itWhere) {
			auto itWhere2 = itWhere;
			auto itWhat = itWhatBegin;
			for (;;) {
				if (itWhat == itWhatEnd) {
					return RangeReturn<RngWhere>::pack_view(std::forward<RngWhere>(rngWhere), tc_move(itWhere), tc_move(itWhere2));
				}
				if (itWhere2 == itWhereEnd) {
					return RangeReturn<RngWhere>::pack_no_element(std::forward<RngWhere>(rngWhere));
				}
				if (!pred(*itWhere2, *itWhat)) break;
				++itWhere2;
				++itWhat;
			}
		}
	}
	template<template<typename> class RangeReturn, typename RngWhere, typename RngWhat, std::enable_if_t<!std::is_base_of<x3::parser_base, RngWhat>::value>* = nullptr>
	decltype(auto) search(RngWhere&& rngWhere, RngWhat const& rngWhat) noexcept {
		return tc::search<RangeReturn>(std::forward<RngWhere>(rngWhere), rngWhat, tc::fn_equal_to());
	}

	template<template <typename> class RangeReturn, typename Rng, typename Expr, std::enable_if_t<std::is_base_of<x3::parser_base, Expr>::value>* = nullptr>
	decltype(auto) search(Rng&& rng, Expr const& expr) noexcept {
		auto const itEnd = tc::end(rng);
		for (auto it = tc::begin(rng); it != itEnd; ++it) {
			auto itEndParse = it;
			if (tc::parse_iterator(itEndParse, itEnd, expr)) {
				return RangeReturn<Rng>::pack_view(std::forward<Rng>(rng), tc_move(it), tc_move(itEndParse));
			}
		}
		return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
	}

	// cannot use list::remove because T may not be list::value_type
	// cannot use key-based lookup for set/map because T may not be Cont::value_type and !Cont::predicate()(a,b) && !Cont::predicate()(b,a) may not be the same as ==
	template<typename Cont, typename T, std::enable_if_t<!std::is_base_of<x3::parser_base, T>::value>* = nullptr>
	void remove_inplace(Cont& cont, T const& t) noexcept {
		tc::filter_inplace( cont, [&](auto const& _) noexcept { return !tc::equal_to(_, t); } );
	}

	template<typename Cont, typename Expr, std::enable_if_t<std::is_base_of<x3::parser_base, Expr>::value>* = nullptr>
	void remove_inplace(Cont& cont, typename boost::range_iterator<Cont>::type it, Expr const& expr) noexcept {
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

	template<typename Cont, typename Expr, std::enable_if_t<std::is_base_of<x3::parser_base, Expr>::value>* = nullptr>
	void remove_inplace(Cont& cont, Expr const& expr) noexcept {
		tc::remove_inplace(cont, tc::begin(cont), expr);
	}
}
