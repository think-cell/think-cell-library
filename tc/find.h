
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "sub_range.h"
#include "storage_for.h"
#include "for_each_xxx.h"
#include "array.h"

#include <boost/next_prior.hpp>

namespace tc {

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	decltype(auto) find_unique_if(Rng&& rng, Pred pred) noexcept {
		auto const itEnd=tc::end(rng);
		for( auto it=tc::begin(rng); it!=itEnd; ++it ) {
			auto && ref=*it;
			if( pred(ref) ) {
				_ASSERT( std::none_of( boost::next(it), itEnd, pred ) );
				return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
		return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred, std::enable_if_t<RangeReturn<Rng>::requires_iterator>* = nullptr >
	decltype(auto) find_first_if(Rng&& rng, Pred&& pred) MAYTHROW {
		auto const itEnd=tc::end(rng);
		for( auto it=tc::begin(rng); it!=itEnd; ++it ) {
			auto && ref = *it;
			if (pred(ref)) {
				return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
		}
		return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred, std::enable_if_t<!RangeReturn<Rng>::requires_iterator>* = nullptr >
	typename RangeReturn<Rng>::type find_first_if(Rng&& rng, Pred&& pred) MAYTHROW {
		tc::storage_for<typename RangeReturn<Rng>::type> ot;
		if(tc::break_ == tc::for_each(std::forward<Rng>(rng), [&](auto&& t) noexcept {
			if (tc::bool_cast(pred(t))) {
				ot.ctor(RangeReturn<Rng>::pack_element(std::forward<decltype(t)>(t)));
				return tc::break_;
			} else {
				return tc::continue_;
			}
		})) {
			scope_exit(ot.dtor());
			return *tc_move(ot);
		} else {
			return RangeReturn<Rng>::pack_no_element();
		}
	}

	namespace find_last_if_detail {
		template< template<typename> class RangeReturn, typename Rng, typename Pred >
		decltype(auto) find_last_if(Rng&& rng, Pred pred, boost::iterators::bidirectional_traversal_tag) noexcept {
			auto itBegin=tc::begin(rng);
			for( auto it=tc::end(rng); it!=itBegin; ) {
				--it;
				auto && ref = *it;
				if (pred(ref)) return RangeReturn<Rng>::pack_element(it,std::forward<Rng>(rng),tc_move_if_owned(ref));
			}
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		}

		template< template<typename> class RangeReturn, typename Rng, typename Pred >
		decltype(auto) find_last_if(Rng&& rng, Pred pred, boost::iterators::forward_traversal_tag) noexcept {
			auto const itEnd=tc::end(rng);
			for( auto itFound=tc::begin(rng); itFound!=itEnd; ++itFound ) {
				tc::array<tc::storage_for<tc::iterator_cache<decltype(itFound)>>, 2> aic;
				int iFound = 0;
				aic[iFound].ctor(itFound);
				scope_exit(aic[iFound].dtor()); //iFound captured by reference
				if (pred(**aic[iFound])) {
					for (auto itNext = boost::next(itFound); itNext!=itEnd; ++itNext) {
						aic[1 - iFound].ctor(itNext);
						if (pred(**aic[1 - iFound])) {
							iFound = 1 - iFound;
						}
						aic[1 - iFound].dtor();
					}
					return RangeReturn<Rng>::pack_element(tc_move(itFound),std::forward<Rng>(rng),*tc_move_always(*aic[iFound]));
				}
			}
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename Pred >
	decltype(auto) find_last_if(Rng&& rng, Pred pred) noexcept {
		return find_last_if_detail::find_last_if<RangeReturn>(
			std::forward<Rng>(rng),
			tc_move(pred),
			typename boost::range_traversal<Rng>::type()
		);
	}

	template< typename Rng, typename It, typename Func >
	auto for_each_iterator_pair_outwards(Rng&& rng, It itOrigin, bool bSkipSelf, Func func) noexcept
		-> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::ptr_range<It> const&>())), INTEGRAL_CONSTANT(tc::continue_)>
	{
		tc::array<It, 2> const aitLimit(tc::aggregate_tag, tc::begin(rng), tc::end(rng));
		tc::array<It, 2> ait(tc::fill_tag, tc_move(itOrigin));

		if (!bSkipSelf) {
			_ASSERT(tc_back(aitLimit) != tc_back(ait));
			RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::take_first(tc::as_const(ait))));
			++tc_back(ait);
		} else if(tc_back(aitLimit) != tc_back(ait)) {
			++tc_back(ait);
		}

		for (;;) {
			if (tc_front(aitLimit) == tc_front(ait)) {
				for (; tc_back(ait) != tc_back(aitLimit); ++tc_back(ait)) {
					RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::drop_first(tc::as_const(ait))));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
			if (tc_back(aitLimit) == tc_back(ait)) {
				for (; tc_front(ait) != tc_front(aitLimit); ) {
					--tc_front(ait);
					RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::take_first(tc::as_const(ait))));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
			--tc_front(ait);
			RETURN_IF_BREAK(tc::continue_if_not_break(func, tc::as_const(ait)));
			++tc_back(ait);
		}
	}

	template < template<typename> class RangeReturn, typename Rng, typename It, typename Pred>
	decltype(auto) find_closest_if(Rng&& rng, It it, bool bSkipSelf, Pred pred) noexcept {
		tc::storage_for<tc::iterator_cache<It>> oitc;
		if (tc::break_ == tc::for_each_iterator_pair_outwards(rng, tc_move(it), bSkipSelf, [&](auto const& rngit) noexcept {
			return tc::for_each(rngit, [&](auto const& it) noexcept {
				oitc.ctor(it);
				if (pred(**tc::as_const(oitc))) { return tc::break_; }
				oitc.dtor();
				return tc::continue_;
			});
		})) {
			scope_exit(oitc.dtor());
			return RangeReturn<Rng>::pack_element(oitc->iterator(), std::forward<Rng>(rng), **tc_move(oitc));
		} else {
			return RangeReturn<Rng>::pack_no_element(std::forward<Rng>(rng));
		}
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) find_unique(Rng&& rng, T const& t) noexcept {
		return find_unique_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) find_first(Rng&& rng, T const& t) noexcept {
		return tc::find_first_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}

	template< template<typename> class RangeReturn, typename Rng, typename T >
	decltype(auto) find_last(Rng&& rng, T const& t) noexcept {
		return tc::find_last_if<RangeReturn>( std::forward<Rng>(rng), [&](auto const& _) noexcept { return tc::equal_to(_, t); } );
	}
}
