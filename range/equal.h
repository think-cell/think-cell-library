//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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

#include "quantifier.h"
#include "for_each.h"
#include "break_or_continue.h"
#include "range_defines.h"
#include "meta.h"

#include <boost/range/iterator.hpp>

#include "noncopyable.h"

#include <functional>
#include <unordered_set>
#include <unordered_map>

namespace tc{

	//-------------------------------------------------------------------------------------------------------------------------
	// equal - check whether two ranges are equal - overloaded for combinations of generator and iterator based ranges

	namespace equal_impl {

		template<typename Rng, typename Pred>
		struct is_equal_elem final : tc::noncopyable {
			is_equal_elem(Rng const& rng, Pred&& pred_) noexcept : it(boost::begin(rng)), end(boost::end(rng)), pred(std::forward<Pred>(pred_)), equal(true) {}

			template<typename Elem>
			break_or_continue operator()(Elem const& elem) & noexcept {
				if (it == end || !tc::bool_cast(pred(elem, *it))) { equal = false; return break_; }
				++it;
				return continue_;
			}

			bool result() const& noexcept { return equal && it == end; }

			private:
				using iterator_type = typename boost::range_iterator<Rng const>::type;

				iterator_type it;
				iterator_type end;
				tc::decay_t<Pred> pred;
				bool equal;
		};
	}

	template<typename LRng, typename RRng, typename Pred, std::enable_if_t<is_range_with_iterators< RRng >::value>* = nullptr>
		bool equal(LRng const& lrng, RRng const& rrng, Pred&& pred) noexcept {
		// TODO: this does not protect us against inputs such as transform(unordered_set)
		static_assert(!tc::is_instance<std::unordered_set, LRng>::value, "");
		static_assert(!tc::is_instance<std::unordered_map, LRng>::value, "");
		static_assert(!tc::is_instance<std::unordered_set, RRng>::value, "");
		static_assert(!tc::is_instance<std::unordered_map, RRng>::value, "");

		equal_impl::is_equal_elem<RRng, Pred> equalpred(rrng, std::forward<Pred>(pred));
		tc::for_each(lrng, std::ref(equalpred));

		return equalpred.result();
	}

	// forward to the symetric case above
	template<typename LRng, typename RRng, typename Pred, std::enable_if_t<!is_range_with_iterators< RRng >::value && is_range_with_iterators< LRng >::value>* = nullptr>
		bool equal(LRng const& lrng, RRng const& rrng, Pred pred) noexcept {
		return tc::equal(rrng, lrng, std::bind(pred, std::placeholders::_2, std::placeholders::_1)); 
	}

	// is_arithmetic helpful for generic programming
	// only do if semantics are clear-cut
	template<typename T, typename Pred, std::enable_if_t<std::is_arithmetic< T >::value>* = nullptr>
		bool equal(T const& lhs, T const& rhs, Pred pred) noexcept {
		return pred(lhs,rhs);
	}

	// forward the non predicate version
	template<typename LRng, typename RRng>
	bool equal(LRng const& lrng, RRng const& rrng) noexcept {
		return tc::equal(lrng, rrng, tc::fn_equal_to());
	}

	// boost::ends_with does not work with boost::range_iterator<transform_range>::type returning by value because it has input_iterator category
	template<typename LRng, typename RRng, typename Pred=tc::fn_equal_to>
	bool ends_with(LRng const& lrng, RRng const& rrng, Pred pred=Pred()) noexcept {
		auto itL=boost::end(lrng);
		auto itR=boost::end(rrng);
		auto const itBeginL=boost::begin(lrng);
		auto const itBeginR=boost::begin(rrng);
		for(;;) {
			if( itR==itBeginR ) return true;
			if( itL==itBeginL ) return false;
			--itR;
			--itL;
			if( !tc::bool_cast(pred(*itL,*itR)) ) return false;
		}
	}
}

