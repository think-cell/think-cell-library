
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "for_each.h"
#include "break_or_continue.h"
#include "range_defines.h"
#include "meta.h"
#include "noncopyable.h"
#include "as_lvalue.h"

#include <boost/range/iterator.hpp>

#include <functional>
#include <unordered_set>
#include <unordered_map>

namespace tc{

	//-------------------------------------------------------------------------------------------------------------------------
	// equal - check whether two ranges are equal - overloaded for combinations of generator and iterator based ranges

	namespace equal_impl {
		template<typename Rng, typename Pred>
		struct is_equal_elem_base {};

		template<typename It>
		struct is_equal_elem_base<It, tc::fn_equal_to> {
			using sink_value_type = tc::decay_t<decltype(*std::declval<It&>())>;
		};

		template<typename Pred>
		struct reverse_pred {
			Pred& m_pred;

			reverse_pred(Pred& pred) noexcept
				: m_pred(pred)
			{}

			template<typename Arg1, typename Arg2>
			auto operator()(Arg1&& arg1, Arg2&& arg2) const& noexcept {
				return m_pred(std::forward<Arg2>(arg2), std::forward<Arg1>(arg1));
			}
		};

		template<typename It>
		struct is_equal_elem_base<It, reverse_pred<tc::fn_equal_to>> {
			using sink_value_type = tc::decay_t<decltype(*std::declval<It&>())>;
		};

		template<typename It, typename ItEnd, typename Pred>
		struct is_equal_elem /*final*/ : is_equal_elem_base<It, Pred> {
			explicit is_equal_elem(
				Pred& pred,
				It& it,
				ItEnd itEnd
			) noexcept
				: m_it(it)
				, m_itEnd(tc_move(itEnd))
				, m_pred(pred)
			{}

			template<typename Elem>
			break_or_continue operator()(Elem const& elem) const& noexcept {
				if (m_it == m_itEnd || !tc::bool_cast(m_pred(*m_it, elem))) { return tc::break_; }
				++m_it;
				return tc::continue_;
			}

			It& m_it;
			ItEnd m_itEnd;

			Pred& m_pred;
		};

		template<typename It, typename ItEnd, typename RRng, typename Pred>
		bool starts_with(It& it, ItEnd itEnd, RRng const& rrng, Pred pred) noexcept {
			// TODO: this does not protect us against inputs such as transform(unordered_set)
			static_assert(!tc::is_instance<std::unordered_set, RRng>::value);
			static_assert(!tc::is_instance<std::unordered_map, RRng>::value);
			return tc::continue_ == tc::for_each(rrng, equal_impl::is_equal_elem<It, ItEnd, Pred>(pred, it, tc_move(itEnd)));
		}
	}

	template<typename LRng, typename RRng, typename Pred>
	bool starts_with(LRng const& lrng, RRng const& rrng, Pred&& pred) noexcept {
		// TODO: this does not protect us against inputs such as transform(unordered_set)
		static_assert(!tc::is_instance<std::unordered_set, LRng>::value);
		static_assert(!tc::is_instance<std::unordered_map, LRng>::value);
		return equal_impl::starts_with(tc::as_lvalue(tc::begin(lrng)), tc::end(lrng), rrng, std::forward<Pred>(pred));
	}

	template<typename LRng, typename RRng>
	bool starts_with(LRng const& lrng, RRng const& rrng) noexcept {
		return starts_with(lrng,rrng,tc::fn_equal_to());
	}

	template<typename LRng, typename RRng, typename Pred, std::enable_if_t<is_range_with_iterators< LRng >::value>* = nullptr>
	bool equal(LRng const& lrng, RRng const& rrng, Pred&& pred) noexcept {
		// TODO: this does not protect us against inputs such as transform(unordered_set)
		static_assert(!tc::is_instance<std::unordered_set, LRng>::value);
		static_assert(!tc::is_instance<std::unordered_map, LRng>::value);
		auto it = tc::begin(lrng);
		auto_cref(itEnd, tc::end(lrng));
		return equal_impl::starts_with(it,itEnd,rrng,std::forward<Pred>(pred)) && itEnd == it;
	}

	// forward to the symetric case above
	template<typename LRng, typename RRng, typename Pred, std::enable_if_t<!is_range_with_iterators< LRng >::value && is_range_with_iterators< RRng >::value>* = nullptr>
	bool equal(LRng const& lrng, RRng const& rrng, Pred pred) noexcept {
		return tc::equal(rrng, lrng, equal_impl::reverse_pred<Pred>(pred));
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
		auto itL=tc::end(lrng);
		auto itR=tc::end(rrng);
		auto const itBeginL=tc::begin(lrng);
		auto const itBeginR=tc::begin(rrng);
		for(;;) {
			if( itR==itBeginR ) return true;
			if( itL==itBeginL ) return false;
			--itR;
			--itL;
			if( !tc::bool_cast(pred(*itL,*itR)) ) return false;
		}
	}

	template <typename Lhs, typename Rhs, std::enable_if_t<
		tc::is_range_with_iterators<Lhs>::value && tc::is_range_with_iterators<Rhs>::value
	>* = nullptr>
	bool value_equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
		return tc::equal(lhs, rhs);
	}

	template <typename Lhs, typename Rhs, std::enable_if_t<
		!tc::is_range_with_iterators<Lhs>::value && !tc::is_range_with_iterators<Rhs>::value
	>* = nullptr>
	bool value_equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
		return tc::equal_to(lhs, rhs);
	}
}

