
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/noncopyable.h"
#include "../base/as_lvalue.h"
#include "../base/modified.h"

#include "for_each.h"
#include "../base/assign.h"

#include <boost/range/iterator.hpp>

#include <functional>
#include <unordered_set>
#include <unordered_map>

namespace tc{
	namespace no_adl {
		template< typename Lhs, typename Rhs, typename Enable = void >
		struct has_parse_match final : tc::constant<false> {};

		template< typename Lhs, typename Rhs >
		struct has_parse_match<Lhs, Rhs, tc::void_t<decltype(std::declval<Lhs const&>().parse_match(std::declval<Rhs const&>()))>> final : tc::constant<true> {};
	}

	template<typename Lhs, typename Rhs, std::enable_if_t<!no_adl::has_parse_match<Lhs, Rhs>::value && !no_adl::has_parse_match<Rhs, Lhs>::value>* =nullptr>
	[[nodiscard]] constexpr auto equal_to_or_parse_match(Lhs const& lhs, Rhs const& rhs) return_decltype_noexcept( tc::equal_to(lhs,rhs) )

	template<typename Lhs, typename Rhs, std::enable_if_t<no_adl::has_parse_match<Lhs, Rhs>::value && !no_adl::has_parse_match<Rhs, Lhs>::value>* =nullptr>
	[[nodiscard]] constexpr bool equal_to_or_parse_match(Lhs const& lhs, Rhs const& rhs) noexcept { return lhs.parse_match(rhs); }

	template<typename Lhs, typename Rhs, std::enable_if_t<!no_adl::has_parse_match<Lhs, Rhs>::value && no_adl::has_parse_match<Rhs, Lhs>::value>* =nullptr>
	[[nodiscard]] constexpr bool equal_to_or_parse_match(Lhs const& lhs, Rhs const& rhs) noexcept { return rhs.parse_match(lhs); }

	DEFINE_FN( equal_to_or_parse_match );

	//-------------------------------------------------------------------------------------------------------------------------
	// equal - check whether two ranges are equal - overloaded for combinations of generator and iterator based ranges

	namespace equal_impl {
		namespace no_adl {
			template<typename Pred>
			struct reverse_pred {
				Pred& m_pred;

				constexpr reverse_pred(Pred& pred) noexcept
					: m_pred(pred)
				{}

				template<typename Arg1, typename Arg2>
				constexpr auto operator()(Arg1&& arg1, Arg2&& arg2) const& noexcept {
					return m_pred(std::forward<Arg2>(arg2), std::forward<Arg1>(arg1));
				}
			};

			template<typename It, typename ItEnd, typename Pred>
			struct is_equal_elem /*final*/ {
				constexpr explicit is_equal_elem(
					It& it,
					ItEnd itEnd,
					Pred& pred
				) noexcept
					: m_it(it)
					, m_itEnd(tc_move(itEnd))
					, m_pred(pred)
				{}

				template<typename Elem>
				constexpr break_or_continue operator()(Elem const& elem) const& noexcept {
					if (m_it == m_itEnd || !tc::explicit_cast<bool>(m_pred(tc::as_const(*m_it), elem))) { return tc::break_; }
					++m_it;
					return tc::continue_;
				}

			private:
				It& m_it;
				ItEnd m_itEnd;
				Pred& m_pred;
			};

			// TODO: this does not protect us against inputs such as transform(unordered_set)
			template<typename X> struct is_unordered_range : tc::constant<false> {};
			template<typename... X> struct is_unordered_range<std::unordered_set<X...>> : tc::constant<true> {};
			template<typename... X> struct is_unordered_range<std::unordered_map<X...>> : tc::constant<true> {};
		}

		template<typename It, typename ItEnd, typename RRng, typename Pred>
		[[nodiscard]] constexpr bool starts_with(It& it, ItEnd itEnd, RRng&& rrng, Pred pred) noexcept(noexcept(tc::continue_ == tc::for_each(tc_move_if_owned(rrng), no_adl::is_equal_elem<It, ItEnd, Pred>(it, tc_move(itEnd), pred)))) {
			static_assert(!no_adl::is_unordered_range<tc::decay_t<RRng>>::value);
			return tc::continue_ == tc::for_each(tc_move_if_owned(rrng), no_adl::is_equal_elem<It, ItEnd, Pred>(it, tc_move(itEnd), pred)); // MAYTHROW
		}
	}

	template<typename RangeReturn, typename LRng, typename RRng, typename Pred>
	[[nodiscard]] constexpr decltype(auto) starts_with(LRng&& lrng, RRng const& rrng, Pred&& pred) noexcept {
		static_assert(!equal_impl::no_adl::is_unordered_range<tc::decay_t<LRng>>::value);
		auto itlrng = tc::begin(lrng);
		return equal_impl::starts_with(itlrng, tc::end(lrng), rrng, std::forward<Pred>(pred))
			? RangeReturn::pack_border(itlrng, std::forward<LRng>(lrng))
			: RangeReturn::pack_no_border(std::forward<LRng>(lrng));
	}

	template<typename RangeReturn, typename LRng, typename RRng>
	[[nodiscard]] constexpr decltype(auto) starts_with(LRng&& lrng, RRng const& rrng) noexcept {
		return starts_with<RangeReturn>(std::forward<LRng>(lrng), rrng, tc::fn_equal_to_or_parse_match());
	}

	template<typename LRng, typename RRng, typename Pred> requires is_range_with_iterators<LRng>::value
	[[nodiscard]] constexpr bool equal(LRng const& lrng, RRng&& rrng, Pred&& pred) MAYTHROW {
		static_assert(!equal_impl::no_adl::is_unordered_range<tc::decay_t<LRng>>::value);
		constexpr bool bHasSize=tc::has_size<LRng>::value && tc::has_size<RRng>::value;
		if constexpr(bHasSize) {
			if(tc::size(lrng)!=tc::size(rrng)) return false;
		}
		auto it = tc::begin(lrng);
		auto_cref(itEnd, tc::end(lrng));
		return equal_impl::starts_with(it,itEnd,tc_move_if_owned(rrng),std::forward<Pred>(pred)) && (bHasSize || itEnd==it); // MAYTHROW
	}

	// forward to the symetric case above
	template<typename LRng, typename RRng, typename Pred> requires (!is_range_with_iterators< LRng >::value && is_range_with_iterators< RRng >::value)
	[[nodiscard]] constexpr bool equal(LRng&& lrng, RRng const& rrng, Pred pred) noexcept {
		return tc::equal(rrng, tc_move_if_owned(lrng), equal_impl::no_adl::reverse_pred<Pred>(pred));
	}

	// is_arithmetic helpful for generic programming
	// only do if semantics are clear-cut
	template<typename T, typename Pred> requires std::is_arithmetic< T >::value
	[[nodiscard]] constexpr bool equal(T const& lhs, T const& rhs, Pred pred) noexcept {
		return pred(lhs,rhs);
	}

	template<typename R, typename S, typename T, typename U, typename Pred>
	[[nodiscard]] constexpr bool equal(std::pair<R,S> const& lhs, std::pair<T,U> const& rhs, Pred pred) noexcept {
		return tc::equal(lhs.first, rhs.first, pred) && tc::equal(lhs.second, rhs.second, pred);
	}

	// forward the non predicate version
	template<typename LRng, typename RRng>
	[[nodiscard]] constexpr bool equal(LRng&& lrng, RRng&& rrng) return_MAYTHROW(
		tc::equal(tc_move_if_owned(lrng), tc_move_if_owned(rrng), tc::fn_equal_to_or_parse_match())
	)

	DEFINE_FN2(tc::equal, fn_equal) // no DEFINE_FN to avoid ADL

	// boost::ends_with does not work with boost::range_iterator<transform_range>::type returning by value because it has input_iterator category
	template<typename RangeReturn, typename LRng, typename RRng, typename Pred=tc::fn_equal_to_or_parse_match>
	[[nodiscard]] constexpr decltype(auto) ends_with(LRng&& lrng, RRng const& rrng, Pred pred=Pred()) noexcept {
		auto itL=tc::end(lrng);
		auto itR=tc::end(rrng);
		auto const itBeginL=tc::begin(lrng);
		auto const itBeginR=tc::begin(rrng);
		for(;;) {
			if( itR==itBeginR ) return RangeReturn::pack_border(itL, std::forward<LRng>(lrng));
			if( itL==itBeginL ) return RangeReturn::pack_no_border(std::forward<LRng>(lrng));
			--itR;
			--itL;
			if( !tc::explicit_cast<bool>(pred(tc::as_const(*itL),tc::as_const(*itR))) ) return RangeReturn::pack_no_border(std::forward<LRng>(lrng));
		}
	}

	namespace value_equal_to_detail {
		template<typename T>
		concept safe_value =
			!tc::is_range_with_iterators<T>::value ||
			!tc::is_range_with_iterators<tc::type::only_t<typename tc::is_instance<std::optional, T>::arguments>>::value;
	}

	template <typename Lhs, typename Rhs>
	[[nodiscard]] constexpr bool value_equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
		if constexpr( tc::is_range_with_iterators<Lhs>::value && tc::is_range_with_iterators<Rhs>::value ) {
			STATICASSERTEQUAL( (tc::is_instance<std::optional, Lhs>::value), (tc::is_instance<std::optional, Rhs>::value) );
			return tc::equal(lhs, rhs, tc::fn_equal_to());
		} else {
			static_assert( value_equal_to_detail::safe_value<Lhs> );
			static_assert( value_equal_to_detail::safe_value<Rhs> );
			return tc::equal_to(lhs, rhs);
		}
	}
}

