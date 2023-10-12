
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
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
	[[nodiscard]] constexpr auto equal_to_or_parse_match(Lhs const& lhs, Rhs const& rhs) return_decltype_MAYTHROW( tc::equal_to(lhs,rhs) )

	template<typename Lhs, typename Rhs, std::enable_if_t<no_adl::has_parse_match<Lhs, Rhs>::value && !no_adl::has_parse_match<Rhs, Lhs>::value>* =nullptr>
	[[nodiscard]] constexpr auto equal_to_or_parse_match(Lhs const& lhs, Rhs const& rhs) return_decltype_MAYTHROW( tc::implicit_cast<bool>(lhs.parse_match(rhs)) )

	template<typename Lhs, typename Rhs, std::enable_if_t<!no_adl::has_parse_match<Lhs, Rhs>::value && no_adl::has_parse_match<Rhs, Lhs>::value>* =nullptr>
	[[nodiscard]] constexpr auto equal_to_or_parse_match(Lhs const& lhs, Rhs const& rhs) return_decltype_MAYTHROW( tc::implicit_cast<bool>(rhs.parse_match(lhs)) )

	tc_define_fn( equal_to_or_parse_match );

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
					return m_pred(tc_move_if_owned(arg2), tc_move_if_owned(arg1));
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

				template<typename Elem> requires requires(Pred& pred, It& it, Elem const& elem) {pred(tc::as_const(*it), elem);}
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
		return equal_impl::starts_with(itlrng, tc::end(lrng), rrng, tc_move_if_owned(pred))
			? RangeReturn::pack_border(itlrng, tc_move_if_owned(lrng))
			: RangeReturn::pack_no_border(tc_move_if_owned(lrng));
	}

	template<typename RangeReturn, typename LRng, typename RRng>
	[[nodiscard]] constexpr decltype(auto) starts_with(LRng&& lrng, RRng const& rrng) noexcept {
		return starts_with<RangeReturn>(tc_move_if_owned(lrng), rrng, tc::fn_equal_to_or_parse_match());
	}

	template<tc::range_with_iterators LRng, typename RRng, typename Pred> requires
		requires(LRng const& lrng, RRng&& rrng){equal_impl::starts_with(tc::as_lvalue(tc::begin(lrng)), tc::as_const(tc::as_lvalue(tc::end(lrng))), tc_move_if_owned(rrng), std::declval<Pred>());}
	[[nodiscard]] constexpr bool equal(LRng const& lrng, RRng&& rrng, Pred&& pred) MAYTHROW {
		static_assert(!equal_impl::no_adl::is_unordered_range<tc::decay_t<LRng>>::value);
		constexpr bool bHasSize=tc::has_size<LRng> && tc::has_size<RRng>;
		if constexpr(bHasSize) {
			if(tc::size(lrng)!=tc::size(rrng)) return false;
		}
		auto it = tc::begin(lrng);
		tc_auto_cref(itEnd, tc::end(lrng));
		return equal_impl::starts_with(it,itEnd,tc_move_if_owned(rrng),tc_move_if_owned(pred)) && (bHasSize || itEnd==it); // MAYTHROW
	}

	// forward to the symmetric case above
	template<typename LRng, tc::range_with_iterators RRng, typename Pred> requires (!tc::range_with_iterators< LRng >)
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

	DEFINE_FN2(tc::equal, fn_equal) // no tc_define_fn to avoid ADL

	// boost::ends_with does not work with boost::range_iterator<transform_range>::type returning by value because it has input_iterator category
	template<typename RangeReturn, typename LRng, typename RRng, typename Pred=tc::fn_equal_to_or_parse_match>
	[[nodiscard]] constexpr decltype(auto) ends_with(LRng&& lrng, RRng const& rrng, Pred pred=Pred()) noexcept {
		auto itL=tc::end(lrng);
		auto itR=tc::end(rrng);
		auto const itBeginL=tc::begin(lrng);
		auto const itBeginR=tc::begin(rrng);
		for(;;) {
			if( itR==itBeginR ) return RangeReturn::pack_border(itL, tc_move_if_owned(lrng));
			if( itL==itBeginL ) return RangeReturn::pack_no_border(tc_move_if_owned(lrng));
			--itR;
			--itL;
			if( !tc::explicit_cast<bool>(pred(tc::as_const(*itL),tc::as_const(*itR))) ) return RangeReturn::pack_no_border(tc_move_if_owned(lrng));
		}
	}

	namespace value_equal_to_detail {
		template<typename T>
		concept safe_value =
			!tc::range_with_iterators<T> ||
			!tc::range_with_iterators<tc::type::only_t<typename tc::is_instance<T, std::optional>::arguments>>;
	}

	template <typename Lhs, typename Rhs>
	[[nodiscard]] constexpr bool value_equal_to(Lhs const& lhs, Rhs const& rhs) noexcept {
		if constexpr( tc::range_with_iterators<Lhs> && tc::range_with_iterators<Rhs> ) {
			STATICASSERTEQUAL( (tc::instance<Lhs, std::optional>), (tc::instance<Rhs, std::optional>) );
			return tc::equal(lhs, rhs, tc::fn_equal_to());
		} else {
			static_assert( value_equal_to_detail::safe_value<Lhs> );
			static_assert( value_equal_to_detail::safe_value<Rhs> );
			return tc::equal_to(lhs, rhs);
		}
	}
}

