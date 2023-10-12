
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/invoke.h"
#include "../range/transform_adaptor.h"
#include "../variant.h"
#include "for_each.h"
#include "find.h"
#include "compare.h"
#include "any_accu.h"
#include <utility>

/////////////////////////////////////////////
// std::all/any/none_of on ranges

namespace tc {

template< typename Rng, typename Pred = tc::identity >
[[nodiscard]] constexpr bool any_of(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
	return tc::find_first_if<tc::return_bool>(tc_move_if_owned(rng), tc_move_if_owned(pred));
}

template< typename Rng, typename Pred = tc::identity >
[[nodiscard]] constexpr bool all_of(Rng&& rng, Pred&& pred = Pred()) MAYTHROW {
	return !tc::any_of(tc_move_if_owned(rng), std::not_fn(tc_move_if_owned(pred)));
}

// pair is in same order as if minmax_element( ..., operator<( bool, bool ) ) would have been used.
template< typename Rng >
[[nodiscard]] std::pair<bool,bool> all_any_of( Rng const& rng ) MAYTHROW {
	std::pair<bool,bool> pairb(true,false);
	tc::for_each(rng, [&](bool const b) noexcept {
		pairb.first=pairb.first && b;
		pairb.second=pairb.second || b;
		return continue_if( pairb.first || !pairb.second );
	} );
	return pairb;
}

template< typename Rng, typename Pred >
[[nodiscard]] std::pair<bool,bool> all_any_of(Rng const& rng, Pred&& pred) MAYTHROW {
	return all_any_of( tc::transform(rng,tc_move_if_owned(pred)) );
}

[[nodiscard]] inline bool eager_or(std::initializer_list<tc::bool_context> ab) MAYTHROW {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::any_of(ab);
}

[[nodiscard]] inline bool eager_and(std::initializer_list<tc::bool_context> ab) MAYTHROW {
	// use initializer list instead of variadic template: initializer list guarantees evaluation in order of appearance
	return tc::all_of(ab);
}

template< typename Rng, typename Pred >
[[nodiscard]] bool eager_any_of(Rng&& rng, Pred pred) MAYTHROW {
	tc::any_accu any;
	tc::for_each(rng, [&](auto&& t) noexcept {
		any(tc::invoke(pred, tc_move_if_owned(t)));
	});
	return any;
}

template< typename Rng, typename Pred >
[[nodiscard]] bool eager_all_of(Rng&& rng, Pred&& pred) MAYTHROW {
	return !tc::eager_any_of(tc_move_if_owned(rng), std::not_fn(tc_move_if_owned(pred)));
}

// all_same_element
namespace all_same_result {
	struct empty_t final {};
	struct not_same_t final {};

	template<typename T=void>
	struct never_empty /*final*/ {
		constexpr T operator()(empty_t) const& noexcept { _ASSERTFALSE; return tc::construct_default_or_terminate<T>(); }
	};
}

namespace no_adl {
	template<typename T, typename Equal=tc::fn_equal_to>
	struct [[nodiscard]] accumulator_all_same final {
		static_assert(tc::decayed<Equal>);

		using result_type = std::variant<all_same_result::empty_t, T, all_same_result::not_same_t>;

		constexpr accumulator_all_same() noexcept = default;
		constexpr accumulator_all_same(auto&& equal) noexcept : m_equal(tc_move_if_owned(equal)) {}

		template<typename U>
		constexpr auto operator()(U&& u) & MAYTHROW {
			if constexpr( std::is_same<accumulator_all_same, tc::decay_t<U>>::value ) {
				return tc::fn_visit(
					[&](all_same_result::empty_t) noexcept {
						return tc::continue_if(!std::holds_alternative<all_same_result::not_same_t>(m_result));
					},
					[&](auto&& t) MAYTHROW {
						return operator()(tc_move_if_owned(t)); // MAYTHROW
					},
					[&](all_same_result::not_same_t) noexcept {
						m_result=all_same_result::not_same_t();
						return tc::break_;
					}
				)(tc_move_if_owned(u).m_result_());
			} else {
				return tc::fn_visit(
					[&](all_same_result::empty_t) MAYTHROW {
						m_result=tc_move_if_owned(u); // MAYTHROW
						return tc::continue_;
					},
					[&](T const& t) noexcept {
						if(m_equal(t, tc::as_const(u))) {
							return tc::continue_;
						} else {
							m_result=all_same_result::not_same_t();
							return tc::break_;
						}
					},
					[](all_same_result::not_same_t) noexcept {
						return tc::break_;
					}
				)(m_result);
			}
		}

	private:
		PRIVATE_MEMBER_PUBLIC_ACCESSOR(result_type, m_result)
		Equal m_equal;
	};

	struct return_all_same_result;

	template<typename RangeReturn, typename Rng, typename Equal>
	struct all_same_element_impl;

	template<typename Rng, typename Equal>
	struct all_same_element_impl<return_all_same_result, Rng, Equal> final {
		static constexpr auto fn(Rng&& rng, Equal&& equal) MAYTHROW {
			accumulator_all_same<tc::range_value_t<Rng>, tc::decay_t<Equal>> accu(tc_move_if_owned(equal));
			tc::for_each(tc_move_if_owned(rng), std::ref(accu));
			return accu.m_result_();
		}
	};

	template<typename RangeReturn, typename Rng, typename Equal>
	struct all_same_element_impl final {
		static constexpr decltype(auto) fn(Rng&& rng, Equal&& equal) MAYTHROW {
			// TODO: For RangeReturn return_value, without _CHECKS, the algorithm should not loop but compute the result in O(1) time.
			return tc::fn_visit(
				[](auto&& t) noexcept -> decltype(auto) {
					return RangeReturn::template pack_element<Rng>(tc_move_if_owned(t));
				},
				[](tc::all_same_result::empty_t) noexcept -> decltype(auto) {
					return RangeReturn::template pack_no_element<Rng>();
				},
				[](tc::all_same_result::not_same_t) noexcept -> decltype(auto) {
					return RangeReturn::template pack_no_element<Rng>();
				}
			)(all_same_element_impl<return_all_same_result, Rng, Equal>::fn(tc_move_if_owned(rng), tc_move_if_owned(equal)) /*MAYTHROW*/);
		}
	};
}
using no_adl::accumulator_all_same;
using no_adl::return_all_same_result;

template<typename RangeReturn, typename Rng, typename Equal = tc::fn_equal_to >
[[nodiscard]] constexpr decltype(auto) all_same_element(Rng&& rng, Equal&& equal = Equal()) MAYTHROW {
	return no_adl::all_same_element_impl<RangeReturn, Rng, Equal>::fn(tc_move_if_owned(rng), tc_move_if_owned(equal));
}

template< typename Rng, typename Equal = tc::fn_equal_to >
[[nodiscard]] constexpr bool all_same(Rng&& rng, Equal&& equal = Equal()) noexcept {
	return tc::fn_visit(
		[](auto&&) noexcept { return true; },
		[](tc::all_same_result::empty_t) noexcept { return true; },
		[](tc::all_same_result::not_same_t) noexcept { return false; }
	)(tc::all_same_element<tc::return_all_same_result>(tc_move_if_owned(rng), tc_move_if_owned(equal)));
}
}
