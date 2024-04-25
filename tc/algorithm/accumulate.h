
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "for_each.h"
#include "../base/modified.h"
#include "../optional.h"

namespace tc {
	/////////////////////////////////////////////////////
	// accumulate

	namespace no_adl {
		template< typename T, typename AccuOp >
		struct accumulate_fn /*final*/ {
			T * m_pt;
			AccuOp * m_paccuop;
			constexpr accumulate_fn( T & t, AccuOp & accuop ) noexcept
			:  m_pt(std::addressof(t)), m_paccuop(std::addressof(accuop))
			{}

			template<typename S>
			constexpr auto operator()(S&& s) const& MAYTHROW {
				return tc::continue_if_not_break(*m_paccuop, *m_pt, tc_move_if_owned(s));
			}
		};
	}

	template< typename Rng, typename T, typename AccuOp >
	[[nodiscard]] constexpr T accumulate(Rng&& rng, T t, AccuOp accuop) MAYTHROW {
		tc::for_each(tc_move_if_owned(rng), no_adl::accumulate_fn<T,AccuOp>(t,accuop));
		return t;
	}

	namespace no_adl {
		template< typename T, typename AccuOp >
		struct [[nodiscard]] accumulator_with_front /*final*/ {
			std::optional<T> & m_t;
			AccuOp m_accuop;

			template< typename S >
			constexpr auto operator()( S&& s ) const& MAYTHROW {
				return tc_conditional_prvalue_as_val(
					m_t,
					tc::continue_if_not_break( m_accuop, *m_t, tc_move_if_owned(s) ),
					TC_FWD(tc::optional_emplace(m_t, tc_move_if_owned(s)), tc::constant<tc::continue_>())
				);
			}
		};
	}

	template<typename Value, typename AccuOp>
	constexpr no_adl::accumulator_with_front<Value, AccuOp> make_accumulator_with_front(std::optional<Value>& value, AccuOp&& accumulate) noexcept {
		return {value, tc_move_if_owned(accumulate)};
	}

	template<typename T, typename Rng, typename AccuOp>
	[[nodiscard]] constexpr std::optional<T> accumulate_with_front(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		static_assert(tc::decayed<T>);
		std::optional<T> t;
		tc::for_each(tc_move_if_owned(rng), tc::make_accumulator_with_front(t, tc_move_if_owned(accuop)));
		return t;
	}

	template<typename Rng, typename AccuOp>
	[[nodiscard]] constexpr auto accumulate_with_front(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		return tc::accumulate_with_front<tc::range_value_t<Rng>>(tc_move_if_owned(rng), tc_move_if_owned(accuop));
	}
}
