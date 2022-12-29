
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
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
				return tc::continue_if_not_break(*m_paccuop, *m_pt, std::forward<S>(s));
			}
		};
	}

	template< typename Rng, typename T, typename AccuOp >
	[[nodiscard]] constexpr T accumulate(Rng&& rng, T t, AccuOp accuop) MAYTHROW {
		tc::for_each(std::forward<Rng>(rng), no_adl::accumulate_fn<T,AccuOp>(t,accuop));
		return t;
	}

	namespace no_adl {
		template< typename T, typename AccuOp >
		struct [[nodiscard]] accumulator_with_front /*final*/ {
			std::optional<T> & m_t;
			AccuOp m_accuop;

			template< typename S >
			constexpr auto operator()( S&& s ) const& MAYTHROW {
				return CONDITIONAL_PRVALUE_AS_VAL(
					m_t,
					tc::continue_if_not_break( m_accuop, *m_t, std::forward<S>(s) ),
					TC_FWD(tc::optional_emplace(m_t, std::forward<S>(s)), tc::constant<tc::continue_>())
				);
			}
		};
	}

	template<typename Value, typename AccuOp>
	constexpr no_adl::accumulator_with_front<Value, AccuOp> make_accumulator_with_front(std::optional<Value>& value, AccuOp&& accumulate) noexcept {
		return {value, std::forward<AccuOp>(accumulate)};
	}

	template<typename T, typename Rng, typename AccuOp>
	[[nodiscard]] constexpr std::optional<T> accumulate_with_front(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		static_assert(tc::decayed<T>);
		std::optional<T> t;
		tc::for_each(std::forward<Rng>(rng), tc::make_accumulator_with_front(t, std::forward<AccuOp>(accuop)));
		return t;
	}

	template<typename Rng, typename AccuOp>
	[[nodiscard]] constexpr auto accumulate_with_front(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		return tc::accumulate_with_front<tc::range_value_t<Rng>>(std::forward<Rng>(rng), std::forward<AccuOp>(accuop));
	}
}
