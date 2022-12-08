
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../algorithm/for_each.h"
#include "../base/modified.h"
#include "range_adaptor.h"

namespace tc {
	/////////////////////////////////////////////////////
	// partial_sum

	namespace no_adl {
		template <typename Rng, typename T, typename AccuOp, bool c_bIncludeInit>
		struct [[nodiscard]] partial_sum_adaptor : private tc::range_adaptor_base_range<Rng> {
		private:
			reference_or_value<T> m_init;
			tc::decay_t<AccuOp> m_accuop;
		public:
			friend auto range_output_t_impl(partial_sum_adaptor const&) -> tc::type::list<tc::decay_t<T> const&>; // declaration only

			template <typename RngRef, typename TRef, typename AccuOpRef>
			constexpr partial_sum_adaptor(RngRef&& rng, TRef&& init, AccuOpRef&& accuop) noexcept
				: tc::range_adaptor_base_range<Rng>(aggregate_tag, std::forward<RngRef>(rng))
				, m_init(aggregate_tag, std::forward<TRef>(init))
				, m_accuop(std::forward<AccuOpRef>(accuop))
			{}

			template <typename Sink>
			constexpr auto operator()(Sink sink) const& MAYTHROW {
				tc::decay_t<T> accu = *m_init;
				return tc_break_or_continue_sequence(
					(CONDITIONAL_PRVALUE_AS_VAL(
						c_bIncludeInit,
						tc::continue_if_not_break(sink, tc::as_const(accu)),
						tc::constant<tc::continue_>()
					))
					(tc::for_each(this->base_range(), [&](auto&& arg) MAYTHROW {
						m_accuop(accu, tc_move_if_owned(arg));
						return tc::continue_if_not_break(sink, tc::as_const(accu));
					}))
				);
			}

			template<ENABLE_SFINAE>
			constexpr auto size() const& noexcept -> tc::decay_t<decltype(tc::size_raw(SFINAE_VALUE(this)->base_range()))> {
				return modified(
					tc::size_raw(this->base_range()),
					if constexpr( c_bIncludeInit ) ++_;
				);
			}
		};
	}
	using no_adl::partial_sum_adaptor;

	template <typename Rng, typename T, typename AccuOp = tc::fn_assign_plus>
	constexpr auto partial_sum_excluding_init(Rng&& rng, T&& init, AccuOp&& accuop = AccuOp()) noexcept {
		return partial_sum_adaptor<Rng, T, AccuOp, /*c_bIncludeInit*/false>(std::forward<Rng>(rng), std::forward<T>(init), std::forward<AccuOp>(accuop));
	}

	template <typename Rng, typename T, typename AccuOp = tc::fn_assign_plus>
	constexpr auto partial_sum_including_init(Rng&& rng, T&& init, AccuOp&& accuop = AccuOp()) noexcept {
		return partial_sum_adaptor<Rng, T, AccuOp, /*c_bIncludeInit*/true>(std::forward<Rng>(rng), std::forward<T>(init), std::forward<AccuOp>(accuop));
	}
}
