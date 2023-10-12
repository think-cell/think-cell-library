
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
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
				: tc::range_adaptor_base_range<Rng>(aggregate_tag, tc_move_if_owned(rng))
				, m_init(aggregate_tag, tc_move_if_owned(init))
				, m_accuop(tc_move_if_owned(accuop))
			{}

			template <typename Sink>
			constexpr auto operator()(Sink sink) const& MAYTHROW {
				tc::decay_t<T> accu = *m_init;
				return tc_break_or_continue_sequence(
					(tc_conditional_prvalue_as_val(
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

			constexpr auto size() const& MAYTHROW requires tc::has_size<Rng> {
				return tc::compute_range_adaptor_size<[](auto n) noexcept {
					if constexpr (c_bIncludeInit) ++n;
					return n;
				}>(this->base_range());
			}
		};
	}
	using no_adl::partial_sum_adaptor;

	template <typename Rng, typename T, typename AccuOp = tc::fn_assign_plus>
	constexpr auto partial_sum_excluding_init(Rng&& rng, T&& init, AccuOp&& accuop = AccuOp()) noexcept {
		return partial_sum_adaptor<Rng, T, AccuOp, /*c_bIncludeInit*/false>(tc_move_if_owned(rng), tc_move_if_owned(init), tc_move_if_owned(accuop));
	}

	template <typename Rng, typename T, typename AccuOp = tc::fn_assign_plus>
	constexpr auto partial_sum_including_init(Rng&& rng, T&& init, AccuOp&& accuop = AccuOp()) noexcept {
		return partial_sum_adaptor<Rng, T, AccuOp, /*c_bIncludeInit*/true>(tc_move_if_owned(rng), tc_move_if_owned(init), tc_move_if_owned(accuop));
	}
}
