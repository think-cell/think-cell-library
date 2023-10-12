
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/move.h"
#include "../base/conditional.h"
#include "../base/invoke.h"
#include "../base/trivial_functors.h"

#include "range_adaptor.h"
#include "meta.h"

namespace tc {
	namespace no_adl {
		template< typename Pred, typename Rng, bool HasIterator=tc::range_with_iterators< Rng > >
		struct take_while_adaptor;

		template< typename Pred, typename Rng >
		struct [[nodiscard]] take_while_adaptor<Pred, Rng, false> : tc::range_adaptor_base_range<Rng>, tc::range_output_from_base_range {
		protected:
			static_assert(tc::decayed<Pred>);
			Pred m_pred;

		public:
			constexpr take_while_adaptor() = default;
			template< typename RngRef, typename PredRef >
			constexpr take_while_adaptor(RngRef&& rng, PredRef&& pred) noexcept
				: take_while_adaptor::range_adaptor_base_range(aggregate_tag, tc_move_if_owned(rng))
				, m_pred(tc_move_if_owned(pred))
			{}

			template<tc::decayed_derived_from<take_while_adaptor> Self, typename Sink> 
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				tc::common_type_t<decltype(tc::for_each(tc_move_if_owned(self).base_range(), sink)),tc::constant<tc::continue_>> boc = tc::constant<tc::continue_>();
				tc::for_each(
					tc_move_if_owned(self).base_range(),
					[&](auto&& t) MAYTHROW -> tc::break_or_continue {
						if (tc::invoke(self.m_pred, tc::as_const(t))) {
							boc = tc::continue_if_not_break(sink, tc_move_if_owned(t));
							return boc;
						} else {
							return tc::break_;
						}
					}
				);
				return boc;
			}
		};

		template< typename Pred, typename Rng >
		struct [[nodiscard]] take_while_adaptor<Pred, Rng, true>
			: tc::index_range_adaptor<
				take_while_adaptor<Pred, Rng, true>,
				Rng, tc::index_range_adaptor_flags::inherit_begin | tc::index_range_adaptor_flags::inherit_behavior,
				take_while_adaptor<Pred, Rng, false>
			>
		{
		private:
			using this_type = take_while_adaptor;
			using base_ = typename take_while_adaptor::index_range_adaptor;

		public:
			using typename base_::tc_index;

			constexpr take_while_adaptor() = default;
			using base_::base_;

		private:
			STATIC_FINAL_MOD(constexpr, end_index)() const& = delete;  // let range_iterator_from_index::end return end_sentinel

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& return_MAYTHROW(
				tc::at_end_index(this->base_range(), idx) || !tc::invoke(this->m_pred, tc::as_const(this->dereference_index(idx)))
			)

		public:
			constexpr static auto element_base_index(tc_index const& idx) noexcept {
				return idx;
			}
		};
	}
	using no_adl::take_while_adaptor;

	template<typename Pred, typename Rng>
	constexpr auto enable_stable_index_on_move<tc::take_while_adaptor<Pred, Rng, true>> = tc::stable_index_on_move<Rng>;

	template<typename Rng, typename Pred = tc::identity>
	constexpr auto take_while(Rng&& rng, Pred&& pred = Pred())
		return_ctor_noexcept( TC_FWD( tc::take_while_adaptor<tc::decay_t<Pred>, Rng>), (tc_move_if_owned(rng),tc_move_if_owned(pred)) )
}
