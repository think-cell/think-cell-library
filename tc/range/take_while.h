
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/tc_move.h"
#include "../base/conditional.h"
#include "../base/invoke.h"

#include "range_adaptor.h"
#include "meta.h"
#include "range_fwd.h"

namespace tc {
	namespace no_adl {
		template< typename Pred, typename Rng >
		struct [[nodiscard]] take_while_adaptor<Pred, Rng, false> : tc::range_adaptor_base_range<Rng>, tc::range_output_from_base_range {
		protected:
			static_assert(tc::decayed<Pred>);
			Pred m_pred;

		public:
			constexpr take_while_adaptor() = default;
			template< typename RngRef, typename PredRef >
			constexpr take_while_adaptor(RngRef&& rng, PredRef&& pred) noexcept
				: take_while_adaptor::range_adaptor_base_range(aggregate_tag, std::forward<RngRef>(rng))
				, m_pred(std::forward<PredRef>(pred))
			{}

			template<tc::decayed_derived_from<take_while_adaptor> Self, typename Sink> 
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				tc::common_type_t<decltype(tc::for_each(std::forward<Self>(self).base_range(), sink)),tc::constant<tc::continue_>> breakorcontinue = tc::constant<tc::continue_>();
				tc::for_each(
					std::forward<Self>(self).base_range(),
					[&](auto&& t) MAYTHROW -> tc::break_or_continue {
						if (tc::invoke(self.m_pred, tc::as_const(t))) {
							breakorcontinue = tc::continue_if_not_break(sink, tc_move_if_owned(t));
							return breakorcontinue;
						} else {
							return tc::break_;
						}
					}
				);
				return breakorcontinue;
			}
		};

		template< typename Pred, typename Rng >
		struct [[nodiscard]] take_while_adaptor<Pred, Rng, true>
			: tc::index_range_adaptor<
				take_while_adaptor<Pred, Rng, true>,
				Rng,
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
				this->template at_end_index<base_>(idx) || !tc::invoke(this->m_pred, tc::as_const(this->dereference_index(idx)))
			)

		public:
			constexpr static auto element_base_index(tc_index const& idx) noexcept {
				return idx;
			}
		};
	}

	template<typename Rng, typename Pred = tc::identity>
	constexpr auto take_while(Rng&& rng, Pred&& pred = Pred())
		return_ctor_noexcept( TC_FWD( take_while_adaptor<tc::decay_t<Pred>, Rng>), (std::forward<Rng>(rng),std::forward<Pred>(pred)) )

	namespace no_adl {
		template<typename Pred, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::take_while_adaptor<Pred, Rng, true>>: tc::is_index_valid_for_move_constructed_range<Rng> {};
	}
}
