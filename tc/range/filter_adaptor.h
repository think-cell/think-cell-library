
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
		template<typename Pred, typename Sink>
		struct filter_sink {
			static_assert(tc::decayed<Sink>);
			using guaranteed_break_or_continue = std::conditional_t<
				std::is_same<tc::constant<tc::continue_>, guaranteed_break_or_continue_t<Sink>>::value,
				tc::constant<tc::continue_>,
				tc::break_or_continue
			>;
			Pred const& m_pred;
			Sink m_sink;

			template<typename T>
			constexpr auto operator()(T&& t) const& return_decltype_MAYTHROW(
				tc::explicit_cast<bool>(tc::invoke(m_pred, tc::as_const(t)))
					? tc::continue_if_not_break(m_sink, std::forward<T>(t))
					: tc::constant<tc::continue_>()
			)
		};

		template< typename Pred, typename Rng >
		struct [[nodiscard]] filter_adaptor<Pred, Rng, false> : tc::generator_range_adaptor<Rng>, tc::range_output_from_base_range {
		protected:
			static_assert(tc::decayed<Pred>);
			Pred m_pred;

		public:
			constexpr filter_adaptor() = default;
			template< typename RngRef, typename PredRef >
			constexpr filter_adaptor(RngRef&& rng, PredRef&& pred) noexcept
				: filter_adaptor::generator_range_adaptor(aggregate_tag, std::forward<RngRef>(rng))
				, m_pred(std::forward<PredRef>(pred))
			{}

			template<typename Sink>
			constexpr auto adapted_sink(Sink&& sink, bool /*bReverse*/) const& noexcept {
				return filter_sink<Pred, tc::decay_t<Sink>>{m_pred, std::forward<Sink>(sink)};
			}
		};

		template< typename Pred, typename Rng >
		struct [[nodiscard]] filter_adaptor<Pred, Rng, true>
			: tc::index_range_adaptor<
				filter_adaptor<Pred, Rng, true>,
				Rng,
				filter_adaptor<Pred, Rng, false>,
				boost::iterators::bidirectional_traversal_tag // filter_adaptor is bidirectional at best
			>
		{
		private:
			using this_type = filter_adaptor;
			using base_ = typename filter_adaptor::index_range_adaptor;

		public:
			using typename base_::tc_index;

			constexpr filter_adaptor() = default;
			using base_::base_;

		private:
			void increment_until_kept(tc_index& idx) const& MAYTHROW {
				// always call operator() const, which is assumed to be thread-safe
				while(!this->template at_end_index<base_>(idx) && !tc::explicit_cast<bool>(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx))))) {
					this->template increment_index<base_>(idx);
				}
			}

			STATIC_FINAL(begin_index)() const& MAYTHROW -> tc_index {
				tc_index idx=this->template begin_index<base_>();
				increment_until_kept(idx);
				return idx;
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& MAYTHROW -> void {
				this->template increment_index<base_>(idx);
				increment_until_kept(idx);
			}

			STATIC_FINAL(decrement_index)(tc_index& idx) const& MAYTHROW -> void {
				do {
					this->template decrement_index<base_>(idx);
					// always call operator() const, which is assumed to be thread-safe
				} while(!tc::explicit_cast<bool>(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx)))));
			}

			STATIC_FINAL(middle_point)( tc_index & idx, tc_index const& idxEnd ) const& MAYTHROW -> void {
				tc_index const idxBegin = idx;
				this->template middle_point<base_>(idx,idxEnd);
			
				// always call operator() const, which is assumed to be thread-safe
				while(idxBegin != idx && !tc::explicit_cast<bool>(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx))))) {
					this->template decrement_index<base_>(idx);
				}
			}
		public:
			static constexpr decltype(auto) element_base_index(tc_index const& idx) noexcept {
				return idx;
			}
			static constexpr decltype(auto) element_base_index(tc_index&& idx) noexcept {
				return tc_move(idx);
			}

			constexpr decltype(auto) dereference_untransform(tc_index const& idx) const& noexcept {
				return this->base_range().dereference_untransform(idx);
			}
		};
	}

	template<typename Rng, typename Pred = tc::identity>
	constexpr auto filter(Rng&& rng, Pred&& pred = Pred())
		return_ctor_noexcept( TC_FWD(filter_adaptor<tc::decay_t<Pred>, Rng>), (std::forward<Rng>(rng),std::forward<Pred>(pred)) )

	namespace no_adl {
		template<typename Pred, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::filter_adaptor<Pred, Rng, true>>: tc::is_index_valid_for_move_constructed_range<Rng> {};
	}
}
