
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

			template<typename T> requires tc::runtime_predicate<Pred, T> && tc::sinkable<Sink, T>
			constexpr auto operator()(T&& t) const& noexcept(tc::nothrow_predicate<Pred, T> && tc::nothrow_sinkable<Sink, T>) {
				return tc::explicit_cast<bool>(tc::invoke(m_pred, tc::as_const(t)))
							? tc::continue_if_not_break(m_sink, tc_move_if_owned(t))
							: tc::constant<tc::continue_>();
			}

			template<typename T> requires tc::constant_predicate_true<Pred, T>  && tc::sinkable<Sink, T>
			constexpr auto operator()(T&& t) const& noexcept(tc::nothrow_predicate<Pred, T> && tc::nothrow_sinkable<Sink, T>) {
				tc::discard(tc::invoke(m_pred, tc::as_const(t)));
				return tc::continue_if_not_break(m_sink, tc_move_if_owned(t));
			}
			template<typename T> requires tc::constant_predicate_false<Pred, T>
			constexpr auto operator()(T&& t) const& noexcept(tc::nothrow_predicate<Pred, T>) {
				tc::discard(tc::invoke(m_pred, tc::as_const(t)));
				return tc::constant<tc::continue_>();
			}
		};

		template< typename Pred, typename Rng, bool HasIterator=tc::range_with_iterators< Rng > >
		struct filter_adaptor;

		template< typename Pred, typename Rng >
		struct [[nodiscard]] filter_adaptor<Pred, Rng, false> : tc::generator_range_adaptor<Rng>, tc::range_output_from_base_range {
		protected:
			static_assert(tc::decayed<Pred>);
			Pred m_pred;

		public:
			constexpr filter_adaptor() = default;
			template< typename RngRef, typename PredRef >
			constexpr filter_adaptor(RngRef&& rng, PredRef&& pred) noexcept
				: filter_adaptor::generator_range_adaptor(aggregate_tag, tc_move_if_owned(rng))
				, m_pred(tc_move_if_owned(pred))
			{}

			template<typename Sink>
			constexpr auto adapted_sink(Sink&& sink, bool /*bReverse*/) const& noexcept {
				return filter_sink<Pred, tc::decay_t<Sink>>{m_pred, tc_move_if_owned(sink)};
			}
		};

		template< typename Pred, typename Rng >
		struct [[nodiscard]] filter_adaptor<Pred, Rng, true>
			: tc::index_range_adaptor<
				filter_adaptor<Pred, Rng, true>,
				Rng, tc::index_range_adaptor_flags::inherit_end | tc::index_range_adaptor_flags::inherit_dereference,
				filter_adaptor<Pred, Rng, false>
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
				while(!this->at_end_index(idx) && !tc::explicit_cast<bool>(tc::invoke(this->m_pred, tc::as_const(this->dereference_index(idx))))) {
					tc::increment_index(this->base_range(), idx);
				}
			}

			STATIC_FINAL(begin_index)() const& MAYTHROW -> tc_index {
				tc_index idx=this->base_begin_index();
				increment_until_kept(idx);
				return idx;
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& MAYTHROW -> void {
				tc::increment_index(this->base_range(), idx);
				increment_until_kept(idx);
			}

			STATIC_FINAL(decrement_index)(tc_index& idx) const& MAYTHROW -> void
				requires tc::has_decrement_index<Rng>
			{
				do {
					tc::decrement_index(this->base_range(), idx);
					// always call operator() const, which is assumed to be thread-safe
				} while(!tc::explicit_cast<bool>(tc::invoke(this->m_pred, tc::as_const(this->dereference_index(idx)))));
			}

			STATIC_FINAL(middle_point)( tc_index & idx, tc_index const& idxEnd ) const& MAYTHROW -> void
				requires tc::has_middle_point<Rng> && tc::has_decrement_index<Rng>
			{
				tc_index const idxBegin = idx;
				tc::middle_point(this->base_range(), idx, idxEnd);
			
				// always call operator() const, which is assumed to be thread-safe
				while(idxBegin != idx && !tc::explicit_cast<bool>(tc::invoke(this->m_pred, tc::as_const(this->dereference_index(idx))))) {
					tc::decrement_index(this->base_range(), idx);
				}
			}
		public:
			static constexpr decltype(auto) element_base_index(tc_index const& idx) noexcept {
				return idx;
			}
			static constexpr decltype(auto) element_base_index(tc_index&& idx) noexcept {
				return tc_move(idx);
			}
		};
	}
	using no_adl::filter_adaptor;

	template<typename Pred, typename Rng>
	constexpr auto enable_stable_index_on_move<tc::filter_adaptor<Pred, Rng, true>> = tc::stable_index_on_move<Rng>;

	template<typename Rng, typename Pred = tc::identity>
	constexpr auto filter(Rng&& rng, Pred&& pred = Pred())
		return_ctor_noexcept( TC_FWD(tc::filter_adaptor<tc::decay_t<Pred>, Rng>), (tc_move_if_owned(rng),tc_move_if_owned(pred)) )
}

