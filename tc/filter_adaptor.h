
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"

#include "tc_move.h" 
#include "range_adaptor.h"
#include "meta.h"
#include "conditional.h"
#include "range_fwd.h"
#include "invoke.h"

namespace tc {
	namespace no_adl {
		template<typename Pred, typename Sink>
		struct filter_sink : tc::sink_value_type_base<Sink> {
			static_assert(tc::is_decayed<Sink>::value);
			using guaranteed_break_or_continue = std::conditional_t<
				std::is_same<INTEGRAL_CONSTANT(tc::continue_), guaranteed_break_or_continue_t<Sink>>::value,
				INTEGRAL_CONSTANT(tc::continue_),
				tc::break_or_continue
			>;
			Pred const& m_pred;
			Sink m_sink;

			template<typename T>
			constexpr auto operator()(T&& t) const& return_decltype_MAYTHROW(
				tc::bool_cast(tc::invoke(m_pred, tc::as_const(t)))
					? tc::continue_if_not_break(m_sink, std::forward<T>(t))
					: INTEGRAL_CONSTANT(tc::continue_)()
			)
		};

		template< typename Pred, typename Rng >
		struct [[nodiscard]] filter_adaptor<Pred, Rng, false> : tc::generator_range_adaptor<Rng> {
		protected:
			static_assert( tc::is_decayed<Pred>::value );
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
				return filter_sink<Pred, tc::decay_t<Sink>>{{}, m_pred, std::forward<Sink>(sink)};
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
			using typename base_::index;

			constexpr filter_adaptor() = default;
			using base_::base_;

		private:
			void increment_until_kept(index& idx) const& MAYTHROW {
				// always call operator() const, which is assumed to be thread-safe
				while(!this->template at_end_index<base_>(idx) && !tc::bool_cast(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx))))) {
					this->template increment_index<base_>(idx);
				}
			}

			STATIC_FINAL(begin_index)() const& MAYTHROW -> index {
				index idx=this->template begin_index<base_>();
				increment_until_kept(idx);
				return idx;
			}

			STATIC_FINAL(increment_index)(index& idx) const& MAYTHROW -> void {
				this->template increment_index<base_>(idx);
				increment_until_kept(idx);
			}

			STATIC_FINAL(decrement_index)(index& idx) const& MAYTHROW -> void {
				do {
					this->template decrement_index<base_>(idx);
					// always call operator() const, which is assumed to be thread-safe
				} while(!tc::bool_cast(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx)))));
			}

			STATIC_FINAL(middle_point)( index & idx, index const& idxEnd ) const& MAYTHROW -> void {
				index const idxBegin = idx;
				this->template middle_point<base_>(idx,idxEnd);
			
				// always call operator() const, which is assumed to be thread-safe
				while(idxBegin != idx && !tc::bool_cast(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx))))) {
					this->template decrement_index<base_>(idx);
				}
			}
		public:
			auto element_base_index(index const& idx) const& noexcept {
				return idx;
			}
		};

		template<typename FilterAdaptor, typename Pred, typename Rng>
		struct range_value<FilterAdaptor, filter_adaptor<Pred, Rng, false>, tc::void_t<tc::range_value_t<Rng>>> final {
			using type = tc::range_value_t<Rng>;
		};
	}

	template<typename Rng, typename Pred = tc::identity>
	constexpr auto filter(Rng&& rng, Pred&& pred = Pred())
		return_ctor_noexcept( filter_adaptor<tc::decay_t<Pred> BOOST_PP_COMMA() Rng >, (std::forward<Rng>(rng),std::forward<Pred>(pred)) )

	namespace no_adl {
		template<typename Pred, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::filter_adaptor<Pred, Rng, true>, std::enable_if_t<std::is_lvalue_reference<Rng>::value>>: std::true_type {};
		
		template<typename Pred, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::filter_adaptor<Pred, Rng, true>, std::enable_if_t<!std::is_reference<Rng>::value>>: tc::is_index_valid_for_move_constructed_range<Rng> {};
	}
}
