
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"

#include "tc_move.h" 
#include "range_adaptor.h"
#include "meta.h"
#include "conditional.h"
#include "range_fwd.h"
#include "invoke.h"

namespace tc {
	namespace no_adl {
		template<typename Pred, typename Rng>
		using filter_adaptor_base_t = range_adaptor<filter_adaptor<Pred,Rng>, Rng
			, boost::iterators::bidirectional_traversal_tag // filter_adaptor is bidirectional at best
		>;

		template< typename Pred, typename Rng >
		struct [[nodiscard]] filter_adaptor<Pred, Rng, false> : filter_adaptor_base_t<Pred,Rng> {
        private:
			using base_ = filter_adaptor_base_t<Pred,Rng>;

		protected:
			static_assert( tc::is_decayed<Pred>::value );
			Pred m_pred;

		private:
			template<typename, typename>
			friend struct no_adl::range_adaptor_access;

			template< typename Apply, typename A0>
			auto apply(Apply&& apply, A0&& a0) const& MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(std::declval<Apply>(), std::declval<A0>())), INTEGRAL_CONSTANT(tc::continue_)> {
				if(tc::invoke(m_pred, tc::as_const(a0))) {
					return tc::continue_if_not_break(std::forward<Apply>(apply), std::forward<A0>(a0));
				} else {
					return INTEGRAL_CONSTANT(tc::continue_)();
				}
			}

		public:
			template< typename RngRef, typename PredRef >
			explicit filter_adaptor( RngRef&& rng, PredRef&& pred ) noexcept
				: base_(aggregate_tag, std::forward<RngRef>(rng))
				, m_pred(std::forward<PredRef>(pred))
			{}
		};

		template< typename Pred, typename Rng >
		struct [[nodiscard]] filter_adaptor<Pred, Rng, true> : filter_adaptor<Pred, Rng, false> {
		private:
			using this_type = filter_adaptor;
			using base_ = filter_adaptor<Pred, Rng, false>;

		public:
			using typename base_::index;

		private:
			void increment_until_kept(index& idx) const& MAYTHROW {
				// always call operator() const, which is assumed to be thread-safe
				while(!this->template at_end_index<base_>(idx) && !tc::bool_cast(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx))))) {
					this->template increment_index<base_>(idx);
				}
			}

		public:
			template< typename RngRef, typename PredRef >
			explicit filter_adaptor( RngRef&& rng, PredRef&& pred) noexcept
			:	base_( std::forward<RngRef>(rng)
			,	std::forward<PredRef>(pred))
			{}
		private:
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
				while(!this->template equal_index<base_>(idxBegin, idx) && !tc::bool_cast(tc::invoke(this->m_pred, tc::as_const(this->template dereference_index<base_>(idx))))) {
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

	template<typename Rng, typename Pred>
	auto filter(Rng&& rng, Pred&& pred)
		return_ctor_noexcept( filter_adaptor<tc::decay_t<Pred> BOOST_PP_COMMA() Rng >, (std::forward<Rng>(rng),std::forward<Pred>(pred)) )

	namespace no_adl {
		template<typename Pred, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::filter_adaptor<Pred, Rng, true>, std::enable_if_t<std::is_lvalue_reference<Rng>::value>>: std::true_type {};
		
		template<typename Pred, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::filter_adaptor<Pred, Rng, true>, std::enable_if_t<!std::is_reference<Rng>::value>>: tc::is_index_valid_for_move_constructed_range<Rng> {};
	}
}
