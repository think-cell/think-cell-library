//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"

#include "tc_move.h" 
#include "range_adaptor.h"
#include "meta.h"
#include "conditional.h"

namespace tc {

	namespace filter_adaptor_impl {

		template< typename Pred, typename Rng, bool HasIterator=is_range_with_iterators< Rng >::value >
		struct filter_adaptor;

		template<typename Pred, typename Rng>
		using filter_adaptor_base_t = range_adaptor<filter_adaptor<Pred,Rng>, Rng
			, boost::iterators::bidirectional_traversal_tag // filter_adaptor is bidirectional at best
		>;

		template< typename Pred, typename Rng >
		struct filter_adaptor<Pred, Rng, false> : filter_adaptor_base_t<Pred,Rng> {
        private:
			using base_ = filter_adaptor_base_t<Pred,Rng>;

		protected:
			static_assert( tc::is_decayed<Pred>::value );
			Pred m_pred;

		private:
			friend struct range_adaptor_impl::range_adaptor_access;

			template< typename Apply, typename A0>
			auto apply(Apply&& apply, A0&& a0) const& MAYTHROW return_by_val(
				CONDITIONAL(
					m_pred(a0),
					tc::continue_if_not_break(std::forward<Apply>(apply), std::forward<A0>(a0)),
					INTEGRAL_CONSTANT(tc::continue_)()
				)
			)

		public:
			template< typename RngRef, typename PredRef >
			explicit filter_adaptor( RngRef&& rng, PredRef&& pred ) noexcept
				: base_(aggregate_tag(), std::forward<RngRef>(rng))
				, m_pred(std::forward<PredRef>(pred))
			{}
		};

		template< typename Pred, typename Rng >
		struct filter_adaptor<Pred, Rng, true> : filter_adaptor<Pred, Rng, false> {
		private:
			using this_type = filter_adaptor;
			using base_ = filter_adaptor<Pred, Rng, false>;
			static_assert( 
				std::is_same< Rng, view_by_value_t<Rng> >::value,
				"adaptors must hold ranges by value"
			);

		public:
			using typename base_::index;

		private:
			void increment_until_kept(index& idx) const& MAYTHROW {
				// always call operator() const, which is assumed to be thread-safe
				while(!base_::STATIC_VIRTUAL_METHOD_NAME(at_end_index)(idx) && !tc::bool_cast(this->m_pred(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx)))) {
					base_::STATIC_VIRTUAL_METHOD_NAME(increment_index)(idx);
				}
			}

		public:
			template< typename RngRef, typename PredRef >
			explicit filter_adaptor( RngRef&& rng, PredRef&& pred) noexcept
			:	base_( std::forward<RngRef>(rng)
			,	std::forward<PredRef>(pred))
			{}

			STATIC_FINAL(begin_index)() const& MAYTHROW -> index {
				index idx=base_::STATIC_VIRTUAL_METHOD_NAME(begin_index)();
				increment_until_kept(idx);
				return idx;
			}

			STATIC_FINAL(increment_index)(index& idx) const& MAYTHROW -> void {
				base_::STATIC_VIRTUAL_METHOD_NAME(increment_index)(idx);
				increment_until_kept(idx);
			}

			STATIC_FINAL(decrement_index)(index& idx) const& MAYTHROW -> void {
				do {
					base_::STATIC_VIRTUAL_METHOD_NAME(decrement_index)(idx);
					// always call operator() const, which is assumed to be thread-safe
				} while(!tc::bool_cast(this->m_pred(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx))));
			}

			STATIC_FINAL(middle_point)( index & idx, index const& idxEnd ) const& MAYTHROW -> void {
				index const idxBegin = idx;
				base_::STATIC_VIRTUAL_METHOD_NAME(middle_point)(idx,idxEnd);
			
				// always call operator() const, which is assumed to be thread-safe
				while(!base_::STATIC_VIRTUAL_METHOD_NAME(equal_index)(idxBegin, idx) && !tc::bool_cast(this->m_pred(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx)))) {
					base_::STATIC_VIRTUAL_METHOD_NAME(decrement_index)(idx);
				}
			}

			auto element_base_index(index const& idx) const& noexcept {
				return idx;
			}
		};
	}
	using filter_adaptor_impl::filter_adaptor;

	namespace range_reference_adl_barrier {
		template< typename Pred, typename Rng, bool bConst >
		struct range_reference_filter_adaptor_base {
			using type=tc::range_reference_t<
				tc::apply_if_t<
					bConst,
					std::add_const,
					filter_adaptor_impl::filter_adaptor_base_t<Pred, Rng>
				>
			>;
		};

		template< typename Pred, typename Rng >
		struct range_reference<filter_adaptor<Pred, Rng, false> > : range_reference_filter_adaptor_base<Pred, Rng, false> {};

		template< typename Pred, typename Rng >
		struct range_reference<filter_adaptor<Pred, Rng, false> const> : range_reference_filter_adaptor_base<Pred, Rng, true> {};
	}

	template<typename Rng, typename Pred>
	auto filter(Rng&& rng, Pred&& pred) noexcept
		return_ctor( filter_adaptor<tc::decay_t<Pred> BOOST_PP_COMMA() view_by_value_t<Rng> >, (std::forward<Rng>(rng),std::forward<Pred>(pred)) )
}
