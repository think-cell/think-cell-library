//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "size.h"
#include "modified.h"

namespace tc {
	namespace adl_barrier {
		template<typename Rng>
		struct reverse_adaptor : tc::range_iterator_from_index<
			reverse_adaptor<Rng>,
			boost::optional<
				typename std::remove_reference_t<
					index_range_t<Rng>
				>::index
			>,
			traversal_t<Rng>
		>
		{
			reference_or_value< index_range_t<Rng> > m_baserng;
		private:
			using this_type = reverse_adaptor;
		public:
			using index = typename reverse_adaptor::index;

			template<typename RngRef>
			explicit reverse_adaptor(aggregate_tag, RngRef&& rng) :
				m_baserng(reference_or_value< index_range_t<Rng> >(aggregate_tag(), std::forward<RngRef>(rng)))
			{}

			template< typename Func >
			tc::break_or_continue operator()(Func func) /* no & */ MAYTHROW {
				auto const itBegin=boost::begin(boost::implicit_cast<std::remove_reference_t<Rng>&>(*m_baserng));
				auto itEnd=boost::end(boost::implicit_cast<std::remove_reference_t<Rng>&>(*m_baserng));
				while( itEnd!=itBegin ) {
					--itEnd;
					if( break_==continue_if_not_break(func, *itEnd) ) return break_;
				}
				return continue_;
			}

			template< typename Func >
			tc::break_or_continue operator()(Func func) const/* no & */ MAYTHROW {
				auto const itBegin=boost::begin(boost::implicit_cast<std::remove_reference_t<Rng> const&>((*m_baserng)));
				auto itEnd=boost::end(boost::implicit_cast<std::remove_reference_t<Rng> const&>(*m_baserng));
				while( itEnd!=itBegin ) {
					--itEnd;
					if( break_==continue_if_not_break(func, *itEnd) ) return break_;
				}
				return continue_;
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				auto idx = m_baserng->end_index();
				return m_baserng->equal_index(m_baserng->begin_index(),idx) ? boost::none : ( m_baserng->decrement_index(idx), boost::make_optional(idx) );
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return boost::none;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return !tc::bool_cast(idx);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				if (m_baserng->equal_index(m_baserng->begin_index(), *idx)) {
					idx = boost::none;
				} else {
					m_baserng->decrement_index(*idx);
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				if (idx) {
					m_baserng->increment_index(*idx);
				} else {
					idx = m_baserng->begin_index();
				}
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept return_decltype(
				m_baserng->dereference_index(*idx)
			)

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept return_decltype(
				m_baserng->dereference_index(*idx)
			)

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::bool_cast(idxLhs) == tc::bool_cast(idxRhs) && (!idxLhs || m_baserng->equal_index(*idxLhs, *idxRhs));
			}

			using difference_type = range_difference_type<Rng,traversal_t<Rng>>;

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type {
				return m_baserng->distance_to_index(idxRhs ? *idxRhs : m_baserng->begin_index(), idxLhs ? *idxLhs : m_baserng->begin_index()) +  (idxRhs ? 0 : 1) + (idxLhs ? 0 : -1);
			}

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const& noexcept -> void {
				if (idx) {
					m_baserng->advance_index(*idx, -(d-1));
					if (m_baserng->equal_index(m_baserng->begin_index(), *idx)) {
						idx = boost::none;
					} else {
						m_baserng->decrement_index(*idx);
					}
				} else {
					if (0 != d) {
						_ASSERT(d < 0);
						idx = m_baserng->begin_index();
						m_baserng->advance_index(*idx, -(d+1));
					}
				}
			}

			auto bound_base_index(index const& idx) const& noexcept {
				return idx ? modified(*idx, m_baserng->increment_index(_)) : m_baserng->begin_index();
			}

			auto element_base_index(index const& idx) const& noexcept {
				_ASSERT(!this->at_end_index(idx));
				return *idx;
			}

			auto base_range() & noexcept {
				return *m_baserng;
			}

			auto base_range() const & noexcept {
				return *m_baserng;
			}

			STATIC_FINAL(middle_point)(index & idx, index const& idxEnd ) const& noexcept -> void {
				auto idxBeginBase = bound_base_index(idxEnd);
				m_baserng->middle_point(idxBeginBase, bound_base_index(idx));
				idx = idxBeginBase;
			}

			template <typename It>
			friend void take_inplace_impl(reverse_adaptor& rng, It&& it) noexcept {
				drop_inplace(*rng.m_baserng, it.bound_base());
			}

			template <typename It>
			friend void drop_inplace_impl(reverse_adaptor& rng, It&& it) noexcept {
				take_inplace(*rng.m_baserng, it.bound_base());
			}
		};
	}
	using adl_barrier::reverse_adaptor;

	template<typename Rng>
	auto reverse(Rng&& rng) noexcept return_ctor(
		reverse_adaptor< view_by_value_t<Rng> >,
		(aggregate_tag(), std::forward<Rng>(rng))
	)


}






