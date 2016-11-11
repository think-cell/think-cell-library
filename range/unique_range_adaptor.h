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
#include "static_polymorphism.h"
#include "sub_range.h"

namespace tc {

	namespace unique_range_adaptor_adl_barrier {

		template<
			typename Rng,
			typename Equals
		>
		struct unique_adaptor
			: range_iterator_generator_from_index<
				unique_adaptor<Rng, Equals>,
				typename std::remove_reference<
					index_range_t<Rng>
				>::type::index,
				typename boost::range_detail::demote_iterator_traversal_tag<
					boost::iterators::bidirectional_traversal_tag,
					traversal_t<Rng>
				>::type
			>
		{
			using index = typename unique_adaptor::index;

			template<
				typename RngRef,
				typename EqualsRef
			>
			explicit unique_adaptor(RngRef&& rng, EqualsRef&& equals) noexcept
				: m_baserng(reference_or_value< index_range_t<Rng> >(aggregate_tag(), std::forward<RngRef>(rng)))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

		private:
			using this_type = unique_adaptor;
			reference_or_value< index_range_t<Rng> > m_baserng;
			Equals m_equals;

		public:

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return m_baserng->begin_index();
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return m_baserng->end_index();
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return m_baserng->at_end_index(idx);
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept return_decltype(
				m_baserng->dereference_index(idx)
			)

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept return_decltype(
				m_baserng->dereference_index(idx)
			)

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return m_baserng->equal_index(idxLhs, idxRhs);
			}

			STATIC_FINAL(increment_index)(index& idx0) const& noexcept -> void {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERT(!this->at_end_index(idx0));

				auto idx1=idx0;
				m_baserng->increment_index(idx0);
				if (!m_baserng->at_end_index(idx0)) {
					RefType ref1(aggregate_tag(), this->dereference_index(idx1));

					for (;;) {
						RefType ref0(aggregate_tag(), this->dereference_index(idx0));
						if (!this->m_equals(*ref1, *ref0)) {
							break;
						}

						idx1 = idx0;
						m_baserng->increment_index(idx0);
						if (m_baserng->at_end_index(idx0)) {
							break;
						}
						ref1 = RefType(aggregate_tag(), this->dereference_index(idx0));
						if (!this->m_equals(*ref0, *ref1)) {
							break;
						}

						idx1 = idx0;
						m_baserng->increment_index(idx0);
						if (m_baserng->at_end_index(idx0)) {
							break;
						}
					}
				}
			}

			STATIC_FINAL(decrement_index)(index& idx0) const& noexcept -> void {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERT(!this->equal_index(this->begin_index(),idx0));

				m_baserng->decrement_index(idx0);
				if (!this->equal_index(this->begin_index(),idx0)) {
					RefType ref0(aggregate_tag(), this->dereference_index(idx0));

					for (;;) {
						auto idx1=idx0;
						m_baserng->decrement_index(idx1);
						RefType ref1(aggregate_tag(), this->dereference_index(idx1));
						if (!this->m_equals(*ref1, *ref0)) {
							break;
						}
						if (this->equal_index(this->begin_index(),idx1)) {
							idx0 = tc_move(idx1);
							break;
						}

						idx0 = idx1;
						m_baserng->decrement_index(idx0);
						ref0 = RefType(aggregate_tag(), this->dereference_index(idx0));
						if (!this->m_equals(*ref0, *ref1)) {
							idx0 = tc_move(idx1);
							break;
						}
						if (this->equal_index(this->begin_index(),idx0)) {
							break;
						}
					}
				}
			}

			auto element_base_index(index const& idx) const& noexcept {
				return idx;
			}

			auto base_range() & noexcept return_decltype(
				*m_baserng
			)

			auto base_range() const & noexcept return_decltype(
				*m_baserng
			)
		};

		template< typename IndexBase >
		struct unique_range_index {
			using index_base = IndexBase;
			index_base m_idxBegin;
			index_base m_idxEnd;
		};

		template<
			typename Derived,
			typename Rng,
			typename Equals
		>
		struct unique_range_adaptor
			: range_iterator_generator_from_index<
				Derived,
				unique_range_index<
					typename std::remove_reference_t<
						index_range_t<Rng>
					>::index
				>,
				typename boost::range_detail::demote_iterator_traversal_tag<
					boost::iterators::forward_traversal_tag,
					traversal_t<Rng>
				>::type
			>
		{
			using index = typename unique_range_adaptor::index;

		private:
			using this_type = unique_range_adaptor;

		protected:
			reference_or_value< index_range_t<Rng> > m_baserng;
			Equals m_equals;

		public:
			template<
				typename RngRef,
				typename EqualsRef
			>
			explicit unique_range_adaptor(RngRef&& rng, EqualsRef&& equals) noexcept
				: m_baserng(reference_or_value< index_range_t<Rng> >(aggregate_tag(), std::forward<RngRef>(rng)))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

			STATIC_VIRTUAL(FindSubRangeEnd)

			STATIC_OVERRIDE(begin_index)() const& noexcept -> index {
				auto idxBegin = m_baserng->begin_index();
				return {idxBegin, FindSubRangeEnd(idxBegin) };
			}

			STATIC_OVERRIDE(end_index)() const& noexcept -> index {
				return {m_baserng->end_index(), m_baserng->end_index()};
			}

			STATIC_OVERRIDE(increment_index)(index& idx) const& noexcept -> void {
				idx.m_idxBegin = tc_move(idx.m_idxEnd);
				idx.m_idxEnd = FindSubRangeEnd(idx.m_idxBegin);
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const& noexcept -> bool {
				return m_baserng->at_end_index(idx.m_idxBegin);
			}

			STATIC_OVERRIDE(dereference_index)(index const& idx) const& noexcept return_decltype(
				tc::slice(*m_baserng, idx.m_idxBegin, idx.m_idxEnd)
			)

			STATIC_OVERRIDE(dereference_index)(index const& idx) & noexcept return_decltype(
				tc::slice(*m_baserng, idx.m_idxBegin, idx.m_idxEnd)
			)

			STATIC_OVERRIDE(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return m_baserng->equal_index(idxLhs.m_idxBegin, idxRhs.m_idxBegin);
			}

		};

		template<
			typename Rng,
			typename Equals
		>
		struct unique_range_front_adaptor
			: unique_range_adaptor<unique_range_front_adaptor<Rng, Equals>, Rng, Equals>
		{
		private:
			using this_type = unique_range_front_adaptor;
			friend struct unique_range_adaptor<unique_range_front_adaptor<Rng, Equals>, Rng, Equals>;

		public:
			template<typename RhsRng, typename RhsEquals>
			explicit unique_range_front_adaptor(RhsRng&& rng, RhsEquals&& equals) noexcept : unique_range_adaptor<unique_range_front_adaptor, Rng, Equals>(std::forward<RhsRng>(rng), std::forward<RhsEquals>(equals))
			{}

			using index = typename unique_range_front_adaptor::index;

		private:
			using index_base = typename index::index_base;
			STATIC_FINAL(FindSubRangeEnd)(index_base const& idx) const& noexcept -> index_base {
				if (this->m_baserng->at_end_index(idx)) return idx;

				auto idxEnd = idx;
				auto&& front = this->m_baserng->dereference_index(idx);
				do {
					this->m_baserng->increment_index(idxEnd);
				} while (!this->m_baserng->at_end_index(idxEnd) && this->m_equals(front, this->m_baserng->dereference_index(idxEnd)));

				return idxEnd;
			}
		};

		template<
			typename Rng,
			typename Equals
		>
		struct unique_range_adjacent_adaptor
			: unique_range_adaptor<unique_range_adjacent_adaptor<Rng, Equals>, Rng, Equals>
		{
		private:
			using this_type = unique_range_adjacent_adaptor;
			friend struct unique_range_adaptor<unique_range_adjacent_adaptor<Rng, Equals>, Rng, Equals>;

		public:
			template<typename RhsRng, typename RhsEquals>
			explicit unique_range_adjacent_adaptor(RhsRng&& rng, RhsEquals&& equals) noexcept : unique_range_adaptor<unique_range_adjacent_adaptor, Rng, Equals>(std::forward<RhsRng>(rng), std::forward<RhsEquals>(equals))
			{}

			using index = typename unique_range_adjacent_adaptor::index;

		private:
			using index_base = typename index::index_base;

			STATIC_FINAL(FindSubRangeEnd)(index_base const& idx) const& noexcept -> index_base {
				if (this->m_baserng->at_end_index(idx)) return idx;

				for(auto idxEnd = idx;;) {
					auto const idxPrev = idxEnd;
					this->m_baserng->increment_index(idxEnd);
					if (
						this->m_baserng->at_end_index(idxEnd) ||
						!this->m_equals(this->m_baserng->dereference_index(idxPrev), this->m_baserng->dereference_index(idxEnd))
					) return idxEnd;
				}
			}
		};
	}

	using unique_range_adaptor_adl_barrier::unique_range_front_adaptor;
	using unique_range_adaptor_adl_barrier::unique_range_adjacent_adaptor;
	using unique_range_adaptor_adl_barrier::unique_adaptor;

	template<
		typename Rng,
		typename Equals
	>
	auto front_unique_range(Rng&& rng, Equals&& equals) noexcept return_ctor(
		unique_range_front_adaptor< view_by_value_t<Rng> BOOST_PP_COMMA() tc::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto front_unique_range(Rng&& rng) noexcept return_decltype(
		front_unique_range(std::forward<Rng>(rng),tc::fn_equal_to())
	)

	template<
		typename Rng,
		typename Equals
	>
	auto adjacent_unique_range(Rng&& rng, Equals&& equals) noexcept return_ctor(
		unique_range_adjacent_adaptor< view_by_value_t<Rng> BOOST_PP_COMMA() tc::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto adjacent_unique_range(Rng&& rng) noexcept return_decltype(
		adjacent_unique_range(std::forward<Rng>(rng), tc::fn_equal_to())
	)

	/*
		In contrase to std::unique, tc::adjacent_unique / tc::adjacent_unique_inplace always compares adjacent elements. This allows implementing
		bidirectional tc::adjacent_unique, with tc::adjacent_unique_inplace yielding the same result.
	*/
	template<
		typename Rng,
		typename Equals
	>
	auto adjacent_unique(Rng&& rng, Equals&& equals) noexcept return_ctor(
		unique_adaptor< view_by_value_t<Rng> BOOST_PP_COMMA() tc::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto adjacent_unique(Rng&& rng) noexcept return_decltype(
		adjacent_unique(std::forward<Rng>(rng),tc::fn_equal_to())
	)


}
