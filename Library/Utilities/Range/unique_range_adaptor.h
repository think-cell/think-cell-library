#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "../static_polymorphism.h"

namespace RANGE_PROPOSAL_NAMESPACE {

	namespace unique_range_adaptor_adl_barrier {

		template<
			typename Rng,
			typename Equals
		>
		struct unique_adaptor
			: range_generator_from_index<
				unique_adaptor<Rng, Equals>,
				range_iterator_from_index<
					unique_adaptor<Rng, Equals>,
					typename std::remove_reference<
						typename index_range<Rng>::type
					>::type::index,
					typename boost::range_detail::demote_iterator_traversal_tag<
						boost::iterators::bidirectional_traversal_tag,
						traversal_t<Rng>
					>::type
				>
			>
		{
			using index = typename unique_adaptor::index;

			template<
				typename RngRef,
				typename EqualsRef
			>
			unique_adaptor(RngRef&& rng, EqualsRef&& equals)
				: m_baserng(reference_or_value< typename index_range<Rng>::type >(std::forward<RngRef>(rng), aggregate_tag()))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

		private:
			using this_type = unique_adaptor;
			reference_or_value< typename index_range<Rng>::type > m_baserng;
            Equals m_equals;

		public:

			STATIC_FINAL(begin_index)() const -> index {
				return m_baserng->begin_index();
			}

			STATIC_FINAL(end_index)() const -> index {
				return m_baserng->end_index();
			}

			STATIC_FINAL(at_end_index)(index const& idx) const -> bool {
				return m_baserng->at_end_index(idx);
			}

			bool at_begin_index(index const& idx) const {
				return m_baserng->at_begin_index(idx);
			}

			STATIC_FINAL(dereference_index)(index const& idx) const return_decltype(
				m_baserng->dereference_index(idx)
			)

			STATIC_FINAL(dereference_index)(index const& idx) return_decltype(
				m_baserng->dereference_index(idx)
			)

			bool equal_index(index const& idxLhs, index const& idxRhs) const {
				return m_baserng->equal_index(idxLhs, idxRhs);
			}

			STATIC_FINAL(increment_index)(index& idx0) const -> void {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERT(!this->at_end_index(idx0));

				auto idx1=idx0;
				m_baserng->increment_index(idx0);
				if (!m_baserng->at_end_index(idx0)) {
					RefType ref1(this->dereference_index(idx1), aggregate_tag());

					for (;;) {
						RefType ref0(this->dereference_index(idx0), aggregate_tag());
						if (!this->m_equals(*ref1, *ref0)) {
							break;
						}

						idx1 = idx0;
						m_baserng->increment_index(idx0);
						if (m_baserng->at_end_index(idx0)) {
							break;
						}
						ref1 = RefType(this->dereference_index(idx0), aggregate_tag());
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


			void decrement_index(index& idx0) const {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERT(!equal_index(this->begin_index(),idx0));

				m_baserng->decrement_index(idx0);
				if (!equal_index(this->begin_index(),idx0)) {
					RefType ref0(this->dereference_index(idx0), aggregate_tag());

					for (;;) {
						auto idx1=idx0;
						m_baserng->decrement_index(idx1);
						RefType ref1(this->dereference_index(idx1), aggregate_tag());
						if (!this->m_equals(*ref1, *ref0)) {
							break;
						}
						if (equal_index(this->begin_index(),idx1)) {
							idx0 = tc_move(idx1);
							break;
						}

						idx0 = idx1;
						m_baserng->decrement_index(idx0);
						ref0 = RefType(this->dereference_index(idx0), aggregate_tag());
						if (!this->m_equals(*ref0, *ref1)) {
							idx0 = tc_move(idx1);
							break;
						}
						if (equal_index(this->begin_index(),idx0)) {
							break;
						}
					}
				}
			}

			auto base_range() return_decltype(
				*m_baserng
			)

			auto base_range() const return_decltype(
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
			: range_generator_from_index<
				Derived,
				range_iterator_from_index<
					Derived,
					unique_range_index<
						typename std::remove_reference<
							typename index_range<Rng>::type
						>::type::index
					>,
					typename boost::range_detail::demote_iterator_traversal_tag<
						boost::iterators::forward_traversal_tag,
						traversal_t<Rng>
					>::type
				>
			>
		{
			using index = typename unique_range_adaptor::index;

		private:
			using this_type = unique_range_adaptor;

		protected:
			reference_or_value< typename index_range<Rng>::type > m_baserng;
			Equals m_equals;

		public:
			template<
				typename RngRef,
				typename EqualsRef
			>
			unique_range_adaptor(RngRef&& rng, EqualsRef&& equals)
				: m_baserng(reference_or_value< typename index_range<Rng>::type >(std::forward<RngRef>(rng), aggregate_tag()))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

			STATIC_VIRTUAL(FindSubRangeEnd)

			STATIC_OVERRIDE(begin_index)() const -> index {
				auto idxBegin = m_baserng->begin_index();
				return {idxBegin, FindSubRangeEnd(idxBegin) };
			}

			STATIC_OVERRIDE(end_index)() const -> index {
				return {m_baserng->end_index(), m_baserng->end_index()};
			}

			STATIC_OVERRIDE(increment_index)(index& idx) const -> void {
				idx.m_idxBegin = tc_move(idx.m_idxEnd);
				idx.m_idxEnd = FindSubRangeEnd(idx.m_idxBegin);
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const -> bool {
				return m_baserng->at_end_index(idx.m_idxBegin);
			}

			bool at_begin_index(index const& idx) const {
				return m_baserng->at_begin_index(idx.m_idxBegin);
			}

			STATIC_OVERRIDE(dereference_index)(index const& idx) const return_decltype(
				tc::slice(*m_baserng, idx.m_idxBegin, idx.m_idxEnd)
			)

			STATIC_OVERRIDE(dereference_index)(index const& idx) return_decltype(
				tc::slice(*m_baserng, idx.m_idxBegin, idx.m_idxEnd)
			)

			bool equal_index(index const& idxLhs, index const& idxRhs) const {
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
			friend struct unique_range_adaptor<unique_range_front_adaptor<Rng, Equals>, Rng, Equals>;

			template<typename RhsRng, typename RhsEquals>
			unique_range_front_adaptor(RhsRng&& rng, RhsEquals&& equals) : unique_range_adaptor<unique_range_front_adaptor, Rng, Equals>(std::forward<RhsRng>(rng), std::forward<RhsEquals>(equals))
			{}

			using index = typename unique_range_front_adaptor::index;

		private:
			using index_base = typename index::index_base;
			using this_type = unique_range_front_adaptor;
			STATIC_FINAL(FindSubRangeEnd)(index_base const& idx) const -> index_base {
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
			friend struct unique_range_adaptor<unique_range_adjacent_adaptor<Rng, Equals>, Rng, Equals>;

			template<typename RhsRng, typename RhsEquals>
			unique_range_adjacent_adaptor(RhsRng&& rng, RhsEquals&& equals) : unique_range_adaptor<unique_range_adjacent_adaptor, Rng, Equals>(std::forward<RhsRng>(rng), std::forward<RhsEquals>(equals))
			{}

			using index = typename unique_range_adjacent_adaptor::index;

		private:
			using index_base = typename index::index_base;
			using this_type = unique_range_adjacent_adaptor;

			STATIC_FINAL(FindSubRangeEnd)(index_base const& idx) const -> index_base {
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
	auto front_unique_range(Rng&& rng, Equals&& equals) return_ctor(
		unique_range_front_adaptor< typename range_by_value<Rng>::type BOOST_PP_COMMA() std::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto front_unique_range(Rng&& rng) return_decltype(
		front_unique_range(std::forward<Rng>(rng),tc::fn_equal_to())
	)

	template<
		typename Rng,
		typename Equals
	>
	auto adjacent_unique_range(Rng&& rng, Equals&& equals) return_ctor(
		unique_range_adjacent_adaptor< typename range_by_value<Rng>::type BOOST_PP_COMMA() std::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto adjacent_unique_range(Rng&& rng) return_decltype(
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
	auto adjacent_unique(Rng&& rng, Equals&& equals) return_ctor(
		unique_adaptor< typename range_by_value<Rng>::type BOOST_PP_COMMA() std::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto adjacent_unique(Rng&& rng) return_decltype(
		adjacent_unique(std::forward<Rng>(rng),tc::fn_equal_to())
	)


}
