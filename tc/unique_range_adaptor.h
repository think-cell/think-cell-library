
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "static_polymorphism.h"
#include "sub_range.h"

namespace tc {

	namespace no_adl {

		template<
			typename Rng,
			typename Equals
		>
		struct unique_adaptor
			: range_iterator_generator_from_index<
				unique_adaptor<Rng, Equals>,
				tc::index_t<std::remove_reference_t<Rng>>,
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
				: m_baserng(reference_or_value< Rng >(aggregate_tag(), std::forward<RngRef>(rng)))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

		private:
			using this_type = unique_adaptor;
			reference_or_value< Rng > m_baserng;
			Equals m_equals;

		public:

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return tc::begin_index(m_baserng);
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return tc::end_index(m_baserng);
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(*m_baserng,idx);
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept return_decltype(
				tc::dereference_index(*m_baserng,idx)
			)

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept return_decltype(
				tc::dereference_index(*m_baserng,idx)
			)

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::equal_index(*m_baserng,idxLhs, idxRhs);
			}

			STATIC_FINAL(increment_index)(index& idx0) const& noexcept -> void {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERT(!this->at_end_index(idx0));

				auto idx1=idx0;
				tc::increment_index(*m_baserng,idx0);
				if (!tc::at_end_index(*m_baserng,idx0)) {
					RefType ref1(aggregate_tag(), this->dereference_index(idx1));

					for (;;) {
						RefType ref0(aggregate_tag(), this->dereference_index(idx0));
						if (!this->m_equals(*ref1, *ref0)) {
							break;
						}

						idx1 = idx0;
						tc::increment_index(*m_baserng,idx1);
						if (tc::at_end_index(*m_baserng,idx1)) {
							idx0 = tc_move(idx1);
							break;
						}
						ref1 = RefType(aggregate_tag(), this->dereference_index(idx1));
						if (!this->m_equals(*ref0, *ref1)) {
							idx0 = tc_move(idx1);
							break;
						}

						idx0 = idx1;
						tc::increment_index(*m_baserng,idx0);
						if (tc::at_end_index(*m_baserng,idx0)) {
							break;
						}
					}
				}
			}

			STATIC_FINAL(decrement_index)(index& idx0) const& noexcept -> void {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERT(!this->equal_index(this->begin_index(),idx0));

				tc::decrement_index(*m_baserng,idx0);
				if (!this->equal_index(this->begin_index(),idx0)) {
					RefType ref0(aggregate_tag(), this->dereference_index(idx0));

					for (;;) {
						auto idx1=idx0;
						tc::decrement_index(*m_baserng,idx1);
						RefType ref1(aggregate_tag(), this->dereference_index(idx1));
						if (!this->m_equals(*ref1, *ref0)) {
							break;
						}
						if (this->equal_index(this->begin_index(),idx1)) {
							idx0 = tc_move(idx1);
							break;
						}

						idx0 = idx1;
						tc::decrement_index(*m_baserng,idx0);
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

			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *std::move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}
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
					tc::index_t<std::remove_reference_t<Rng>>
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
			reference_or_value< Rng > m_baserng;
			Equals m_equals;

		public:
			template<
				typename RngRef,
				typename EqualsRef
			>
			explicit unique_range_adaptor(RngRef&& rng, EqualsRef&& equals) noexcept
				: m_baserng(reference_or_value< Rng >(aggregate_tag(), std::forward<RngRef>(rng)))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

			STATIC_VIRTUAL(FindSubRangeEnd)

			STATIC_OVERRIDE(begin_index)() const& noexcept -> index {
				auto idxBegin = tc::begin_index(m_baserng);
				return {idxBegin, FindSubRangeEnd(idxBegin) };
			}

			STATIC_OVERRIDE(end_index)() const& noexcept -> index {
				return {tc::end_index(m_baserng), tc::end_index(m_baserng)};
			}

			STATIC_OVERRIDE(increment_index)(index& idx) const& noexcept -> void {
				idx.m_idxBegin = tc_move(idx.m_idxEnd);
				idx.m_idxEnd = FindSubRangeEnd(idx.m_idxBegin);
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(*m_baserng,idx.m_idxBegin);
			}

			STATIC_OVERRIDE(dereference_index)(index const& idx) const& noexcept return_decltype(
				tc::slice(*m_baserng, idx.m_idxBegin, idx.m_idxEnd)
			)

			STATIC_OVERRIDE(dereference_index)(index const& idx) & noexcept return_decltype(
				tc::slice(*m_baserng, idx.m_idxBegin, idx.m_idxEnd)
			)

			STATIC_OVERRIDE(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::equal_index(*m_baserng,idxLhs.m_idxBegin, idxRhs.m_idxBegin);
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
				if (tc::at_end_index(*this->m_baserng,idx)) return idx;

				auto idxEnd = idx;
				auto&& front = tc::dereference_index(*this->m_baserng,idx);
				do {
					tc::increment_index(*this->m_baserng,idxEnd);
				} while (!tc::at_end_index(*this->m_baserng,idxEnd) && this->m_equals(front, tc::dereference_index(*this->m_baserng,idxEnd)));

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
				if (tc::at_end_index(*this->m_baserng,idx)) return idx;

				for(auto idxEnd = idx;;) {
					auto const idxPrev = idxEnd;
					tc::increment_index(*this->m_baserng,idxEnd);
					if (
						tc::at_end_index(*this->m_baserng,idxEnd) ||
						!this->m_equals(tc::dereference_index(*this->m_baserng,idxPrev), tc::dereference_index(*this->m_baserng,idxEnd))
					) return idxEnd;
				}
			}
		};
	}

	using no_adl::unique_range_front_adaptor;
	using no_adl::unique_range_adjacent_adaptor;
	using no_adl::unique_adaptor;

	template<
		typename Rng,
		typename Equals
	>
	auto front_unique_range(Rng&& rng, Equals&& equals) noexcept return_ctor(
		unique_range_front_adaptor< Rng BOOST_PP_COMMA() tc::decay_t<Equals> >,
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
		unique_range_adjacent_adaptor< Rng BOOST_PP_COMMA() tc::decay_t<Equals> >,
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
		unique_adaptor< Rng BOOST_PP_COMMA() tc::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto adjacent_unique(Rng&& rng) noexcept return_decltype(
		adjacent_unique(std::forward<Rng>(rng),tc::fn_equal_to())
	)


}
