
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "static_polymorphism.h"
#include "subrange.h"

namespace tc {

	namespace no_adl {

		template<
			typename Rng,
			typename Equals
		>
		struct [[nodiscard]] unique_adaptor
			: range_iterator_from_index<
				unique_adaptor<Rng, Equals>,
				tc::index_t<std::remove_reference_t<Rng>>
			>
			, tc::range_adaptor_base_range<Rng>
		{
		private:
			using this_type = unique_adaptor;
		public:
			using typename this_type::range_iterator_from_index::index;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;
			
			template<
				typename RngRef,
				typename EqualsRef
			>
			explicit constexpr unique_adaptor(RngRef&& rng, EqualsRef&& equals) noexcept
				: tc::range_adaptor_base_range<Rng>(aggregate_tag, std::forward<RngRef>(rng))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

		private:
			Equals m_equals;

			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> index {
				return this->base_begin_index();
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> index {
				return this->base_end_index();
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(),idx);
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(),idx)
			)

			STATIC_FINAL_MOD(constexpr, dereference_index)(index const& idx) & return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(),idx)
			)

			STATIC_FINAL_MOD(constexpr, increment_index)(index& idx0) const& noexcept -> void {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERTE(!this->at_end_index(idx0));

				auto idx1=idx0;
				tc::increment_index(this->base_range(),idx0);
				if (!tc::at_end_index(this->base_range(),idx0)) {
					RefType ref1(aggregate_tag, this->dereference_index(idx1));

					for (;;) {
						RefType ref0(aggregate_tag, this->dereference_index(idx0));
						if (!this->m_equals(tc::as_const(*ref1), tc::as_const(*ref0))) {
							break;
						}

						idx1 = idx0;
						tc::increment_index(this->base_range(),idx1);
						if (tc::at_end_index(this->base_range(),idx1)) {
							idx0 = tc_move(idx1);
							break;
						}
						ref1 = RefType(aggregate_tag, this->dereference_index(idx1));
						if (!this->m_equals(tc::as_const(*ref0), tc::as_const(*ref1))) {
							idx0 = tc_move(idx1);
							break;
						}

						idx0 = idx1;
						tc::increment_index(this->base_range(),idx0);
						if (tc::at_end_index(this->base_range(),idx0)) {
							break;
						}
					}
				}
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(index& idx0) const& noexcept -> void {
				using RefType = tc::reference_or_value<decltype(this->dereference_index(idx0))>;

				_ASSERTE(this->begin_index() != idx0);

				tc::decrement_index(this->base_range(),idx0);
				if (this->begin_index() != idx0) {
					RefType ref0(aggregate_tag, this->dereference_index(idx0));

					for (;;) {
						auto idx1=idx0;
						tc::decrement_index(this->base_range(),idx1);
						RefType ref1(aggregate_tag, this->dereference_index(idx1));
						if (!this->m_equals(tc::as_const(*ref1), tc::as_const(*ref0))) {
							break;
						}
						if (this->begin_index() == idx1) {
							idx0 = tc_move(idx1);
							break;
						}

						idx0 = idx1;
						tc::decrement_index(this->base_range(),idx0);
						ref0 = RefType(aggregate_tag, this->dereference_index(idx0));
						if (!this->m_equals(tc::as_const(*ref0), tc::as_const(*ref1))) {
							idx0 = tc_move(idx1);
							break;
						}
						if (this->begin_index() == idx0) {
							break;
						}
					}
				}
			}
		public:
			constexpr auto element_base_index(index const& idx) const& noexcept {
				return idx;
			}
		};
	}

	using no_adl::unique_adaptor;

	namespace unique_range_adaptor_adl {
		template< typename IndexBase >
		struct unique_range_index : tc::equality_comparable<unique_range_index<IndexBase>> {
			using index_base = IndexBase;
			index_base m_idxBegin;
			index_base m_idxEnd;

			friend bool operator==(unique_range_index const& lhs, unique_range_index const& rhs) noexcept {
				return EQUAL_MEMBERS(m_idxBegin);
			}
		};

		template<
			typename Derived,
			typename Rng,
			typename Equals
		>
		struct unique_range_adaptor
			: range_iterator_from_index<
				Derived,
				unique_range_index<
					tc::index_t<std::remove_reference_t<Rng>>
				>
			>
			, protected tc::range_adaptor_base_range<Rng>
		{
		private:
			using this_type = unique_range_adaptor;

		protected:
			Equals m_equals;

		public:
			using typename this_type::range_iterator_from_index::index;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;

			template<
				typename RngRef,
				typename EqualsRef
			>
			explicit unique_range_adaptor(RngRef&& rng, EqualsRef&& equals) noexcept
				: tc::range_adaptor_base_range<Rng>(aggregate_tag, std::forward<RngRef>(rng))
				, m_equals(std::forward<EqualsRef>(equals))
			{}

			STATIC_VIRTUAL(FindSubRangeEnd)
		private:
			STATIC_OVERRIDE(begin_index)() const& noexcept -> index {
				auto idxBegin = this->base_begin_index();
				return {{}, idxBegin, FindSubRangeEnd(idxBegin) };
			}

			STATIC_OVERRIDE(end_index)() const& noexcept -> index {
				return {{}, this->base_end_index(), this->base_end_index()};
			}

			STATIC_OVERRIDE(increment_index)(index& idx) const& noexcept -> void {
				idx.m_idxBegin = tc_move(idx.m_idxEnd);
				idx.m_idxEnd = FindSubRangeEnd(idx.m_idxBegin);
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(),idx.m_idxBegin);
			}

			STATIC_OVERRIDE(dereference_index)(index const& idx) const& return_decltype_noexcept(
				tc::slice(this->base_range(), idx.m_idxBegin, idx.m_idxEnd)
			)

			STATIC_OVERRIDE(dereference_index)(index const& idx) & return_decltype_noexcept(
				tc::slice(this->base_range(), idx.m_idxBegin, idx.m_idxEnd)
			)

			template<typename UniqueRng, std::enable_if_t<
				tc::is_base_of_decayed<unique_range_adaptor, UniqueRng>::value
			>* = nullptr>
			friend constexpr auto join_impl(UniqueRng&& uniquerng) return_decltype_xvalue_by_ref_noexcept(
				std::forward<UniqueRng>(uniquerng).base_range()
			)

			template<typename SubUniqueRng, std::enable_if_t<
				tc::is_base_of_decayed<
					unique_range_adaptor,
					tc::type::only_t<
						typename tc::is_instance<subrange, std::remove_reference_t<SubUniqueRng>>::arguments
					>
				>::value
			>* = nullptr>
			friend constexpr auto join_impl(SubUniqueRng&& subuniquerng) return_decltype_noexcept(
				tc::slice(
					std::forward<SubUniqueRng>(subuniquerng).base_range().base_range(),
					std::forward<SubUniqueRng>(subuniquerng).begin_index().m_idxBegin,
					std::forward<SubUniqueRng>(subuniquerng).end_index().m_idxBegin
				)
			)
		};
	}

	using unique_range_adaptor_adl::unique_range_adaptor;

	namespace no_adl {
		template<
			typename Rng,
			typename Equals
		>
		struct [[nodiscard]] unique_range_front_adaptor
			: unique_range_adaptor<unique_range_front_adaptor<Rng, Equals>, Rng, Equals>
		{
		private:
			using this_type = unique_range_front_adaptor;
			friend struct unique_range_adaptor<unique_range_front_adaptor<Rng, Equals>, Rng, Equals>;

		public:
			template<typename RhsRng, typename RhsEquals>
			explicit unique_range_front_adaptor(RhsRng&& rng, RhsEquals&& equals) noexcept : unique_range_adaptor<unique_range_front_adaptor, Rng, Equals>(std::forward<RhsRng>(rng), std::forward<RhsEquals>(equals))
			{}

			using typename this_type::unique_range_adaptor::index;

		private:
			using index_base = typename index::index_base;
			STATIC_FINAL(FindSubRangeEnd)(index_base const& idx) const& noexcept -> index_base {
				if (tc::at_end_index(this->base_range(),idx)) return idx;

				auto idxEnd = idx;
				auto const& front = tc::dereference_index(this->base_range(),idx);
				do {
					tc::increment_index(this->base_range(),idxEnd);
				} while (!tc::at_end_index(this->base_range(),idxEnd) && this->m_equals(front, tc::as_const(tc::dereference_index(this->base_range(),idxEnd))));

				return idxEnd;
			}
		};

		template<
			typename Rng,
			typename Equals
		>
		struct [[nodiscard]] unique_range_adjacent_adaptor
			: unique_range_adaptor<unique_range_adjacent_adaptor<Rng, Equals>, Rng, Equals>
		{
		private:
			using this_type = unique_range_adjacent_adaptor;
			friend struct unique_range_adaptor<unique_range_adjacent_adaptor<Rng, Equals>, Rng, Equals>;

		public:
			template<typename RhsRng, typename RhsEquals>
			explicit unique_range_adjacent_adaptor(RhsRng&& rng, RhsEquals&& equals) noexcept : unique_range_adaptor<unique_range_adjacent_adaptor, Rng, Equals>(std::forward<RhsRng>(rng), std::forward<RhsEquals>(equals))
			{}

			using typename this_type::unique_range_adaptor::index;

		private:
			using index_base = typename index::index_base;

			STATIC_FINAL(FindSubRangeEnd)(index_base const& idx) const& noexcept -> index_base {
				if (tc::at_end_index(this->base_range(),idx)) return idx;

				for(auto idxEnd = idx;;) {
					auto const idxPrev = idxEnd;
					tc::increment_index(this->base_range(),idxEnd);
					if (
						tc::at_end_index(this->base_range(),idxEnd) ||
						!this->m_equals(tc::as_const(tc::dereference_index(this->base_range(),idxPrev)), tc::as_const(tc::dereference_index(this->base_range(),idxEnd)))
					) return idxEnd;
				}
			}
		};
	}

	using no_adl::unique_range_front_adaptor;
	using no_adl::unique_range_adjacent_adaptor;

	template<
		typename Rng,
		typename Equals
	>
	auto front_unique_range(Rng&& rng, Equals&& equals) return_ctor_noexcept(
		unique_range_front_adaptor< Rng BOOST_PP_COMMA() tc::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto front_unique_range(Rng&& rng) return_decltype_noexcept(
		front_unique_range(std::forward<Rng>(rng),tc::fn_equal_to())
	)

	template<
		typename Rng,
		typename Equals
	>
	auto adjacent_unique_range(Rng&& rng, Equals&& equals) return_ctor_noexcept(
		unique_range_adjacent_adaptor< Rng BOOST_PP_COMMA() tc::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	auto adjacent_unique_range(Rng&& rng) return_decltype_noexcept(
		adjacent_unique_range(std::forward<Rng>(rng), tc::fn_equal_to())
	)

	/*
		In contrast to std::unique, tc::adjacent_unique / tc::adjacent_unique_inplace always compares adjacent elements. This allows implementing
		bidirectional tc::adjacent_unique, with tc::adjacent_unique_inplace yielding the same result.
	*/
	template<
		typename Rng,
		typename Equals
	>
	constexpr auto adjacent_unique(Rng&& rng, Equals&& equals) return_ctor_noexcept(
		unique_adaptor< Rng BOOST_PP_COMMA() tc::decay_t<Equals> >,
		(std::forward<Rng>(rng), std::forward<Equals>(equals))
	)

	template< typename Rng >
	constexpr auto adjacent_unique(Rng&& rng) return_decltype_noexcept(
		adjacent_unique(std::forward<Rng>(rng),tc::fn_equal_to())
	)
}
