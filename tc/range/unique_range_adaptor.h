
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/static_polymorphism.h"
#include "../algorithm/compare.h"
#include "../algorithm/empty.h"
#include "range_adaptor.h"
#include "meta.h"
#include "subrange.h"

namespace tc {
	namespace unique_adaptor_detail {
		template<typename Rng, typename Index, typename Equals>
		constexpr void increment_index(Rng const& rng, Index& idx0, Equals const equals) noexcept(noexcept(equals(tc::dereference_index(rng, idx0), tc::dereference_index(rng, idx0)))) {
			using RefType = tc::reference_or_value<decltype(tc::dereference_index(rng, idx0))>;
			auto idx1 = idx0;
			tc::increment_index(rng, idx0);
			if (!tc::at_end_index(rng, idx0)) {
				RefType ref1(aggregate_tag, tc::dereference_index(rng, idx1));

				for (;;) {
					RefType ref0(aggregate_tag, tc::dereference_index(rng, idx0));
					if (!equals(tc::as_const(*ref1), tc::as_const(*ref0))) {
						return;
					}
					idx1 = idx0;
					if constexpr( tc::has_stashing_index<std::remove_reference_t<Rng>>::value ) {
						// Moving dereferenced stashing index is impossible. Must partially unroll loop.
						tc::increment_index(rng, idx1);
						if (tc::at_end_index(rng, idx1)) {
							idx0 = tc_move(idx1);
							return;
						}
						ref1 = {aggregate_tag, tc::dereference_index(rng, idx1)};
						if (!equals(tc::as_const(*ref0), tc::as_const(*ref1))) {
							idx0 = tc_move(idx1);
							return;
						}
						idx0 = idx1;
					}
					tc::increment_index(rng, idx0);
					if (tc::at_end_index(rng, idx0)) {
						return;
					}
					if constexpr( !tc::has_stashing_index<std::remove_reference_t<Rng>>::value ) {
						ref1 = tc_move(ref0);
					}
				}
			}
		}

		template<typename Rng, typename Index, typename Equals>
		constexpr void decrement_index(Rng const& rng, Index& idx0, Equals const equals) noexcept(noexcept(equals(tc::dereference_index(rng, idx0), tc::dereference_index(rng, idx0)))) {
			using RefType = tc::reference_or_value<decltype(tc::dereference_index(rng, idx0))>;
			auto const idxBegin = tc::begin_index(rng);
			_ASSERTE(idxBegin != idx0);
			tc::decrement_index(rng, idx0);
			if (idxBegin != idx0) {
				RefType ref0(aggregate_tag, tc::dereference_index(rng, idx0));
				auto idx1 = idx0;

				for (;;) {
					tc::decrement_index(rng, idx1);
					RefType ref1(aggregate_tag, tc::dereference_index(rng, idx1));
					if (!equals(tc::as_const(*ref1), tc::as_const(*ref0))) {
						return;
					}
					idx0 = idx1;
					if (idxBegin == idx1) {
						return;
					}
					if constexpr( tc::has_stashing_index<std::remove_reference_t<Rng>>::value ) {
						// Moving dereferenced stashing index is impossible. Must partially unroll loop.
						tc::decrement_index(rng, idx0);
						ref0 = {aggregate_tag, tc::dereference_index(rng, idx0)};
						if (!equals(tc::as_const(*ref0), tc::as_const(*ref1))) {
							idx0 = tc_move(idx1);
							return;
						} else if (idxBegin == idx0) {
							return;
						}
						idx1 = idx0;
					} else {
						ref0 = tc_move(ref1);
					}
				}
			}
		}
	}

	namespace unique_adaptor_adl {
		template<typename Rng, typename Equals, bool HasIterator = tc::range_with_iterators<Rng>>
		struct unique_adaptor;

		template<typename Rng, typename Equals>
		struct [[nodiscard]] unique_adaptor<Rng, Equals, false> : tc::range_adaptor_base_range<Rng>, tc::range_output_from_base_range {
			explicit constexpr unique_adaptor(auto&& rng, auto&& equals) noexcept
				: tc::range_adaptor_base_range<Rng>(aggregate_tag, tc_move_if_owned(rng))
				, m_equals(tc_move_if_owned(equals))
			{}

		protected:
			static_assert(tc::decayed<Equals>);
			Equals m_equals;

		public:
			template<tc::decayed_derived_from<unique_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				if constexpr( tc::range_with_iterators<Rng> ) {
					decltype(auto) rng = self.base_range();
					std::array<
						std::pair<
							tc::decay_t<decltype(self.base_begin_index())>,
							std::optional<tc::reference_or_value<decltype(tc::dereference_index(rng, self.base_begin_index()))>>
						>,
						2
					> apairidxref;
					auto* ppairidxorefPrev = std::addressof(apairidxref[1]);
					auto* ppairidxorefCurrent = std::addressof(apairidxref[0]);
					ppairidxorefCurrent->first = { self.base_begin_index() };

					return [&]() -> tc::common_type_t<decltype(tc::continue_if_not_break(sink, **ppairidxorefCurrent->second)), tc::constant<tc::continue_>> {
						bool bFirst = true;

						while( !tc::at_end_index(rng, ppairidxorefCurrent->first) ) { // MAYTHROW
							_ASSERTDEBUG( std::is_trivially_destructible<decltype(ppairidxorefPrev->second)>::value || !ppairidxorefCurrent->second );
							ppairidxorefCurrent->second.emplace(aggregate_tag, tc::dereference_index(rng, ppairidxorefCurrent->first) /* MAYTHROW */ );
							if( tc::change(bFirst, false) || [&]() MAYTHROW {
								bool const bEqual = self.m_equals(tc::as_const(**ppairidxorefPrev->second), tc::as_const(**ppairidxorefCurrent->second)); // MAYTHROW
								if constexpr( !std::is_trivially_destructible<decltype(ppairidxorefPrev->second)>::value ) {
									ppairidxorefPrev->second = std::nullopt;
								}
								return !bEqual;
							}() ) {
								tc_yield(sink, **ppairidxorefCurrent->second) /* MAYTHROW */;
							}
							tc::swap(ppairidxorefPrev, ppairidxorefCurrent);
							ppairidxorefCurrent->first = ppairidxorefPrev->first;
							tc::increment_index(rng, ppairidxorefCurrent->first); // MAYTHROW
						}

						return tc::constant<tc::continue_>();
					}();
				} else {
					std::optional<tc::range_value_t<decltype(std::declval<Self>().base_range())>> ovalLast;
					return tc::for_each(tc_move_if_owned(self).base_range(), [&](auto&& elem) MAYTHROW {
						bool const bSkip = tc::and_then(ovalLast, [&](auto const& valLast) MAYTHROW {
							return self.m_equals(valLast, tc::as_const(elem)); // MAYTHROW
						});
						ovalLast.emplace(tc_move_if_owned(elem));
						return tc_conditional_prvalue_as_val(bSkip, tc::constant<tc::continue_>(), tc::continue_if_not_break(sink, *ovalLast) /*MAYTHROW*/);
					});
				}
			}

			bool empty() const& noexcept {
				return tc::empty(this->base_range());
			}
		};

		template<typename Rng, typename Equals>
		struct [[nodiscard]] unique_adaptor<Rng, Equals, true>
			: unique_adaptor<Rng, Equals, false>
			, tc::range_iterator_from_index<
				unique_adaptor<Rng, Equals, true>,
				tc::index_t<std::remove_reference_t<Rng>>
			>
		{
		private:
			using this_type = unique_adaptor;
		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;

			using unique_adaptor<Rng, Equals, false>::unique_adaptor;
			
		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return this->base_begin_index();
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				return this->base_end_index();
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(),idx);
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(),idx)
			)

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) & return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(),idx)
			)

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept(noexcept(unique_adaptor_detail::increment_index(this->base_range(), idx, this->m_equals))) -> void {
				_ASSERTE(!this->at_end_index(idx));
				unique_adaptor_detail::increment_index(this->base_range(), idx, this->m_equals);
			}

			STATIC_FINAL_MOD(
				constexpr,
				decrement_index
			)(tc_index& idx) const& noexcept(noexcept(unique_adaptor_detail::decrement_index(this->base_range(), idx, this->m_equals))) -> void
				requires tc::has_decrement_index<std::remove_reference_t<Rng>>
			{
				unique_adaptor_detail::decrement_index(this->base_range(), idx, this->m_equals);
			}
		public:
			static constexpr auto element_base_index(tc_index const& idx) noexcept {
				return idx;
			}
		};
	}
	using unique_adaptor_adl::unique_adaptor;

	template<typename Rng, typename Equals>
	constexpr auto enable_stable_index_on_move<unique_adaptor<Rng, Equals, true>> = tc::stable_index_on_move<Rng>;

	namespace subrange_range_adaptor_detail::no_adl {
		template<typename IndexBase>
		struct subrange_range_index {
			IndexBase m_idxBegin;
			IndexBase m_idxEnd;

			friend constexpr bool operator==(subrange_range_index const& lhs, subrange_range_index const& rhs) noexcept {
				return EQUAL_MEMBERS(m_idxBegin);
			}
		};
	}

	namespace subrange_range_adaptor_adl {
		template<typename Derived, typename Rng>
		struct subrange_range_adaptor
			: range_iterator_from_index<Derived, subrange_range_adaptor_detail::no_adl::subrange_range_index<tc::index_t<std::remove_reference_t<Rng>>>>
			, protected tc::range_adaptor_base_range<Rng>
		{
		private:
			using this_type = subrange_range_adaptor;

		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex = false;

			using subrange_range_adaptor::range_adaptor_base_range::range_adaptor_base_range;

		private:
			STATIC_OVERRIDE_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(),idx.m_idxBegin);
			}

			STATIC_OVERRIDE_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				auto idxEnd = this->base_end_index();
				return {idxEnd, idxEnd};
			}

			STATIC_OVERRIDE_MOD(constexpr, dereference_index)(tc_index const& idx) const& return_decltype_noexcept(
				tc::slice(this->base_range(), idx.m_idxBegin, idx.m_idxEnd)
			)

			STATIC_OVERRIDE_MOD(constexpr, dereference_index)(tc_index const& idx) & return_decltype_noexcept(
				tc::slice(this->base_range(), idx.m_idxBegin, idx.m_idxEnd)
			)
		};
	}
	using subrange_range_adaptor_adl::subrange_range_adaptor;

	namespace partition_range_adaptor_adl {
		template<typename Derived, typename Rng>
		struct partition_range_adaptor : subrange_range_adaptor<Derived, Rng> {
			using typename partition_range_adaptor::subrange_range_adaptor::tc_index;
		private:
			using this_type = partition_range_adaptor;
		protected:
			STATIC_VIRTUAL(FindPartitionEnd) // (index_t<Rng>&) -> void
		private:
			STATIC_OVERRIDE_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				auto const idxBegin = this->base_begin_index();
				tc_index idx{idxBegin, idxBegin};
				if( !tc::at_end_index(this->base_range(), idx.m_idxEnd) ) {
					FindPartitionEnd(idx.m_idxEnd);
				}
				return idx;
			}

			STATIC_OVERRIDE_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				_ASSERT( idx.m_idxBegin != idx.m_idxEnd );
				idx.m_idxBegin = idx.m_idxEnd;
				if( !tc::at_end_index(this->base_range(), idx.m_idxEnd) ) {
					FindPartitionEnd(idx.m_idxEnd);
				}
			}

			using this_type::subrange_range_adaptor::subrange_range_adaptor;

			template<typename PartitionRange, std::enable_if_t<
				tc::decayed_derived_from<PartitionRange, partition_range_adaptor>
			>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend constexpr auto join_impl(PartitionRange&& partrng) return_decltype_xvalue_by_ref_noexcept(
				tc_move_if_owned(partrng).base_range()
			)

			template<typename SubPartitionRange, std::enable_if_t<
				tc::decayed_derived_from<
					tc::type::only_t<
						typename tc::is_instance<std::remove_reference_t<SubPartitionRange>, subrange>::arguments
					>,
					partition_range_adaptor
				>
			>* = nullptr>
			friend constexpr auto join_impl(SubPartitionRange&& subpartrng) return_decltype_noexcept(
				tc::slice(
					tc_move_if_owned(subpartrng).base_range().base_range(),
					tc_move_if_owned(subpartrng).begin_index().m_idxBegin,
					tc_move_if_owned(subpartrng).end_index().m_idxBegin
				)
			)
		};
	}
	using partition_range_adaptor_adl::partition_range_adaptor;

	namespace no_adl {
		template<typename Rng, typename Equals>
		struct [[nodiscard]] unique_range_front_adaptor : partition_range_adaptor<unique_range_front_adaptor<Rng, Equals>, Rng>
		{
		private:
			using this_type = unique_range_front_adaptor;
			static_assert(tc::decayed<Equals>);
			Equals m_equals;

		public:
			template<typename RhsRng, typename RhsEquals>
			constexpr unique_range_front_adaptor(RhsRng&& rng, RhsEquals&& equals) noexcept
				: this_type::partition_range_adaptor(tc::aggregate_tag, tc_move_if_owned(rng))
				, m_equals(tc_move_if_owned(equals))
			{}

		private:
			STATIC_FINAL_MOD(constexpr, FindPartitionEnd)(tc::index_t<std::remove_reference_t<Rng>>& idx) const& noexcept -> void {
				auto idxBegin = idx; // Necessary, if tc::has_stashing_index<std::remove_reference_t<Rng>>::value
				decltype(auto) front = tc::dereference_index(this->base_range(), idxBegin);
				static_assert( !std::is_rvalue_reference<decltype(front)>::value ); // rvalue reference should be safe, or is it not?
				do {
					tc::increment_index(this->base_range(),idx);
				} while (!tc::at_end_index(this->base_range(),idx) && this->m_equals(tc::as_const(front), tc::as_const(tc::dereference_index(this->base_range(),idx))));
			}
		};

		template<typename Rng, typename Equals>
		struct [[nodiscard]] unique_range_adjacent_adaptor : partition_range_adaptor<unique_range_adjacent_adaptor<Rng, Equals>, Rng>
		{
		private:
			using this_type = unique_range_adjacent_adaptor;
			static_assert(tc::decayed<Equals>);
			Equals m_equals;

		public:
			template<typename RhsRng, typename RhsEquals>
			explicit unique_range_adjacent_adaptor(RhsRng&& rng, RhsEquals&& equals) noexcept
				: this_type::partition_range_adaptor(tc::aggregate_tag, tc_move_if_owned(rng))
				, m_equals(tc_move_if_owned(equals))
			{}

		private:
			STATIC_FINAL_MOD(constexpr, FindPartitionEnd)(tc::index_t<std::remove_reference_t<Rng>>& idx) const& noexcept -> void {
				unique_adaptor_detail::increment_index(this->base_range(), idx, m_equals);
			}

			STATIC_FINAL_MOD(
				constexpr,
				decrement_index
			)(typename this_type::tc_index& idx) const& noexcept -> void
				requires tc::has_decrement_index<std::remove_reference_t<Rng>>
			{
				idx.m_idxEnd = idx.m_idxBegin;
				unique_adaptor_detail::decrement_index(this->base_range(), idx.m_idxBegin, m_equals);
			}
		};
	}

	using no_adl::unique_range_front_adaptor;
	using no_adl::unique_range_adjacent_adaptor;

	template<typename Rng, typename Equals>
	constexpr auto enable_stable_index_on_move<unique_range_front_adaptor<Rng, Equals>> = tc::stable_index_on_move<Rng>;
	template<typename Rng, typename Equals>
	constexpr auto enable_stable_index_on_move<unique_range_adjacent_adaptor<Rng, Equals>> = tc::stable_index_on_move<Rng>;

	template<
		typename Rng,
		typename Equals
	>
	constexpr auto front_unique_range(Rng&& rng, Equals&& equals) return_ctor_noexcept(
		TC_FWD(unique_range_front_adaptor< Rng, tc::decay_t<Equals> >),
		(tc_move_if_owned(rng), tc_move_if_owned(equals))
	)

	template< typename Rng >
	constexpr auto front_unique_range(Rng&& rng) return_decltype_noexcept(
		front_unique_range(tc_move_if_owned(rng),tc::fn_equal_to())
	)

	template<
		typename Rng,
		typename Equals
	>
	auto adjacent_unique_range(Rng&& rng, Equals&& equals) return_ctor_noexcept(
		TC_FWD(unique_range_adjacent_adaptor< Rng, tc::decay_t<Equals> >),
		(tc_move_if_owned(rng), tc_move_if_owned(equals))
	)

	template< typename Rng >
	auto adjacent_unique_range(Rng&& rng) return_decltype_noexcept(
		adjacent_unique_range(tc_move_if_owned(rng), tc::fn_equal_to())
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
		TC_FWD(unique_adaptor< Rng, tc::decay_t<Equals> >),
		(tc_move_if_owned(rng), tc_move_if_owned(equals))
	)

	template< typename Rng >
	constexpr auto adjacent_unique(Rng&& rng) return_decltype_noexcept(
		adjacent_unique(tc_move_if_owned(rng),tc::fn_equal_to())
	)
}
