
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "iterator_cache.h"
#include "range_adaptor.h"

#include "../array.h"
#include "../algorithm/element.h"
#include "../algorithm/size_bounded.h"

namespace tc {
	namespace adjacent_tuples_adaptor_adl {
		template<typename Rng, std::size_t N, bool HasIterator = tc::is_range_with_iterators<Rng>::value>
		struct adjacent_tuples_adaptor;

		template<typename Rng, std::size_t N>
		struct [[nodiscard]] adjacent_tuples_adaptor<Rng, N, false> : tc::range_adaptor_base_range<Rng> {
			static_assert( 1 < N );
			using tc::range_adaptor_base_range<Rng>::range_adaptor_base_range;

			template<typename Self, typename Sink> requires tc::is_base_of_decayed<adjacent_tuples_adaptor, Self>::value
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				return internal_for_each(
					std::forward<Self>(self),
					std::forward<Sink>(sink),
					std::make_index_sequence<N - (tc::is_range_with_iterators<Rng>::value ? 1 : 2)>()
				);
			}

			template<ENABLE_SFINAE>
			[[nodiscard]] constexpr auto size() const& noexcept -> decltype(tc::size_raw(SFINAE_VALUE(this)->base_range())) {
				auto n = tc::size_raw(this->base_range());
				if( n < N ) {
					return 0;
				} else {
					return n - (N - 1);
				}
			}

		private:
			template<typename Self, typename Sink, std::size_t... i /*=0,1,...,N-2*/>
			static constexpr auto internal_for_each(Self&& self, Sink const sink, std::index_sequence<i...>) MAYTHROW {
				if constexpr (tc::is_range_with_iterators<Rng>::value) {
					auto GenerateAdjacentTuples = [&]() MAYTHROW {
						auto const itEnd = tc::end(self.base_range());
						auto it = tc::begin(self.base_range());
						auto ait=tc::explicit_cast<std::array<
							tc::iterator_cache<decltype(it)>,
							N
						>>(tc::func_tag, [&](std::size_t) noexcept { return it++; });

						for (;;) {
							for (std::size_t n = 0; n<N; ++n) {
								if (it == itEnd) {
									return tc::continue_if_not_break(sink, tc::forward_as_tuple(*tc_move_always(tc::at(ait, n)), *tc_move_always(tc::at(ait, (n + i + 1) % N))...)); // MAYTHROW
								}
								RETURN_IF_BREAK(tc::continue_if_not_break(sink, tc::forward_as_tuple(*tc_move_always(tc::at(ait, n)), *tc::at(ait, (n + i + 1) % N)...))); // MAYTHROW
								tc::at(ait, n) = it;
								++it;
							}
						}
					};
					return CONDITIONAL_PRVALUE_AS_VAL(
						tc::size_bounded(self.base_range(), N)<N,
						tc::constant<tc::continue_>(),
						GenerateAdjacentTuples() // MAYTHROW
					);
				} else {
					std::array<tc::storage_for<tc::range_value_t<decltype(std::declval<Self>().base_range())>>, N - 1> aoval;
					std::size_t n = 0;
					scope_exit( tc::for_each(tc::begin_next<tc::return_take>(aoval, tc::min(n, N - 1)), TC_MEM_FN(.dtor)) );

					return tc::for_each(std::forward<Self>(self).base_range(), [&](auto&& u) MAYTHROW {
						auto CallSink = [&]() MAYTHROW {
							return tc::continue_if_not_break( // MAYTHROW
								sink,
								tc::forward_as_tuple(
									tc_move_always(*tc::at(aoval, n % (N - 1))),
									*tc::at(aoval, (n + i + 1) % (N - 1))...,
									u
								)
							);
						};
						if (n < N - 1) {
							tc::at(aoval, n).ctor(tc_move_if_owned(u));
						} else {
							RETURN_IF_BREAK( CallSink() ); // MAYTHROW
							tc::renew(*tc::at(aoval, n % (N - 1)), tc_move_if_owned(u));
						}
						++n;
						return tc::implicit_cast<tc::common_type_t<decltype(CallSink()), tc::constant<tc::continue_>>>(tc::constant<tc::continue_>());
					});
				}
			}

			template<typename Self, std::enable_if_t<tc::is_base_of_decayed<adjacent_tuples_adaptor, Self>::value>* = nullptr>
			friend auto range_output_t_impl(Self&&) -> tc::type::list<tc::type::apply_t<tc::tuple,
				tc::type::repeat_n_t<
					N,
					tc::type::apply_t<
						tc::common_reference_xvalue_as_ref_t,
						tc::type::transform_t<
							tc::range_output_t<decltype(std::declval<Self>().base_range())>,
							std::add_rvalue_reference_t
						>
					>
				>
			>> {} // unevaluated
		};
	}

	namespace adjacent_tuples_adaptor_detail::no_adl {
		template<typename Rng, std::size_t N>
		struct adjacent_tuples_index {
			constexpr adjacent_tuples_index() = default;
			template<std::size_t... I>
			constexpr adjacent_tuples_index(Rng const& rng, index_t<std::remove_reference_t<Rng>> idx, std::index_sequence<I...>) noexcept
				: m_aidx{idx, ((tc::at_end_index(rng, idx) ? tc::discard(I) : tc::increment_index(rng, idx)), idx)...}
			{
				STATICASSERTSAME(std::index_sequence<I...>, std::make_index_sequence<N - 1>);
			}

			// If size(rng) < N, the single index has m_aidx = {begin_index(), ..., end_index(), ... end_index()}.
			tc::index_t<std::remove_reference_t<Rng>> m_aidx[N];

			friend constexpr bool operator==(adjacent_tuples_index const& lhs, adjacent_tuples_index const& rhs) noexcept {
				return lhs.m_aidx[0] == rhs.m_aidx[0];
			}
		};
	}

	namespace adjacent_tuples_adaptor_adl {
		template<typename Rng, std::size_t N>
		struct [[nodiscard]] adjacent_tuples_adaptor<Rng, N, true>
			: adjacent_tuples_adaptor<Rng, N, false>
			, tc::range_iterator_from_index<
				adjacent_tuples_adaptor<Rng, N, true>,
				adjacent_tuples_adaptor_detail::no_adl::adjacent_tuples_index<Rng, N>
			>
		{
		private:
			using this_type = adjacent_tuples_adaptor<Rng, N, true>;

		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex=std::disjunction<tc::has_stashing_index<std::remove_reference_t<Rng>>>::value;

			using adjacent_tuples_adaptor<Rng, N, false>::adjacent_tuples_adaptor;

		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return {this->base_range(), this->base_begin_index(), std::make_index_sequence<N - 1>()};
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(), idx.m_aidx[N - 1]);
			}

			STATIC_FINAL_MOD(
				TC_FWD(
					template<ENABLE_SFINAE, std::enable_if_t<
						tc::has_end_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value &&
						tc::has_decrement_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value
					>* = nullptr>
					constexpr
				),
				end_index
			)() const& noexcept -> tc_index {
				auto const idxBegin = this->base_begin_index();
				auto idxBase = this->base_end_index();
				tc_index idx;
				idx.m_aidx[N - 1] = idxBase;
				for( std::size_t i = N - 1; 0 < i; ) {
					--i;
					if( idxBegin == idxBase ) {
						idx = this->begin_index(); // Not return begin_index(); for NRVO
						break;
					} else {
						tc::decrement_index(this->base_range(), idxBase);
						idx.m_aidx[i] = idxBase;
					}
				}
				return idx;
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				// std::ranges::move(tc::begin_next<tc::return_drop>(idx.m_aidx), tc::begin(idx.m_aidx));
				for( std::size_t i = 0; i < N - 1; ++i ) {
					idx.m_aidx[i] = tc_move_always(idx.m_aidx[i + 1]);
				}
				tc::increment_index(this->base_range(), idx.m_aidx[N - 1]);
			}

			STATIC_FINAL_MOD(
				TC_FWD(
					template<ENABLE_SFINAE, std::enable_if_t<tc::has_decrement_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr>
					constexpr
				),
				decrement_index
			)(tc_index& idx) const& noexcept -> void {
				// std::ranges::move_backward(tc::end_prev<tc::return_take>(idx.m_aidx), tc::end(idx.m_aidx));
				for( std::size_t i = N - 1; 0 < i; ) {
					--i;
					idx.m_aidx[i + 1] = tc_move_always(idx.m_aidx[i]);
				}
				tc::decrement_index(this->base_range(), idx.m_aidx[0]);
			}

			template<typename Self, std::size_t... I>
			static constexpr auto dereference_index_impl(Self&& self, tc_index const& idx, std::index_sequence<I...>) noexcept
				-> tc::tuple<decltype(tc::dereference_index(self.base_range(), idx.m_aidx[I]))...>
			{
				return {{
					{{ tc::dereference_index(self.base_range(), idx.m_aidx[I]) }}...
				}};
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) const& noexcept {
				return dereference_index_impl(*this, idx, std::make_index_sequence<N>());
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) & noexcept {
				return dereference_index_impl(*this, idx, std::make_index_sequence<N>());
			}

			STATIC_FINAL_MOD(
				TC_FWD(
					template<ENABLE_SFINAE, std::enable_if_t<tc::has_advance_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr>
					constexpr
				),
				advance_index
			)(tc_index& idx, typename boost::range_difference<Rng>::type d) const& MAYTHROW -> void {
				idx = {this->base_range(), modified(idx.m_aidx[0], tc::advance_index(this->base_range(), _, d)), std::make_index_sequence<N - 1>()};
			}

			STATIC_FINAL_MOD(
				TC_FWD(
					template<ENABLE_SFINAE, std::enable_if_t<tc::has_distance_to_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr>
					constexpr
				),
				distance_to_index
			)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept {
				return tc::distance_to_index(this->base_range(), idxLhs.m_aidx[0], idxRhs.m_aidx[0]);
			}

			STATIC_FINAL_MOD(
				TC_FWD(
					template<ENABLE_SFINAE, std::enable_if_t<tc::has_middle_point<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr>
					constexpr
				),
				middle_point
			)(tc_index& idxLhs, tc_index const& idxRhs) const& noexcept -> void {
				tc::middle_point(this->base_range(), idxLhs.m_aidx[0], idxRhs.m_aidx[0]);
				idxLhs = {this->base_range(), idxLhs.m_aidx[0], std::make_index_sequence<N - 1>()};
			}
		};
	}

	namespace no_adl {
		template<typename Rng, std::size_t N>
		struct constexpr_size_base<adjacent_tuples_adaptor_adl::adjacent_tuples_adaptor<Rng, N>, std::void_t<typename tc::constexpr_size<Rng>::type>>
			: tc::constant<tc::constexpr_size<Rng>::value < N ? 0 : tc::constexpr_size<Rng>::value - (N - 1)>
		{};

		template<typename Rng, std::size_t N>
		struct is_index_valid_for_move_constructed_range<adjacent_tuples_adaptor_adl::adjacent_tuples_adaptor<Rng, N, true>>
			: tc::is_index_valid_for_move_constructed_range<Rng>
		{};
	}

	template<std::size_t N, typename Rng>
	constexpr adjacent_tuples_adaptor_adl::adjacent_tuples_adaptor<Rng, N> adjacent_tuples(Rng&& rng) noexcept {
		return {tc::aggregate_tag, std::forward<Rng>(rng)};
	}
}
