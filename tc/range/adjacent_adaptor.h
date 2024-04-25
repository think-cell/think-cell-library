
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
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
	namespace adjacent_adaptor_detail {
		namespace no_adl {
			template<typename Sink, typename T, std::size_t... i>
			struct adjacent_adaptor_generator_base_sink { // MSVC workaround: not a lambda for shorter symbol names
				static auto constexpr N = sizeof...(i) + 1; // corresponds to adjacent_adaptor<Rng, N+1>
				Sink m_sink;
				std::array<tc::storage_for<T>, N> m_aoval;
				std::size_t m_n = 0;

				constexpr ~adjacent_adaptor_generator_base_sink() {
					tc::for_each(tc::begin_next<tc::return_take>(m_aoval, tc::min(m_n, N)), tc_mem_fn(.dtor));
				}

				constexpr auto operator()(auto&& u) & MAYTHROW {
					auto const CallSink = [&]() MAYTHROW {
						return tc::continue_if_not_break( // MAYTHROW
							m_sink,
							tc::tie(
								tc_move_always(*tc::at(m_aoval, m_n % N)),
								*tc::at(m_aoval, (m_n + i + 1) % N)...,
								u
							)
						);
					};
					if (m_n < N) {
						tc::at(m_aoval, m_n).ctor(tc_move_if_owned(u));
					} else {
						tc_return_if_break( CallSink() ); // MAYTHROW
						tc::assign_explicit_cast(*tc::at(m_aoval, m_n % N), tc_move_if_owned(u)); // MAYTHROW
					}
					++m_n;
					return tc::implicit_cast<tc::common_type_t<decltype(CallSink()), tc::constant<tc::continue_>>>(tc::constant<tc::continue_>());
				}
			};
		}

		template<typename Self, typename Sink, std::size_t... i /*=0,1,...,N-2*/, std::size_t N = sizeof...(i) + 1>
		constexpr auto adjacent_adaptor_for_each(Self&& self, Sink sink, std::index_sequence<i...>) MAYTHROW {
			if constexpr (tc::range_with_iterators<decltype(self.base_range())>) {
				auto const GenerateAdjacentTuples = [&]() MAYTHROW {
					auto const itEnd = tc::end(self.base_range());
					auto it = tc::begin(self.base_range());
					auto ait=tc::explicit_cast<std::array<
						tc::iterator_cache<decltype(it)>,
						N
					>>(tc::func_tag, [&](std::size_t) noexcept { return it++; });

					for (;;) {
						for (std::size_t n = 0; n<N; ++n) {
							if (it == itEnd) {
								return tc::continue_if_not_break(sink, tc::tie(*tc_move_always(tc::at(ait, n)), *tc_move_always(tc::at(ait, (n + i + 1) % N))...)); // MAYTHROW
							}
							tc_return_if_break(tc::continue_if_not_break(sink, tc::tie(*tc_move_always(tc::at(ait, n)), *tc::at(ait, (n + i + 1) % N)...))) // MAYTHROW
							tc::at(ait, n) = it;
							++it;
						}
					}
				};
				return tc_conditional_prvalue_as_val(
					tc::size_bounded(self.base_range(), N)<N,
					tc::constant<tc::continue_>(),
					GenerateAdjacentTuples() // MAYTHROW
				);
			} else {
				return tc::for_each(
					tc_move_if_owned(self).base_range(),
					std::ref(tc::as_lvalue(no_adl::adjacent_adaptor_generator_base_sink<
						tc::decay_t<Sink>,
						tc::range_value_t<decltype(std::declval<Self>().base_range())>,
						i...
					>{tc_move(sink)}))
				);
			}
		}
	}

	namespace adjacent_adaptor_adl {
		template<typename Rng, std::size_t N, bool HasIterator = tc::range_with_iterators<Rng>>
		struct adjacent_adaptor;

		template<typename Rng, std::size_t N>
		struct [[nodiscard]] adjacent_adaptor<Rng, N, false> : tc::range_adaptor_base_range<Rng> {
			static_assert( 1 < N );
			using tc::range_adaptor_base_range<Rng>::range_adaptor_base_range;

			template<tc::decayed_derived_from<adjacent_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				return adjacent_adaptor_detail::adjacent_adaptor_for_each(
					tc_move_if_owned(self),
					tc_move_if_owned(sink),
					std::make_index_sequence<N - (tc::range_with_iterators<Rng> ? 1 : 2)>()
				);
			}

			[[nodiscard]] constexpr auto size() const& MAYTHROW requires tc::has_size<Rng> {
				return tc::compute_range_adaptor_size<[]<typename SizeT>(SizeT const n) noexcept {
					if( n < N ) {
						return tc::explicit_cast<SizeT>(0);
					} else {
						return tc::explicit_cast<SizeT>(n - (N - 1));
					}
				}>(this->base_range());
			}

		private:
			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, adjacent_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> boost::mp11::mp_list<
				boost::mp11::mp_repeat_c<
					tc::tuple<boost::mp11::mp_apply<
						tc::common_reference_t,
						tc::mp_transform<
							std::add_rvalue_reference_t,
							tc::range_output_t<decltype(std::declval<Self>().base_range())>
						>
					>>,
					N
				>
			> {} // unevaluated
		};
	}

	namespace adjacent_adaptor_detail::no_adl {
		template<typename Rng, std::size_t N>
		struct adjacent_index {
			constexpr adjacent_index() = default;
			template<std::size_t... I>
			constexpr adjacent_index(Rng const& rng, index_t<std::remove_reference_t<Rng>> idx, std::index_sequence<I...>) noexcept
				: m_aidx{idx, ((tc::at_end_index(rng, idx) ? tc::discard(I) : tc::increment_index(rng, idx)), idx)...}
			{
				STATICASSERTSAME(std::index_sequence<I...>, std::make_index_sequence<N - 1>);
			}

			// If size(rng) < N, the single index has m_aidx = {begin_index(), ..., end_index(), ... end_index()}.
			tc::index_t<std::remove_reference_t<Rng>> m_aidx[N];

			friend constexpr bool operator==(adjacent_index const& lhs, adjacent_index const& rhs) noexcept {
				return lhs.m_aidx[0] == rhs.m_aidx[0];
			}
		};
	}

	namespace adjacent_adaptor_adl {
		template<typename Rng, std::size_t N>
		struct [[nodiscard]] adjacent_adaptor<Rng, N, true>
			: adjacent_adaptor<Rng, N, false>
			, tc::range_iterator_from_index<
				adjacent_adaptor<Rng, N, true>,
				adjacent_adaptor_detail::no_adl::adjacent_index<Rng, N>
			>
		{
		private:
			using this_type = adjacent_adaptor<Rng, N, true>;

		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex=std::disjunction<tc::has_stashing_index<std::remove_reference_t<Rng>>>::value;
			static constexpr bool c_bPrefersForEach = true;

			using adjacent_adaptor<Rng, N, false>::adjacent_adaptor;

		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return {this->base_range(), this->base_begin_index(), std::make_index_sequence<N - 1>()};
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(), idx.m_aidx[N - 1]);
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index
				requires
					tc::has_end_index<std::remove_reference_t<Rng>> &&
					tc::has_decrement_index<std::remove_reference_t<Rng>>
			{
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
				constexpr,
				decrement_index
			)(tc_index& idx) const& noexcept -> void requires tc::has_decrement_index<std::remove_reference_t<Rng>> {
				// std::ranges::move_backward(tc::end_prev<tc::return_take>(idx.m_aidx), tc::end(idx.m_aidx));
				for( std::size_t i = N - 1; 0 < i; ) {
					--i;
					idx.m_aidx[i + 1] = tc_move_always(idx.m_aidx[i]);
				}
				tc::decrement_index(this->base_range(), idx.m_aidx[0]);
			}

			template<std::size_t... I>
			static constexpr auto dereference_index_impl(auto& self, auto&& idx, std::index_sequence<I...>) MAYTHROW
				-> tc::tuple<decltype(tc::dereference_index(self.base_range(), tc_move_if_owned(idx).m_aidx[I]))...>
			{
				return {{
					{ tc::dereference_index(self.base_range(), tc_move_if_owned(idx).m_aidx[I]) }...
				}};
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(auto&& idx) const& MAYTHROW {
				return dereference_index_impl(*this, tc_move_if_owned(idx), std::make_index_sequence<N>());
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(auto&& idx) & MAYTHROW {
				return dereference_index_impl(*this, tc_move_if_owned(idx), std::make_index_sequence<N>());
			}

			STATIC_FINAL_MOD(
				constexpr,
				advance_index
			)(tc_index& idx, typename boost::range_difference<Rng>::type d) const& MAYTHROW -> void requires tc::has_advance_index<std::remove_reference_t<Rng>> {
				idx = {this->base_range(), tc_modified(idx.m_aidx[0], tc::advance_index(this->base_range(), _, d)), std::make_index_sequence<N - 1>()};
			}

			STATIC_FINAL_MOD(
				constexpr,
				distance_to_index
			)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept requires tc::has_distance_to_index<std::remove_reference_t<Rng>> {
				return tc::distance_to_index(this->base_range(), idxLhs.m_aidx[0], idxRhs.m_aidx[0]);
			}

			STATIC_FINAL_MOD(
				constexpr,
				middle_point
			)(tc_index& idxLhs, tc_index const& idxRhs) const& noexcept -> void requires tc::has_middle_point<std::remove_reference_t<Rng>> {
				tc::middle_point(this->base_range(), idxLhs.m_aidx[0], idxRhs.m_aidx[0]);
				idxLhs = {this->base_range(), idxLhs.m_aidx[0], std::make_index_sequence<N - 1>()};
			}
		};
	}

	template<typename Rng, std::size_t N>
	constexpr auto enable_stable_index_on_move<adjacent_adaptor_adl::adjacent_adaptor<Rng, N, true>> = tc::stable_index_on_move<Rng>;

	template<std::size_t N, typename Rng>
	constexpr adjacent_adaptor_adl::adjacent_adaptor<Rng, N> adjacent(Rng&& rng) noexcept {
		return {tc::aggregate_tag, tc_move_if_owned(rng)};
	}
}
