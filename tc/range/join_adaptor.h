
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/modified.h"
#include "../algorithm/accumulate.h"
#include "../algorithm/size_linear.h"
#include "range_adaptor.h"
#include "transform_adaptor.h"
#include "reverse_adaptor.h"

namespace tc {
	DECLARE_TMPL_FUNC_WITH_CUSTOMIZATIONS(join)
	
	tc_define_fn(size_linear_raw)

	namespace no_adl {
		namespace join_adaptor_detail {
			template<typename RngRng>
			concept joinable_with_iterators = tc::range_with_iterators<RngRng>
				&& !tc::has_stashing_index<std::remove_reference_t<RngRng>>::value // RngRgn must not have "stashing" index/iterator: copying the composite index would invalidate the inner index/iterator.
				&& tc::borrowed_range<std::iter_reference_t<tc::iterator_t<RngRng>>>;

			template <typename RngRng, typename CommonOutput = boost::mp11::mp_apply<
				tc::common_reference_t,
				tc::mp_transform<
					std::add_rvalue_reference_t,
					tc::range_output_t<RngRng>
				>>
			>
				requires tc::has_constexpr_size<CommonOutput>
			auto constexpr rng_constexpr_size = tc::constexpr_size<CommonOutput>;
		}

		template<typename RngRng, bool bHasIterator = join_adaptor_detail::joinable_with_iterators<RngRng>>
		struct join_adaptor;

		template<typename Sink, bool bReverse>
		struct join_sink;

		template<typename Sink>
		struct join_sink<Sink, /*bReverse*/false> {
			static_assert(tc::decayed<Sink>);
			Sink m_sink;

			template<typename Rng>
			constexpr auto operator()(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::for_each(tc_move_if_owned(rng), m_sink)
			)

			template<typename SubRngRng, ENABLE_SFINAE>
			auto chunk(SubRngRng&& rngrng) const& return_decltype_MAYTHROW(
				SFINAE_VALUE(m_sink).chunk(tc::join(tc_move_if_owned(rngrng)))
			)
		};

		template<typename Sink>
		struct join_sink<Sink, /*bReverse*/true> {
			static_assert(tc::decayed<Sink>);
			Sink m_sink;

			template<typename Rng>
			auto operator()(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::for_each(tc::reverse(tc_move_if_owned(rng)), m_sink)
			)
		};

		template<typename RngRng>
		struct [[nodiscard]] join_adaptor<RngRng, false> : tc::generator_range_adaptor<RngRng> {
			constexpr join_adaptor() = default;
			using tc::generator_range_adaptor<RngRng>::generator_range_adaptor;

			template<typename Sink, bool bReverse>
			static constexpr auto adapted_sink(Sink&& sink, tc::constant<bReverse>) noexcept {
				return join_sink<tc::decay_t<Sink>, bReverse>{tc_move_if_owned(sink)};
			}

			constexpr auto size() const& MAYTHROW
				requires tc::has_size<RngRng> && requires(join_adaptor const& join) { join_adaptor_detail::rng_constexpr_size<decltype(join.base_range())>; } {
				return tc::compute_range_adaptor_size<[](auto const n) noexcept {
					return tc::as_unsigned(n * join_adaptor_detail::rng_constexpr_size<decltype(std::declval<join_adaptor>().base_range())>());
				}>(this->base_range());
			}

			template<ENABLE_SFINAE>
			constexpr auto size_linear() const& return_decltype_noexcept(
				tc::accumulate(tc::transform(SFINAE_VALUE(this)->base_range(), tc::fn_size_linear_raw(), tc::explicit_cast<std::size_t>(0), tc::fn_assign_plus()))
			)

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, join_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> boost::mp11::mp_unique<boost::mp11::mp_flatten<tc::mp_transform<tc::range_output_t, tc::range_output_t<decltype(std::declval<Self>().base_range())>>>> {} // unevaluated
			
		};

		template<typename RngRng>
		struct [[nodiscard]] join_adaptor<RngRng, true> :
			join_adaptor<RngRng, false>,
			tc::range_iterator_from_index<
				join_adaptor<RngRng>,
				tc::tuple<
					tc::index_t<std::remove_reference_t<RngRng>>,
					tc::index_t<std::remove_reference_t<std::iter_reference_t<tc::iterator_t<RngRng>>>>
				>
			>
		{
		private:
			using this_type = join_adaptor;
		public:
			using typename this_type::range_iterator_from_index::tc_index;

			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<std::iter_reference_t<tc::iterator_t<RngRng>>>>::value;
			static constexpr bool c_bPrefersForEach = true;
		private:
			tc::index_t<std::remove_reference_t<std::iter_reference_t<tc::iterator_t<RngRng>>>> find_valid_index(tc::index_t<std::remove_reference_t<RngRng>>& idxFirst) const& noexcept {
				while (!tc::at_end_index(this->base_range(), idxFirst)) {
					decltype(auto) rngSecond = tc::dereference_index(this->base_range_best_access(), idxFirst);
					auto idxSecond = tc::begin_index(rngSecond);
					if (tc::at_end_index(rngSecond, idxSecond)) {
						tc::increment_index(this->base_range(), idxFirst);
					} else {
						return idxSecond;
					}
				}
				return {};
			}
		public:
			constexpr join_adaptor() = default;
			using join_adaptor<RngRng, false>::join_adaptor;

			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				auto idxFirst = this->base_begin_index();
				auto idxSecond = find_valid_index(idxFirst);
				return { tc_move(idxFirst), tc_move(idxSecond) };
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				return { this->base_end_index(), {} };
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return tc::at_end_index(this->base_range(), tc::get<0>(idx));
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				_ASSERT(!tc::at_end_index(this->base_range(), tc::get<0>(idx)));
				tc_auto_cref(rngSecond, tc::dereference_index(this->base_range(), tc::get<0>(idx)));
				tc::increment_index(rngSecond, tc::get<1>(idx));
				if (tc::at_end_index(rngSecond, tc::get<1>(idx))) {
					tc::increment_index(this->base_range(), tc::get<0>(idx));
					tc::get<1>(idx) = find_valid_index(tc::get<0>(idx));
				}
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& noexcept -> void
				requires tc::has_decrement_index<std::remove_reference_t<RngRng>>
					&& tc::has_end_index<std::remove_reference_t<std::iter_reference_t<tc::iterator_t<RngRng>>>>
					&& tc::has_decrement_index<std::remove_reference_t<std::iter_reference_t<tc::iterator_t<RngRng>>>>
			{
				auto const funcReverseFindValidIndex = [&]() noexcept {
					for (;;) {
						tc::decrement_index(this->base_range(), tc::get<0>(idx));
						decltype(auto) rngSecond = tc::dereference_index(this->base_range_best_access(), tc::get<0>(idx));
						tc::get<1>(idx) = tc::end_index(rngSecond);
						if (tc::begin_index(rngSecond) != tc::get<1>(idx)) {
							tc::decrement_index(rngSecond, tc::get<1>(idx));
							break;
						}
					}
				};
				if(tc::at_end_index(this->base_range(), tc::get<0>(idx))) {
					funcReverseFindValidIndex();
				} else {
					tc_auto_cref(rngSecond, tc::dereference_index(this->base_range(), tc::get<0>(idx)));
					if(tc::begin_index(rngSecond) == tc::get<1>(idx)) {
						funcReverseFindValidIndex();
					} else {
						tc::decrement_index(rngSecond, tc::get<1>(idx));
					}
				}
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(auto&& idx) const& MAYTHROW -> decltype(auto) {
				return tc::dereference_index(
					tc::dereference_index(this->base_range(), tc::get<0>(tc_move_if_owned(idx))),
					tc::get<1>(tc_move_if_owned(idx))
				);
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(auto&& idx) & MAYTHROW -> decltype(auto) {
				return tc::dereference_index(
					tc::dereference_index(this->base_range(), tc::get<0>(tc_move_if_owned(idx))),
					tc::get<1>(tc_move_if_owned(idx))
				);
			}

			static constexpr auto element_base_index(tc_index const& idx) noexcept {
				return tc::get<0>(idx);
			}
		};
	}
	using no_adl::join_adaptor;

	template<typename RngRng>
	constexpr auto enable_stable_index_on_move<join_adaptor<RngRng, true>> = tc::stable_index_on_move<RngRng>;

	namespace join_default {
		template<typename RngRng>
		constexpr join_adaptor<RngRng> join_impl(RngRng&& rngrng) noexcept {
			return {aggregate_tag, tc_move_if_owned(rngrng)};
		}
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(join)
}
