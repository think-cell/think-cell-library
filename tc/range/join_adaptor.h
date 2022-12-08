
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
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

namespace tc {
	void join();
	
	DEFINE_FN(size_linear_raw)

	namespace no_adl {
		namespace join_adaptor_detail {
			template <typename RngRng>
			struct is_joinable_with_iterators : tc::constant<false> {};

			template <typename RngRng> requires tc::is_range_with_iterators<RngRng>::value
			struct is_joinable_with_iterators<RngRng> : tc::constant<
				tc::is_range_with_iterators<tc::iter_reference_t<tc::iterator_t<RngRng>>>::value &&
				std::is_lvalue_reference<tc::iter_reference_t<tc::iterator_t<RngRng>>>::value
			> {};

			template<typename RngRng>
			using rng_constexpr_size = tc::constexpr_size<tc::type::apply_t<
				tc::common_reference_xvalue_as_ref_t,
				tc::type::transform_t<
					tc::range_output_t<RngRng>,
					std::add_rvalue_reference_t
				>
			>>;
		}

		template<typename RngRng, bool bHasIterator = join_adaptor_detail::is_joinable_with_iterators<RngRng>::value>
		struct join_adaptor;

		template<typename Sink, bool bReverse>
		struct join_sink;

		template<typename Sink>
		struct join_sink<Sink, /*bReverse*/false> {
			static_assert(tc::decayed<Sink>);
			Sink m_sink;

			template<typename Rng>
			constexpr auto operator()(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::for_each(std::forward<Rng>(rng), m_sink)
			)

			template<typename SubRngRng, ENABLE_SFINAE>
			auto chunk(SubRngRng&& rngrng) const& return_decltype_MAYTHROW(
				SFINAE_VALUE(m_sink).chunk(tc::join(std::forward<SubRngRng>(rngrng)))
			)
		};

		template<typename Sink>
		struct join_sink<Sink, /*bReverse*/true> {
			static_assert(tc::decayed<Sink>);
			Sink m_sink;

			template<typename Rng>
			auto operator()(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::for_each(tc::reverse(std::forward<Rng>(rng)), m_sink)
			)
		};

		template<typename RngRng>
		struct [[nodiscard]] join_adaptor<RngRng, false> : tc::generator_range_adaptor<RngRng> {
			constexpr join_adaptor() = default;
			using tc::generator_range_adaptor<RngRng>::generator_range_adaptor;

			template<typename Sink, bool bReverse>
			static constexpr auto adapted_sink(Sink&& sink, tc::constant<bReverse>) noexcept {
				return join_sink<tc::decay_t<Sink>, bReverse>{std::forward<Sink>(sink)};
			}

			template<ENABLE_SFINAE>
			constexpr auto size() const& return_decltype_noexcept(
				// Use tc::mul and boost::multiprecision::number?
				tc::size_raw(SFINAE_VALUE(this)->base_range()) * join_adaptor_detail::rng_constexpr_size<decltype(SFINAE_VALUE(this)->base_range())>::value
			)

			template<ENABLE_SFINAE>
			constexpr auto size_linear() const& return_decltype_noexcept(
				tc::accumulate(tc::transform(SFINAE_VALUE(this)->base_range(), tc::fn_size_linear_raw(), tc::explicit_cast<std::size_t>(0), tc::fn_assign_plus()))
			)

			template<typename Self, std::enable_if_t<tc::is_base_of_decayed<join_adaptor, Self>::value>* = nullptr>
			friend auto range_output_t_impl(Self&&) -> tc::type::unique_t<tc::type::join_t<tc::type::transform_t<tc::range_output_t<decltype(std::declval<Self>().base_range())>, tc::range_output_t>>> {} // unevaluated
			
		};

		template<typename RngRng>
		struct [[nodiscard]] join_adaptor<RngRng, true> :
			join_adaptor<RngRng, false>,
			tc::range_iterator_from_index<
				join_adaptor<RngRng>,
				tc::tuple<
					tc::index_t<std::remove_reference_t<RngRng>>,
					tc::index_t<std::remove_reference_t<tc::iter_reference_t<tc::iterator_t<RngRng>>>>
				>
			>
		{
		private:
			using this_type = join_adaptor;
		public:
			using typename this_type::range_iterator_from_index::tc_index;

			static_assert(!tc::has_stashing_index<std::remove_reference_t<RngRng>>::value,
				"RngRgn must not have \"stashing\" index/iterator: copying the composite index would invalidate the inner index/iterator."
			);
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<tc::iter_reference_t<tc::iterator_t<RngRng>>>>::value;
		private:
			tc::index_t<std::remove_reference_t<tc::iter_reference_t<tc::iterator_t<RngRng>>>> find_valid_index(tc::index_t<std::remove_reference_t<RngRng>>& idxFirst) const& noexcept {
				while (!tc::at_end_index(this->base_range(), idxFirst)) {
					auto& rngSecond = tc::dereference_index(this->base_range_best_access(), idxFirst);
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
				auto_cref(rngSecond, tc::dereference_index(this->base_range(), tc::get<0>(idx)));
				tc::increment_index(rngSecond, tc::get<1>(idx));
				if (tc::at_end_index(rngSecond, tc::get<1>(idx))) {
					tc::increment_index(this->base_range(), tc::get<0>(idx));
					tc::get<1>(idx) = find_valid_index(tc::get<0>(idx));
				}
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& noexcept -> void {
				auto const funcReverseFindValidIndex = [&]() noexcept {
					for (;;) {
						tc::decrement_index(this->base_range(), tc::get<0>(idx));
						auto& rngSecond = tc::dereference_index(this->base_range_best_access(), tc::get<0>(idx));
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
					auto_cref(rngSecond, tc::dereference_index(this->base_range(), tc::get<0>(idx)));
					if(tc::begin_index(rngSecond) == tc::get<1>(idx)) {
						funcReverseFindValidIndex();
					} else {
						tc::decrement_index(rngSecond, tc::get<1>(idx));
					}
				}
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) const& noexcept -> decltype(auto) {
				return tc::dereference_index(tc::dereference_index(this->base_range(), tc::get<0>(idx)), tc::get<1>(idx));
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) & noexcept -> decltype(auto) {
				return tc::dereference_index(tc::dereference_index(this->base_range(), tc::get<0>(idx)), tc::get<1>(idx));
			}

			static constexpr auto element_base_index(tc_index const& idx) noexcept {
				return tc::get<0>(idx);
			}
		};

		template<typename RngRng>
		struct constexpr_size_base<join_adaptor<RngRng>, std::void_t<typename tc::constexpr_size<RngRng>::type, typename join_adaptor_detail::rng_constexpr_size<decltype(std::declval<join_adaptor<RngRng> const&>().base_range())>::type>>
			: tc::constant<tc::constexpr_size<RngRng>::value * join_adaptor_detail::rng_constexpr_size<decltype(std::declval<join_adaptor<RngRng> const&>().base_range())>::value
			>
		{};
	}
	using no_adl::join_adaptor;

	namespace join_default {
		template<typename RngRng>
		constexpr join_adaptor<RngRng> join_impl(RngRng&& rngrng) noexcept {
			return {aggregate_tag, std::forward<RngRng>(rngrng)};
		}
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(join)

	namespace no_adl {
		template<typename RngRng>
		struct is_index_valid_for_move_constructed_range<join_adaptor<RngRng, true>> : tc::is_index_valid_for_move_constructed_range<RngRng> {};
	}
}
