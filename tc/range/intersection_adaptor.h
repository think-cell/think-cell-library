
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../algorithm/compare.h"
#include "../algorithm/algorithm.h"
#include "filter_adaptor.h"

namespace tc {
	namespace intersection_difference_adaptor_detail::no_adl {
		template<typename Sink, bool bIntersection, bool bSubset>
		struct intersection_difference_sink {
			Sink const m_sink;

			constexpr auto operator()(interleave_2_detail::lhs_tag_t, auto&& lhs) const& noexcept {
				if constexpr(!bIntersection) {
					return tc_invoke(m_sink, tc_move_if_owned(lhs));
				}
			}
			constexpr void operator()(interleave_2_detail::rhs_tag_t, tc::unused) const& noexcept {
				if constexpr(bSubset) {
					_ASSERTFALSE;
				}
			}
			constexpr auto operator()(interleave_2_detail::lhsrhs_tag_t, auto&& lhs, tc::unused) const& noexcept {
				if constexpr(bIntersection) {
					return tc_invoke(m_sink, tc_move_if_owned(lhs));
				}
			}
		};
	}

	namespace intersection_difference_adaptor_adl {
		template<
			bool bIntersection,
			bool bSubset,
			typename Comp,
			typename Rng0,
			typename Rng1
		>
		struct [[nodiscard]] intersection_difference_adaptor final
		{
		private:
			tc::tuple<
				tc::reference_or_value< Rng0 >,
				tc::reference_or_value< Rng1 >
			> m_tplbaserng;

			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp2>
			explicit intersection_difference_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp2&& comp) noexcept
				: m_tplbaserng{{
					{{aggregate_tag, tc_move_if_owned(rhs0)}},
					{{aggregate_tag, tc_move_if_owned(rhs1)}}
				}},
				m_comp(tc_move_if_owned(comp))
			{
				// For non-strictly sorted ranges, performs multiset intersect/difference, but the
				// meaning when the ranges are not sorted at all is unclear, though well-defined.
				// m_comp not always applicable on a single range:
				//	_ASSERTDEBUG(tc::is_sorted(*tc::get<0>(m_tplbaserng), tc::lessfrom3way(comp)));
				//	_ASSERTDEBUG(tc::is_sorted(*tc::get<1>(m_tplbaserng), tc::lessfrom3way(comp)));
			}

			template<tc::decayed_derived_from<intersection_difference_adaptor> Self, typename Sink>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				return interleave_2_detail::internal_interleave_2(
					*tc::get<0>(tc_move_if_owned(self).m_tplbaserng),
					*tc::get<1>(tc_move_if_owned(self).m_tplbaserng),
					tc_move_if_owned(self).m_comp,
					intersection_difference_adaptor_detail::no_adl::intersection_difference_sink<tc::decay_t<Sink>, bIntersection, bSubset>{tc_move_if_owned(sink)}
				);
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, intersection_difference_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> tc::range_output_t<decltype(*tc::get<0>(std::declval<Self>().m_tplbaserng))> {} // unevaluated
		};
	}
	using intersection_difference_adaptor_adl::intersection_difference_adaptor;

	template<bool bIntersection, typename Rng0, typename Rng1>
	auto set_intersect_or_difference(Rng0&& rng0, Rng1&& rng1) noexcept {
		static_assert(tc::instance<std::remove_reference_t<Rng1>, std::unordered_set>);
		return tc::filter(
			tc_move_if_owned(rng0),
			[rng1_ = tc::reference_or_value< Rng1 >(tc::aggregate_tag, tc_move_if_owned(rng1))](auto const& element) noexcept {
				if constexpr(bIntersection) {
					return tc::cont_find<tc::return_bool>(*rng1_, element);
				} else {
					return !tc::cont_find<tc::return_bool>(*rng1_, element);
				}
			}
		);
	}

	template<typename Rng0, typename Rng1>
	auto set_intersect(Rng0&& rng0, Rng1&& rng1) noexcept {
		return set_intersect_or_difference<true>(tc_move_if_owned(rng0), tc_move_if_owned(rng1));
	}

	template<typename Rng0, typename Rng1>
	auto set_difference(Rng0&& rng0, Rng1&& rng1) noexcept {
		return set_intersect_or_difference<false>(tc_move_if_owned(rng0), tc_move_if_owned(rng1));
	}

#pragma push_macro("DEFINE_INTERSECT_DIFFERENCE")
#define DEFINE_INTERSECT_DIFFERENCE(name, bIntersect, bSubset) \
	template<typename Rng0, typename Rng1, typename Comp = tc::fn_compare> \
	auto name(Rng0&& rng0, Rng1&& rng1, Comp&& comp = Comp()) return_ctor_noexcept( \
		TC_FWD(intersection_difference_adaptor<bIntersect, bSubset, tc::decay_t<Comp>, Rng0, Rng1>), \
		(tc_move_if_owned(rng0), tc_move_if_owned(rng1), tc_move_if_owned(comp)) \
	)

	DEFINE_INTERSECT_DIFFERENCE(intersect, true, false)
	DEFINE_INTERSECT_DIFFERENCE(subset_intersect, true, true)
	DEFINE_INTERSECT_DIFFERENCE(difference, false, false)
	DEFINE_INTERSECT_DIFFERENCE(subset_difference, false, true)
#pragma pop_macro("DEFINE_INTERSECT_DIFFERENCE")
}
