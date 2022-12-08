
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../algorithm/compare.h"
#include "../algorithm/algorithm.h"
#include "range_fwd.h"
#include "filter_adaptor.h"

namespace tc {
	namespace intersection_difference_adaptor_adl {
		template<
			bool bIntersection,
			typename Comp,
			typename Rng0,
			typename Rng1
		>
		struct [[nodiscard]] intersection_difference_adaptor final
		{
		private:
			tc::tuple<
				reference_or_value< Rng0 >,
				reference_or_value< Rng1 >
			> m_tplbaserng;

			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp2>
			explicit intersection_difference_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp2&& comp) noexcept
				: m_tplbaserng{{
					{{aggregate_tag, std::forward<Rhs0>(rhs0)}},
					{{aggregate_tag, std::forward<Rhs1>(rhs1)}}
				}},
				m_comp(std::forward<Comp2>(comp))
			{
				// For non-strictly sorted ranges, performs multiset intersect/difference, but the
				// meaning when the ranges are not sorted at all is unclear, though well-defined.
				// m_comp not always applicable on a single range:
				//	_ASSERTDEBUG(tc::is_sorted(*tc::get<0>(m_tplbaserng), tc::lessfrom3way(comp)));
				//	_ASSERTDEBUG(tc::is_sorted(*tc::get<1>(m_tplbaserng), tc::lessfrom3way(comp)));
			}

		private:
			template<typename Sink>
			struct FForwardFirstArgOnly final {
				Sink m_sink;
				
				template<typename T0, typename T1>
				auto operator()(T0&& arg0, T1&&) const& MAYTHROW {
					return tc::continue_if_not_break(m_sink, std::forward<T0>(arg0));
				}
			};

		public:
			template<typename Self, typename Sink> requires
				tc::is_base_of_decayed<intersection_difference_adaptor, Self>::value
				&& tc::is_range_with_iterators<Rng1>::value
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				if constexpr (bIntersection) {
					return tc::interleave_2(
						*tc::get<0>(std::forward<Self>(self).m_tplbaserng),
						*tc::get<1>(std::forward<Self>(self).m_tplbaserng),
						std::forward<Self>(self).m_comp,
						tc::noop(),
						tc::noop(),
						FForwardFirstArgOnly<tc::decay_t<Sink>>{std::forward<Sink>(sink)}
					);
				} else {
					return tc::interleave_2(
						*tc::get<0>(std::forward<Self>(self).m_tplbaserng),
						*tc::get<1>(std::forward<Self>(self).m_tplbaserng),
						std::forward<Self>(self).m_comp,
						std::forward<Sink>(sink),
						tc::noop(),
						tc::noop()
					);
				}
			}

			template<typename Self, typename Sink> requires
				tc::is_base_of_decayed<intersection_difference_adaptor, Self>::value
				&& tc::is_range_with_iterators<Rng0>::value && (!tc::is_range_with_iterators<Rng1>::value)
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				auto const predReverse = [pred=tc::decay_copy(std::forward<Self>(self).m_comp)](auto const& lhs, auto const& rhs) noexcept {
					return tc::negate(pred(rhs, lhs));
				};
				if constexpr (bIntersection) {
					return tc::interleave_2(
						*tc::get<1>(std::forward<Self>(self).m_tplbaserng),
						*tc::get<0>(std::forward<Self>(self).m_tplbaserng),
						predReverse,
						tc::noop(),
						tc::noop(),
						FForwardFirstArgOnly<tc::decay_t<Sink>>{std::forward<Sink>(sink)}
					);
				} else {
					return tc::interleave_2(
						*tc::get<1>(std::forward<Self>(self).m_tplbaserng),
						*tc::get<0>(std::forward<Self>(self).m_tplbaserng),
						predReverse,
						tc::noop(),
						std::forward<Sink>(sink),
						tc::noop()
					);
				}
			}

			template<typename Self, std::enable_if_t<tc::is_base_of_decayed<intersection_difference_adaptor, Self>::value>* = nullptr>
			friend auto range_output_t_impl(Self&&) -> tc::range_output_t<decltype(*tc::get<0>(std::declval<Self>().m_tplbaserng))> {} // unevaluated
		};
	}
	using intersection_difference_adaptor_adl::intersection_difference_adaptor;

	template<bool bIntersection, typename Rng0, typename Rng1>
	auto set_intersect_or_difference(Rng0&& rng0, Rng1&& rng1) noexcept {
		static_assert(tc::is_instance<std::unordered_set, std::remove_reference_t<Rng1>>::value);
		return tc::filter(
			std::forward<Rng0>(rng0),
			[rng1_ = reference_or_value< Rng1 >(tc::aggregate_tag, std::forward<Rng1>(rng1))](auto const& element) noexcept {
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
		return set_intersect_or_difference<true>(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1));
	}

	template<typename Rng0, typename Rng1>
	auto set_difference(Rng0&& rng0, Rng1&& rng1) noexcept {
		return set_intersect_or_difference<false>(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1));
	}

	template<typename Rng0, typename Rng1, typename Comp>
	auto intersect(Rng0&& rng0, Rng1&& rng1, Comp&& comp) return_ctor_noexcept(
		TC_FWD(intersection_difference_adaptor<true, tc::decay_t<Comp>, Rng0, Rng1>),
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto intersect(Rng0&& rng0, Rng1&& rng1) return_decltype_noexcept(
		intersect(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), tc::fn_compare())
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto difference(Rng0&& rng0, Rng1&& rng1, Comp&& comp) return_ctor_noexcept(
		TC_FWD(intersection_difference_adaptor<false, tc::decay_t<Comp>, Rng0, Rng1>),
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto difference(Rng0&& rng0, Rng1&& rng1) return_decltype_noexcept(
		difference(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), tc::fn_compare())
	)

}
