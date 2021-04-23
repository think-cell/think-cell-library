
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "range_fwd.h"
#include "compare.h"
#include "algorithm.h"
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
			> m_baserng;

			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp2>
			explicit intersection_difference_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp2&& comp) noexcept
				: m_baserng{{
					{{aggregate_tag, std::forward<Rhs0>(rhs0)}},
					{{aggregate_tag, std::forward<Rhs1>(rhs1)}}
				}},
				m_comp(std::forward<Comp2>(comp))
			{}

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
			template<typename Self, typename Sink, std::enable_if_t<tc::is_base_of_decayed<intersection_difference_adaptor, Self>::value>* = nullptr>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				if constexpr (bIntersection) {
					return tc::interleave_2(
						*tc::get<0>(std::forward<Self>(self).m_baserng),
						*tc::get<1>(std::forward<Self>(self).m_baserng),
						std::forward<Self>(self).m_comp,
						tc::noop(),
						tc::noop(),
						FForwardFirstArgOnly<tc::decay_t<Sink>>{std::forward<Sink>(sink)}
					);
				} else {
					return tc::interleave_2(
						*tc::get<0>(std::forward<Self>(self).m_baserng),
						*tc::get<1>(std::forward<Self>(self).m_baserng),
						std::forward<Self>(self).m_comp,
						std::forward<Sink>(sink),
						tc::noop(),
						tc::noop()
					);
				}
			}
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
		intersection_difference_adaptor< true BOOST_PP_COMMA() tc::decay_t<Comp> BOOST_PP_COMMA() Rng0 BOOST_PP_COMMA() Rng1>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto intersect(Rng0&& rng0, Rng1&& rng1) return_decltype_noexcept(
		intersect(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto difference(Rng0&& rng0, Rng1&& rng1, Comp&& comp) return_ctor_noexcept(
		intersection_difference_adaptor< false BOOST_PP_COMMA() tc::decay_t<Comp> BOOST_PP_COMMA() Rng0 BOOST_PP_COMMA() Rng1>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto difference(Rng0&& rng0, Rng1&& rng1) return_decltype_noexcept(
		difference(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

}
