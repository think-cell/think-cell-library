
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "compare.h"
#include "algorithm.h"
#include "filter_adaptor.h"

namespace tc {
	namespace no_adl {
		template<
			bool bIntersection,
			typename Comp,
			typename Rng0,
			typename Rng1
		>
		struct intersection_difference_adaptor final
		{
		private:
			std::tuple<
				reference_or_value< Rng0 >,
				reference_or_value< Rng1 >
			> m_baserng;

			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp2>
			explicit intersection_difference_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp2&& comp) noexcept
				: m_baserng(
					reference_or_value< Rng0 >(aggregate_tag(), std::forward<Rhs0>(rhs0)),
					reference_or_value< Rng1 >(aggregate_tag(), std::forward<Rhs1>(rhs1))
				),
				m_comp(std::forward<Comp2>(comp))
			{}

		private:
			template<typename Func>
			struct FForwardFirstArgOnly final {
				Func& m_func;
				
				FForwardFirstArgOnly(Func& func) noexcept : m_func(func)
				{}

				template<typename T0, typename T1>
				auto operator()(T0&& arg0, T1&&) const& MAYTHROW {
					return tc::continue_if_not_break(m_func, std::forward<T0>(arg0));
				}

			};

		public:
			template< typename Func >
			auto operator()(Func func) const& MAYTHROW
			{
				if constexpr (bIntersection) {
					return tc::interleave_2(
						*std::get<0>(m_baserng),
						*std::get<1>(m_baserng),
						std::ref(m_comp),
						tc::noop(),
						tc::noop(),
						FForwardFirstArgOnly<Func>(func)
					);
				} else {
					return tc::interleave_2(
						*std::get<0>(m_baserng),
						*std::get<1>(m_baserng),
						std::ref(m_comp),
						std::ref(func),
						tc::noop(),
						tc::noop()
					);
				}
			}
		};
	}

	using no_adl::intersection_difference_adaptor;

	template<bool bIntersection, typename Rng0, typename Rng1>
	auto set_intersect_or_difference(Rng0&& rng0, Rng1&& rng1) noexcept {
		static_assert(tc::is_instance<std::unordered_set, std::remove_reference_t<Rng1>>::value);
		return tc::filter(
			std::forward<Rng0>(rng0),
			[rng1_ = reference_or_value< Rng1 >(tc::aggregate_tag(), std::forward<Rng1>(rng1))](auto const& element) noexcept {
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
	auto intersect(Rng0&& rng0, Rng1&& rng1, Comp&& comp) noexcept return_ctor(
		intersection_difference_adaptor< true BOOST_PP_COMMA() tc::decay_t<Comp> BOOST_PP_COMMA() Rng0 BOOST_PP_COMMA() Rng1>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto intersect(Rng0&& rng0, Rng1&& rng1) noexcept return_decltype(
		intersect(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto difference(Rng0&& rng0, Rng1&& rng1, Comp&& comp) noexcept return_ctor(
		intersection_difference_adaptor< false BOOST_PP_COMMA() tc::decay_t<Comp> BOOST_PP_COMMA() Rng0 BOOST_PP_COMMA() Rng1>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto difference(Rng0&& rng0, Rng1&& rng1) noexcept return_decltype(
		difference(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

}
