//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "compare.h"

namespace tc {

	namespace intersection_difference_adaptor_adl_barrier {
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
				reference_or_value< index_range_t<Rng0> >,
				reference_or_value< index_range_t<Rng1> >
			> m_baserng;

			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp>
			explicit intersection_difference_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp&& comp) noexcept
				: m_baserng(
					reference_or_value< index_range_t<Rng0> >(aggregate_tag(), std::forward<Rhs0>(rhs0)),
					reference_or_value< index_range_t<Rng1> >(aggregate_tag(), std::forward<Rhs1>(rhs1))
				),
				m_comp(std::forward<Comp>(comp))
			{}

		private:
			template<typename Func>
			struct FForwardFirstArgOnly final {
				Func& m_func;
				
				FForwardFirstArgOnly(Func& func) noexcept : m_func(func)
				{}

				template<typename T0, typename T1>
				tc::break_or_continue operator()(T0&& arg0, T1&&) const& MAYTHROW {
					return tc::continue_if_not_break(m_func, std::forward<T0>(arg0));
				}

			};

		public:
			template< typename Func >
			auto operator()(Func func) const& MAYTHROW -> break_or_continue
			{
#pragma warning ( push )
#pragma warning( disable: 4127 ) // conditional expression is constant
				if (bIntersection) {
					return tc::interleave(
						boost::implicit_cast<std::remove_reference_t<Rng0> const&>(*std::get<0>(m_baserng)),
						boost::implicit_cast<std::remove_reference_t<Rng1> const&>(*std::get<1>(m_baserng)),
						std::ref(m_comp),
						MAKE_CONSTEXPR_FUNCTION(tc::continue_),
						MAKE_CONSTEXPR_FUNCTION(tc::continue_),
						FForwardFirstArgOnly<Func>(func)
					);
				} else {
					return tc::interleave(
						boost::implicit_cast<std::remove_reference_t<Rng0> const&>(*std::get<0>(m_baserng)),
						boost::implicit_cast<std::remove_reference_t<Rng1> const&>(*std::get<1>(m_baserng)),
						std::ref(m_comp),
						std::ref(func),
						MAKE_CONSTEXPR_FUNCTION(tc::continue_),
						MAKE_CONSTEXPR_FUNCTION(tc::continue_)
					);
				}
#pragma warning ( pop )
			}
		};
	}

	using intersection_difference_adaptor_adl_barrier::intersection_difference_adaptor;

	template<typename Rng0, typename Rng1, typename Comp>
	auto intersect(Rng0&& rng0, Rng1&& rng1, Comp&& comp) noexcept return_ctor(
		intersection_difference_adaptor< true BOOST_PP_COMMA() tc::decay_t<Comp> BOOST_PP_COMMA() view_by_value_t<Rng0> BOOST_PP_COMMA() view_by_value_t<Rng1>>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto intersect(Rng0&& rng0, Rng1&& rng1) noexcept return_decltype(
		intersect(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto difference(Rng0&& rng0, Rng1&& rng1, Comp&& comp) noexcept return_ctor(
		intersection_difference_adaptor< false BOOST_PP_COMMA() tc::decay_t<Comp> BOOST_PP_COMMA() view_by_value_t<Rng0> BOOST_PP_COMMA() view_by_value_t<Rng1>>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto difference(Rng0&& rng0, Rng1&& rng1) noexcept return_decltype(
		difference(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

}