#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "../compare.h"

namespace RANGE_PROPOSAL_NAMESPACE {

	namespace intersection_difference_adaptor_adl_barrier {
		template<
			bool bIntersection,
			typename Comp,
			typename Rng0,
			typename Rng1
		>
		struct intersection_difference_adaptor
		{
		private:
			std::tuple<
				reference_or_value< typename index_range<Rng0>::type >,
				reference_or_value< typename index_range<Rng1>::type >
			> m_baserng;

			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp>
			intersection_difference_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp&& comp)
				: m_baserng(
					reference_or_value< typename index_range<Rng0>::type >(std::forward<Rhs0>(rhs0), aggregate_tag()),
					reference_or_value< typename index_range<Rng1>::type >(std::forward<Rhs1>(rhs1), aggregate_tag())
				),
				m_comp(std::forward<Comp>(comp))
			{}

		private:
			template<typename Func>
			struct FForwardFirstArgOnly {
				Func& m_func;
				
				FForwardFirstArgOnly(Func& func) : m_func(func)
				{}

				template<typename T0, typename T1>
				tc::break_or_continue operator()(T0&& arg0, T1&&) const {
					return tc::continue_if_not_break(m_func, std::forward<T0>(arg0));
				}

			};

		public:
			template< typename Func >
			auto operator()(Func func) const -> break_or_continue
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
	auto intersect(Rng0&& rng0, Rng1&& rng1, Comp&& comp) return_ctor(
		intersection_difference_adaptor< true BOOST_PP_COMMA() typename std::decay<Comp>::type BOOST_PP_COMMA() typename range_by_value<Rng0>::type BOOST_PP_COMMA() typename range_by_value<Rng1>::type>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto intersect(Rng0&& rng0, Rng1&& rng1) return_decltype(
		intersect(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto difference(Rng0&& rng0, Rng1&& rng1, Comp&& comp) return_ctor(
		intersection_difference_adaptor< false BOOST_PP_COMMA() typename std::decay<Comp>::type BOOST_PP_COMMA() typename range_by_value<Rng0>::type BOOST_PP_COMMA() typename range_by_value<Rng1>::type>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto difference(Rng0&& rng0, Rng1&& rng1) return_decltype(
		difference(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), fn_compare())
	)

}