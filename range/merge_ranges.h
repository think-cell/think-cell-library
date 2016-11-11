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

#include "algorithm.h"
#include "as_lvalue.h"
#include "zip_range.h"

namespace tc {

	namespace merge_many_adaptor_adl_barrier {

		struct MakeReferenceOrValue final {
			template<typename T>
			auto operator()(T&& t) const {
				return tc::reference_or_value<T>(aggregate_tag(), std::forward<T>(t));
			}
		};

		template<typename Rng, std::enable_if_t<!std::is_reference< typename boost::range_reference<Rng>::type >::value>* =nullptr>
		auto make_cached(Rng const& rng) {
			return tc::make_vector(
				tc::transform(
					tc::transform(
						rng,
						MakeReferenceOrValue()
					),
					tc::fn_indirection()
				)
			);
		}

		template<typename Rng, std::enable_if_t<std::is_reference< typename boost::range_reference<std::remove_reference_t<Rng>>::type >::value>* =nullptr>
		auto make_cached(Rng&& rng) -> Rng&& {
			return std::forward<Rng>(rng);
		}

		template<typename RngRng, typename Pred>
		struct merge_many_adaptor {
		private:
			tc::reference_or_value<RngRng> m_baserng;
			Pred m_pred;

		public:
			template<typename Rhs, typename PredRhs>
			explicit merge_many_adaptor(aggregate_tag, Rhs&& rhs, PredRhs&& pred) noexcept
				: m_baserng( aggregate_tag(), std::forward<Rhs>(rhs) )
				, m_pred(std::forward<PredRhs>(pred))
			{}

			template< typename Func >
			tc::break_or_continue operator()(Func func) const& MAYTHROW {
				auto vecrngrng = tc::make_vector(
					tc::transform(
						*m_baserng,
						tc::fn_make_range()
					)
				);

				for (;;) {
					auto it = tc::best_element<tc::return_element_or_singleton>(
						tc::as_lvalue(tc::filter( // as_lvalue -> yield non-const iterator
							vecrngrng,
							tc::not_fn(tc::fn_empty())
						)),
						projected_front(m_pred)
					).element_base();

					if (!it) break;

					if( break_==continue_if_not_break(func, tc_front(*it)) ) return break_;
					tc::drop_first_inplace(*it);
				}
				return tc::continue_;
			}

		};

		template<typename RngRng, typename Pred>
		auto merge_many_impl(RngRng&& rngrng, Pred&& pred) noexcept return_ctor(
			merge_many_adaptor<view_by_value_t<RngRng> BOOST_PP_COMMA() Pred>,
			(aggregate_tag(), std::forward<RngRng>(rngrng) BOOST_PP_COMMA() std::forward<Pred>(pred))
		)
	}

	template<typename RngRng, typename Pred>
	auto merge_many(RngRng&& rngrng, Pred&& pred) {
		return merge_many_adaptor_adl_barrier::merge_many_impl(
			merge_many_adaptor_adl_barrier::make_cached(std::forward<RngRng>(rngrng)),
			std::forward<Pred>(pred)
		);
	}

	template<typename RngRng>
	auto merge_many(RngRng&& rngrng) {
		return merge_many(std::forward<RngRng>(rngrng), tc::fn_less());
	}

}
