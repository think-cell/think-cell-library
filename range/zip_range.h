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

#include "range_adaptor.h"
#include "functors.h"

#include <boost/range/iterator_range.hpp>
#include <boost/fusion/adapted/std_tuple.hpp>
#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/detail/minimum_category.hpp>

namespace tc {

	namespace zip_adaptor_adl_barrier {

		template<typename... Ranges>
		struct zip_adaptor
			: range_iterator_generator_from_index<
				zip_adaptor<Ranges...>,
				std::tuple<
					typename std::remove_reference_t<index_range_t<Ranges>>::index...
				>,
				tc::demote_iterator_traversal_tag_t<traversal_t<Ranges>...>
			>
		{
		private:
			using this_type = zip_adaptor;
		public:

			template<typename... Rhs>
			zip_adaptor(aggregate_tag, Rhs&& ...rhs) noexcept :
				m_tplrng(tc::reference_or_value<index_range_t<Ranges>>(aggregate_tag(), std::forward<Rhs>(rhs))...)
			{}

			using index = typename this_type::index;

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return begin_index_impl(std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return end_index_impl(std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return std::get<0>(m_tplrng)->at_end_index(std::get<0>(idx))
#ifdef _CHECKS
					&& all_at_end_index(idx, std::index_sequence_for<Ranges...>{})
#endif
				;
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				increment_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				decrement_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			using dereference_type = std::tuple<
				typename range_traits<Ranges>::reference...
			>;

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept -> dereference_type {
				return dereference_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept -> dereference_type {
				return dereference_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return std::get<0>(m_tplrng)->equal_index(std::get<0>(idxLhs), std::get<0>(idxRhs));
			}

			// TODO: difference_type should be the smallest type of range_difference_type<Ranges,traversal_t<Ranges>>...
			using difference_type = int;// std::ptrdiff_t;

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const& noexcept -> void {
				advance_index_impl(idx, d, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type {
				return tc::numeric_cast<difference_type>(std::get<0>(m_tplrng)->distance_to_index(std::get<0>(idxLhs), std::get<0>(idxRhs)));
			}

		private:
			template<std::size_t... Is>
			auto begin_index_impl(std::index_sequence<Is...>) const& noexcept -> index {
				return std::make_tuple(std::get<Is>(m_tplrng)->begin_index()...);
			}

			template<std::size_t... Is>
			auto end_index_impl(std::index_sequence<Is...>) const& noexcept -> index {
				return std::make_tuple(std::get<Is>(m_tplrng)->end_index()...);
			}

			template<std::size_t... Is>
			void increment_index_impl(index& idx, std::index_sequence<Is...>) const& noexcept {
				using swallow = int[];
				swallow{(std::get<Is>(m_tplrng)->increment_index(std::get<Is>(idx)) ,0)...};
			}

			template<std::size_t... Is>
			void decrement_index_impl(index& idx, std::index_sequence<Is...>) const& noexcept {
				using swallow = int[];
				swallow{(std::get<Is>(m_tplrng)->decrement_index(std::get<Is>(idx)) ,0)...};
			}

			template<std::size_t... Is>
			auto dereference_index_impl(index const& idx, std::index_sequence<Is...>) const& noexcept -> dereference_type {
				return dereference_type{
					std::get<Is>(m_tplrng)->dereference_index(std::get<Is>(idx))...
				};
			}

			template<std::size_t... Is>
			auto dereference_index_impl(index const& idx, std::index_sequence<Is...>) & noexcept -> dereference_type {
				return dereference_type{
					std::get<Is>(m_tplrng)->dereference_index(std::get<Is>(idx))...
				};
			}

			template<std::size_t... Is>
			auto advance_index_impl(index& idx, difference_type d, std::index_sequence<Is...>) const& noexcept -> void {
				using swallow = int[];
				swallow{(std::get<Is>(m_tplrng)->advance_index(std::get<Is>(idx), d) ,0)...};
			}

#ifdef _CHECKS
			bool all_at_end_index(index const& idx, std::index_sequence<>) const& noexcept {
				return true;
			}

			template<std::size_t N, std::size_t... Is>
			bool all_at_end_index(index const& idx, std::index_sequence<N, Is...>) const& noexcept {
				return std::get<N>(m_tplrng)->at_end_index(std::get<N>(idx)) && all_at_end_index(idx, std::index_sequence<Is...>{});
			}
#endif

			std::tuple<tc::reference_or_value<tc::index_range_t<Ranges>>...> m_tplrng;
		};
	}

	using zip_adaptor_adl_barrier::zip_adaptor;

	template<typename... Ranges>
	auto zip(Ranges&& ...ranges) noexcept return_ctor(
		zip_adaptor<view_by_value_t<Ranges>...>,
		(aggregate_tag() BOOST_PP_COMMA() std::forward<Ranges>(ranges)...)
	)
}
