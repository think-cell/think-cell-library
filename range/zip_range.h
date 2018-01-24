//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
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
#include "utility.h"

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
				auto MemberRangeAtEnd = 
					[&](auto nconstIndex) noexcept {
						return std::get<nconstIndex()>(m_tplrng)->at_end_index(std::get<nconstIndex()>(idx));
					};

				bool const bAtEnd = MemberRangeAtEnd(std::integral_constant<std::size_t, 0>());

				_ASSERT(tc::all_of(
					tc::make_integer_sequence<std::size_t, 1, sizeof...(Ranges)>(),
					[&](auto nconstIndex) noexcept { return MemberRangeAtEnd(nconstIndex) == bAtEnd; }
				));

				return bAtEnd;
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				tc::for_each(
					std::make_index_sequence<sizeof...(Ranges)>(),
					[&](auto nconstIndex) noexcept {
						std::get<nconstIndex()>(m_tplrng)->increment_index(std::get<nconstIndex()>(idx));
					}
				);
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				tc::for_each(
					std::make_index_sequence<sizeof...(Ranges)>(),
					[&](auto nconstIndex) noexcept {
						std::get<nconstIndex()>(m_tplrng)->decrement_index(std::get<nconstIndex()>(idx));
					}
				);
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept -> decltype(auto) {
				return dereference_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept -> decltype(auto) {
				return dereference_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return std::get<0>(m_tplrng)->equal_index(std::get<0>(idxLhs), std::get<0>(idxRhs));
			}

			// TODO: difference_type should be the smallest type of range_difference_type<Ranges,traversal_t<Ranges>>...
			using difference_type = int;// std::ptrdiff_t;

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const& noexcept -> void {
				tc::for_each(
					std::make_index_sequence<sizeof...(Ranges)>(),
					[&](auto nconstIndex) noexcept {
						std::get<nconstIndex()>(m_tplrng)->advance_index(std::get<nconstIndex()>(idx), d);
					}
				);
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type {
				return tc::explicit_cast<difference_type>(std::get<0>(m_tplrng)->distance_to_index(std::get<0>(idxLhs), std::get<0>(idxRhs)));
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
			auto dereference_index_impl(index const& idx, std::index_sequence<Is...>) const& noexcept {
				return std::tuple<decltype(std::get<Is>(m_tplrng)->dereference_index(std::get<Is>(idx)))...>(
					std::get<Is>(m_tplrng)->dereference_index(std::get<Is>(idx))...
				);
			}

			template<std::size_t... Is>
			auto dereference_index_impl(index const& idx, std::index_sequence<Is...>) & noexcept {
				return std::tuple<decltype(std::get<Is>(m_tplrng)->dereference_index(std::get<Is>(idx)))...>(
					std::get<Is>(m_tplrng)->dereference_index(std::get<Is>(idx))...
				);
			}

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
