
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_adaptor.h"
#include "functors.h"
#include "utility.h"
#include "quantifier.h"

#include <boost/range/iterator_range.hpp>
#include <boost/fusion/adapted/std_tuple.hpp>
#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/detail/minimum_category.hpp>

namespace tc {

	namespace no_adl {

		template<typename... Ranges>
		struct zip_adaptor
			: range_iterator_generator_from_index<
				zip_adaptor<Ranges...>,
				std::tuple<
					typename tc::index_t<std::remove_reference_t<Ranges>>...
				>,
				tc::demote_iterator_traversal_tag_t<traversal_t<Ranges>...>
			>
		{
		private:
			using this_type = zip_adaptor;
			using proxy = std::tuple<decltype(*tc::begin(*std::declval<tc::reference_or_value<Ranges>&>()))...>;
			using const_proxy = std::tuple<decltype(*tc::begin(*std::declval<tc::reference_or_value<Ranges> const&>()))...>;
		public:

			template<typename... Rhs>
			zip_adaptor(aggregate_tag, Rhs&& ...rhs) noexcept :
				m_baserng(tc::reference_or_value<Ranges>(aggregate_tag(), std::forward<Rhs>(rhs))...)
			{}

			using index = typename this_type::index;

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return std::apply([&](auto&&... baseranges) {
						return std::make_tuple(tc::begin_index(baseranges)...);
					},
					m_baserng
				);
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return std::apply([&](auto&&... baseranges) {
						return std::make_tuple(tc::end_index(baseranges)...);
					},
					m_baserng
				);
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				auto MemberRangeAtEnd = 
					[&](auto nconstIndex) noexcept {
						return tc::at_end_index(*std::get<nconstIndex()>(m_baserng), std::get<nconstIndex()>(idx));
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
						tc::increment_index(*std::get<nconstIndex()>(m_baserng), std::get<nconstIndex()>(idx));
					}
				);
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				tc::for_each(
					std::make_index_sequence<sizeof...(Ranges)>(),
					[&](auto nconstIndex) noexcept {
						tc::decrement_index(*std::get<nconstIndex()>(m_baserng), std::get<nconstIndex()>(idx));
					}
				);
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept -> const_proxy {
				return dereference_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept -> proxy {
				return dereference_index_impl(idx, std::index_sequence_for<Ranges...>{});
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::equal_index(*std::get<0>(m_baserng), std::get<0>(idxLhs), std::get<0>(idxRhs));
			}

			// TODO: difference_type should be the smallest type of range_difference_type<Ranges,traversal_t<Ranges>>...
			using difference_type = int;// std::ptrdiff_t;

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const& noexcept -> void {
				tc::for_each(
					std::make_index_sequence<sizeof...(Ranges)>(),
					[&](auto nconstIndex) noexcept {
						tc::advance_index(*std::get<nconstIndex()>(m_baserng), std::get<nconstIndex()>(idx), d);
					}
				);
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type {
				return tc::explicit_cast<difference_type>(tc::distance_to_index(*std::get<0>(m_baserng), std::get<0>(idxLhs), std::get<0>(idxRhs)));
			}

		private:
			template<std::size_t... Is>
			const_proxy dereference_index_impl(index const& idx, std::index_sequence<Is...>) const& noexcept {
				return {tc::dereference_index(*std::get<Is>(m_baserng), std::get<Is>(idx))...};
			}

			template<std::size_t... Is>
			proxy dereference_index_impl(index const& idx, std::index_sequence<Is...>) & noexcept {
				return {tc::dereference_index(*std::get<Is>(m_baserng), std::get<Is>(idx))...};
			}

			std::tuple<tc::reference_or_value<Ranges>...> m_baserng;
		};
	}

	template<typename... Ranges>
	auto zip(Ranges&& ...ranges) noexcept return_ctor(
		no_adl::zip_adaptor<Ranges...>,
		(aggregate_tag() BOOST_PP_COMMA() std::forward<Ranges>(ranges)...)
	)
}
