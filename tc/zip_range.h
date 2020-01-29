
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_adaptor.h"
#include "functors.h"
#include "utility.h"
#include "quantifier.h"
#include "range.h"

#include <boost/range/iterator_range.hpp>
#include <boost/fusion/adapted/std_tuple.hpp>
#include <boost/iterator/iterator_categories.hpp>
#include <boost/iterator/detail/minimum_category.hpp>

namespace tc {

	namespace no_adl {

		template<typename... Ranges>
		struct [[nodiscard]] zip_adaptor
			: range_iterator_generator_from_index<
				zip_adaptor<Ranges...>,
				std::tuple<
					typename tc::index_t<std::remove_reference_t<Ranges>>...
				>
			>
		{
		private:
			using this_type = zip_adaptor;
			using proxy = std::tuple<decltype(*tc::as_lvalue(tc::begin(*std::declval<tc::reference_or_value<Ranges>&>())))...>;
			using const_proxy = std::tuple<decltype(*tc::as_lvalue(tc::begin(*std::declval<tc::reference_or_value<Ranges> const&>())))...>;
		public:

			template<typename... Rhs>
			zip_adaptor(aggregate_tag_t, Rhs&& ...rhs) noexcept :
				m_baserng(tc::reference_or_value<Ranges>(aggregate_tag, std::forward<Rhs>(rhs))...)
			{}

			using index = typename this_type::index;

			// TODO: difference_type should be the smallest type of range_difference_type<Ranges,traversal_t<Ranges>>...
			using difference_type = int;// std::ptrdiff_t;

		private:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return std::apply([&](auto&&... baseranges) {
						return std::make_tuple(tc::begin_index(baseranges)...);
					},
					m_baserng
				);
			}

			STATIC_FINAL_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<SFINAE_VALUE(
						std::conjunction<tc::has_end_index<std::remove_reference_t<Ranges>>...>::value
					)>* = nullptr
				>,
			end_index)() const& noexcept -> index {
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

			STATIC_FINAL_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<SFINAE_VALUE(
						std::conjunction<tc::has_decrement_index<std::remove_reference_t<Ranges>>...>::value
					)>* = nullptr
				>,
			decrement_index)(index& idx) const& noexcept -> void {
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

			STATIC_FINAL_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<SFINAE_VALUE(
						std::conjunction<tc::has_advance_index<std::remove_reference_t<Ranges>>...>::value
					)>* = nullptr
				>,
			advance_index)(index& idx, difference_type d) const& noexcept -> void {
				tc::for_each(
					std::make_index_sequence<sizeof...(Ranges)>(),
					[&](auto nconstIndex) noexcept {
						tc::advance_index(*std::get<nconstIndex()>(m_baserng), std::get<nconstIndex()>(idx), d);
					}
				);
			}

			// For consistency with other functions, distance_to_index is only available if all base ranges support it - even though we only require one of the base ranges to support it.
			STATIC_FINAL_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<SFINAE_VALUE(
						std::conjunction<tc::has_distance_to_index<std::remove_reference_t<Ranges>>...>::value
					)>* = nullptr
				>,
			distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept->difference_type {
				return tc::explicit_cast<difference_type>(tc::distance_to_index(*std::get<0>(m_baserng), std::get<0>(idxLhs), std::get<0>(idxRhs)));
			}

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
	auto zip(Ranges&& ...ranges) return_ctor_noexcept(
		no_adl::zip_adaptor<Ranges...>,
		(aggregate_tag BOOST_PP_COMMA() std::forward<Ranges>(ranges)...)
	)

	/*
		TODO: RT#16520
		It is reasonable that the following use case of zip_ranges should work without copying
		the R-value range.

		if (auto o = tc::find_last<tc::return_value_or_none>(
			tc::zip_ranges( CreateRValueRngRng() ),
			predicate
		)) {
			*o; // must still be valid.
		}

		- Currently, *o is 'tc::transform(*rngrng, [n](auto const& rng) return_decltype_MAYTHROW(tc_at(rng, n)))'
		with rngrng beeing then out of scope.

		return_value_or_none.pack_element(It&&, Rng&&, Ref&& ref) is called with R-Value range, so in principle
		it can call (note && and tc_move)
		transform_adaptor::dereference_index() && {
			tc_move(m_func)(...)
		},
		and the transform functor of zip_ranges could overload for '&&' and in that case aggregating rngrng.

		This is not possible with find_last using iterators. Todo is then to
		- Specialize find_last (and similar functions) for index-based ranges
		- Introduce pack_element for index-based results
		- Introduce transform_adaptor::dereference_index &&
	*/
	template<typename RngRng>
	auto zip_ranges(RngRng&& rngrng) noexcept { // for random access ranges
		_ASSERT(tc::all_same(tc::transform(rngrng, tc::fn_size())));
		auto const n = tc::empty(rngrng) ? 0 : tc::size(tc_front(rngrng)); // Do not inline, function evaluation order undefined
		return tc::transform(
			tc::iota(0, n),
			[rngrng = tc::reference_or_value<RngRng>(tc::aggregate_tag, std::forward<RngRng>(rngrng))](auto const n) noexcept {
				return tc::transform(*rngrng, [n](auto const& rng) return_decltype_MAYTHROW(tc_at(rng, n)));
			}
		);
	}
}
