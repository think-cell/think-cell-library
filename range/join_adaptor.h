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

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "size.h"
#include "utility.h"
#include "invoke_with_constant.h"
#include "indexed_variant.h"
#include "algorithm.h"

namespace tc {
	namespace concat_adaptor_impl {
		template<
			bool HasIterator,
			typename... Rng
		>
		struct concat_adaptor_impl;

		template<typename... Rng>
		using concat_adaptor = concat_adaptor_impl< std::conjunction<tc::is_range_with_iterators<Rng>...>::value, Rng... >;

		template<
			typename... Rng
		>
		struct concat_adaptor_impl<false, Rng...> {
			std::tuple<
				tc::reference_or_value< tc::index_range_t<Rng> >...
			> m_baserng;

			template<typename... Rhs>
			explicit concat_adaptor_impl(Rhs&&... rhs) noexcept
				: m_baserng(
					tc::reference_or_value< tc::index_range_t<Rng> >(tc::aggregate_tag(), std::forward<Rhs>(rhs))...
				)
			{}

			template< typename Func >
			auto operator()(Func func) & MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*std::get<nconstIndex()>(m_baserng), std::ref(func));
						}
					);
			}

			template< typename Func >
			auto operator()(Func func) const& MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*std::get<nconstIndex()>(m_baserng), std::ref(func));
						}
					);
			}
		};

		template<
			typename... Rng
		>
		struct concat_adaptor_impl<true, Rng...> :
			concat_adaptor_impl<false, Rng...>,
			tc::range_iterator_from_index<
				concat_adaptor_impl<true, Rng...>,
				tc::indexed_variant<
					typename std::remove_reference_t<
						tc::index_range_t<Rng>
					>::index...
				>,
				tc::demote_iterator_traversal_tag_t<tc::traversal_t<Rng>...>
			>
		{
		private:
			using this_type = concat_adaptor_impl;
		public:
			using index = typename this_type::index;

			using concat_adaptor_impl<false, Rng...>::m_baserng;

			template<typename... Rhs>
			explicit concat_adaptor_impl(Rhs&&... rhs) noexcept
				: concat_adaptor_impl<false, Rng...>(std::forward<Rhs>(rhs)...)
			{}

		private:
			index create_begin_index(std::integral_constant<std::size_t, sizeof...(Rng)>) const& noexcept { _ASSERTFALSE; return {}; } // If this assertion fires, it means that an attempt to increment the end index or advance past it with positive distance has been attempted

			template<std::size_t Index>
			index create_begin_index(std::integral_constant<std::size_t, Index>) const& noexcept {
				return index(tc::in_place_index<Index>, std::get<Index>(m_baserng)->begin_index());
			}

			index create_end_index(std::integral_constant<int, -1>) const& noexcept { _ASSERTFALSE; return {}; } // If this assertion fires, it means that an attempt to decrement the begin index or advance past it with negative distance has been attempted

			template<int Index>
			index create_end_index(std::integral_constant<int, Index>) const& noexcept {
				return index(tc::in_place_index<Index>, std::get<Index>(m_baserng)->end_index());
			}

			template<int IndexFrom>
			index correct_index(index idx) const& noexcept {
				tc::for_each(
					tc::make_integer_sequence<std::size_t, IndexFrom, sizeof...(Rng) - 1>(), // if the index is at end index of the last range, there's nothing to do
					[&](auto nconstIndex) noexcept {
						if (std::get<nconstIndex()>(m_baserng)->at_end_index(tc::get<nconstIndex()>(idx))) {
							idx = create_begin_index(std::integral_constant<std::size_t, nconstIndex() + 1>());
							return tc::continue_;
						} else {
							return tc::break_;
						}
					}
				);

				return idx;
			}

		public:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return correct_index<0>(create_begin_index(std::integral_constant<std::size_t, 0>()));
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return create_end_index(std::integral_constant<int, sizeof...(Rng)-1>());
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return sizeof...(Rng) - 1 == idx.index() && std::get<sizeof...(Rng) - 1>(m_baserng)->at_end_index(tc::get<sizeof...(Rng) - 1>(idx));
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				_ASSERT(!this->at_end_index(idx));

				tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept { 
						std::get<nconstIndex()>(m_baserng)->increment_index(tc::get<nconstIndex()>(idx));
						idx = correct_index<nconstIndex()>(idx);
					},
					idx.index()
				);
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndexStart) noexcept {
						tc::for_each(
							tc::make_reverse_integer_sequence<int, 0, nconstIndexStart() + 1>(),
							[&](auto nconstIndex) noexcept {
								auto& idxCurrent = tc::get<nconstIndex()>(idx);
								if (std::get<nconstIndex()>(m_baserng)->equal_index(std::get<nconstIndex()>(m_baserng)->begin_index(), idxCurrent)) {
									idx = create_end_index(std::integral_constant<int, nconstIndex() - 1>());
									return tc::continue_;
								} else {
									std::get<nconstIndex()>(m_baserng)->decrement_index(idxCurrent);
									return tc::break_;
								}
								
							}
						);
					},
					idx.index()
				);
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept -> decltype(auto) {
				return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept  -> decltype(auto) { // return_decltype leads to ICE 
						return std::get<nconstIndex()>(m_baserng)->dereference_index(tc::get<nconstIndex()>(idx));
					},
					idx.index()
				);
			}

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept -> decltype(auto) {
				return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept -> decltype(auto) { // return_decltype leads to ICE 
						return std::get<nconstIndex()>(m_baserng)->dereference_index(tc::get<nconstIndex()>(idx));
					},
					idx.index()
				);
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				if ( idxLhs.index() != idxRhs.index() ) return false;

				return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept {
						return std::get<nconstIndex()>(m_baserng)->equal_index(tc::get<nconstIndex()>(idxLhs), tc::get<nconstIndex()>(idxRhs));
					},
					idxLhs.index()
				);
			}

			using difference_type = tc::common_type_t<
				tc::range_difference_type<Rng, tc::traversal_t<Rng>>...
			>;

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const& noexcept -> void {
				tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndexStart) noexcept {
						if (d < 0) {
							tc::for_each(
								tc::make_reverse_integer_sequence<int, 0, nconstIndexStart() + 1>(),
								[&](auto nconstIndex) noexcept {
									auto dToBegin = std::get<nconstIndex()>(m_baserng)->distance_to_index(
										tc::get<nconstIndex()>(idx),
										std::get<nconstIndex()>(m_baserng)->begin_index()
									);

									if (!(d < dToBegin)) {
										std::get<nconstIndex()>(m_baserng)->advance_index(tc::get<nconstIndex()>(idx), d);
										return tc::break_;
									} else {
										d -= dToBegin;
										idx = create_end_index(std::integral_constant<int, nconstIndex() - 1>());
										return tc::continue_;
									}
								}
							);
						} else {
							tc::for_each(
								tc::make_integer_sequence<std::size_t, nconstIndexStart(), sizeof...(Rng)>(),
								tc::make_overload<tc::break_or_continue>(
									[&](std::integral_constant<std::size_t, sizeof...(Rng)-1>) noexcept {
										std::get<sizeof...(Rng)-1>(m_baserng)->advance_index(tc::get<sizeof...(Rng)-1>(idx), d);
										return tc::break_;
									},
									[&](auto nconstIndex) noexcept {
										auto dToEnd = std::get<nconstIndex()>(m_baserng)->distance_to_index(
											tc::get<nconstIndex()>(idx),
											std::get<nconstIndex()>(m_baserng)->end_index()
										);

										if (d < dToEnd) {
											std::get<nconstIndex()>(m_baserng)->advance_index(tc::get<nconstIndex()>(idx), d);
											return tc::break_;
										} else {
											d -= dToEnd;
											idx = create_begin_index(std::integral_constant<std::size_t, nconstIndex() + 1>());
											return tc::continue_;
										}
									}
								)
							);
						}
					},
					idx.index()
				);
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type {
				if (idxLhs.index() == idxRhs.index()) {
					return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
						[&](auto nconstIndex) noexcept -> difference_type {
							return std::get<nconstIndex()>(m_baserng)->distance_to_index(tc::get<nconstIndex()>(idxLhs), tc::get<nconstIndex()>(idxRhs));
						},
						idxLhs.index()
					);
				} else {
					auto positive_distance = [&](index const& lhs, index const& rhs) noexcept {
						return tc::accumulate(
							tc::transform(
								tc::make_counting_range(lhs.index() + 1, rhs.index()),
								[&](auto nIndex) noexcept {
									return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
										[&](auto nconstIndex) noexcept { return tc::explicit_cast<difference_type>(BaseRangeSize(nconstIndex)); },
										nIndex
									);
								}
							),
							tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
								[&](auto nconstIndex) noexcept -> difference_type {
									return std::get<nconstIndex()>(m_baserng)->distance_to_index(tc::get<nconstIndex()>(lhs), std::get<nconstIndex()>(m_baserng)->end_index());
								},
								lhs.index()
							) + 
							tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
								[&](auto nconstIndex) noexcept -> difference_type {
									return std::get<nconstIndex()>(m_baserng)->distance_to_index(std::get<nconstIndex()>(m_baserng)->begin_index(), tc::get<nconstIndex()>(rhs));
								},
								rhs.index()
							),
							fn_assign_plus()
						);
					};

					if (idxLhs.index() < idxRhs.index()) {
						return positive_distance(idxLhs, idxRhs);
					} else {
						return -positive_distance(idxRhs, idxLhs);
					}
				}
			}

			template<typename IntConstant>
			auto BaseRangeSize(IntConstant nconstIndex) const& noexcept {
				return tc::size_impl::size(*std::get<nconstIndex()>(this->m_baserng));
			}

			template<typename... Rng>
			constexpr static std::conjunction<tc::size_impl::has_size<Rng>...> all_ranges_have_size(tc::type_list<Rng...>);

			template<typename TypeListRng>
			using all_ranges_have_size_t = decltype(all_ranges_have_size(TypeListRng())); // Cannot inline due to MSVC failing to compile "decltype(...)::value" (10/02/2017)

			template<typename TypeListRng = tc::type_list<tc::index_range_t<Rng>...>, std::enable_if_t<all_ranges_have_size_t<TypeListRng>::value>* = nullptr>
			auto size() const& noexcept {
				return 
					tc::accumulate(
						tc::transform(
							std::index_sequence_for<Rng...>(),
							[&](auto nconstIndex) noexcept { return BaseRangeSize(nconstIndex); }
						),
						tc::common_type_decayed_t<decltype(tc::size_impl::size(std::declval<tc::index_range_t<Rng> const&>()))...>(0),
						fn_assign_plus()
					);
			}
		};
	}

	using concat_adaptor_impl::concat_adaptor;

	namespace range_reference_adl_barrier {
		template<bool bConst, typename... Rng>
		struct range_reference_concat_adaptor {
			using type = tc::common_reference_t<
				reference_for_value_or_reference_with_index_range_t<Rng, bConst>...
			>;
		};

		template<typename... Rng>
		struct range_reference<concat_adaptor_impl::concat_adaptor_impl<false, Rng...>> : range_reference_concat_adaptor<false, Rng...> {};

		template<typename... Rng>
		struct range_reference<concat_adaptor_impl::concat_adaptor_impl<false, Rng...> const> : range_reference_concat_adaptor<true, Rng...> {};
	}

	template<typename... Rng>
	auto concat(Rng&&... rng) noexcept {
		static_assert(1 < sizeof...(Rng), "tc::concat can only be used with two or more ranges");
		return concat_adaptor< tc::view_by_value_t<Rng>...>(std::forward<Rng>(rng)...);
	}
}