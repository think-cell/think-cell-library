
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "index_range.h"
#include "meta.h"
#include "types.h"
#include "size.h"
#include "utility.h"
#include "invoke_with_constant.h"
#include "indexed_variant.h"
#include "accumulate.h"
#include "transform.h"
#include "counting_range.h"

namespace tc {
	namespace no_adl {
		template<typename T, typename Enable=void>
		struct has_common_reference final: std::false_type {};

		template<typename ... T>
		struct has_common_reference<tc::type_list<T...>, tc::void_t<tc::common_reference_t<T...>>> final: std::true_type {};

		template<typename T, typename Enable=void>
		struct has_concat_iterator final: std::false_type {};

		template<typename ... Rng>
		struct has_concat_iterator<tc::type_list<Rng...>, std::enable_if_t<std::conjunction<tc::is_range_with_iterators<Rng>...>::value>> final: std::integral_constant<bool,
			has_common_reference<tc::type_list<tc::range_reference_t<Rng>...>>::value
		> {};

		template<
			bool HasIterator,
			typename... Rng
		>
		struct concat_adaptor_impl;

		template<typename... Rng>
		using concat_adaptor = concat_adaptor_impl< has_concat_iterator<tc::type_list<Rng...>>::value, Rng... >;

		template<
			typename... Rng
		>
		struct concat_adaptor_impl<false, Rng...> {
			std::tuple<
				tc::reference_or_value< Rng >...
			> m_baserng;

			template<typename... Rhs>
			constexpr explicit concat_adaptor_impl(Rhs&&... rhs) noexcept
				: m_baserng(
					tc::reference_or_value< Rng >(tc::aggregate_tag(), std::forward<Rhs>(rhs))...
				)
			{}

			template< typename Func >
			auto operator()(Func func) & MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*std::get<nconstIndex()>(m_baserng), func);
						}
					);
			}

			template< typename Func >
			auto operator()(Func func) const& MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*std::get<nconstIndex()>(m_baserng), func);
						}
					);
			}

			template< typename Func >
			auto operator()(Func func) && MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*tc_move_always(std::get<nconstIndex()>(m_baserng)), func);
						}
					);
			}
		protected:
			template<
				template<typename> typename has_trait,
				typename... Rng2
			>
			constexpr static std::conjunction<has_trait<Rng2>...> each_range(tc::type_list<Rng2...>);

			template<
				template<typename> typename has_trait,
				typename TypeListRng
			>
			using each_range_t = decltype(each_range<has_trait>(TypeListRng()));

			template<typename IntConstant>
			auto BaseRangeSize(IntConstant nconstIndex) const& noexcept {
				return tc::size_raw(*std::get<nconstIndex()>(this->m_baserng));
			}
		public:
			template<typename TypeListRng = tc::type_list<std::remove_reference_t<Rng>...>, std::enable_if_t<each_range_t<tc::has_size, TypeListRng>::value>* = nullptr>
			auto size() const& noexcept {
				return 
					tc::accumulate(
						tc::transform(
							std::index_sequence_for<Rng...>(),
							[&](auto nconstIndex) noexcept { return BaseRangeSize(nconstIndex); }
						),
						tc::common_type_decayed_t<decltype(tc::size_raw(std::declval<Rng const&>()))...>(0),
						fn_assign_plus()
					);
			}
		};

		struct concat_end_index final {};

		template<
			typename... Rng
		>
		struct concat_adaptor_impl<true, Rng...> :
			concat_adaptor_impl<false, Rng...>,
			tc::range_iterator_from_index<
				concat_adaptor_impl<true, Rng...>,
				tc::indexed_variant<
					tc::index_t<std::remove_reference_t<
						Rng
					>>...,
					concat_end_index
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
			constexpr explicit concat_adaptor_impl(Rhs&&... rhs) noexcept
				: concat_adaptor_impl<false, Rng...>(std::forward<Rhs>(rhs)...)
			{}

		private:
			template<std::size_t Index>
			index create_begin_index(std::integral_constant<std::size_t, Index>) const& noexcept {
				static_assert(0 <= Index && Index <= sizeof...(Rng));
				if constexpr (sizeof...(Rng) == Index) {
					return index(tc::in_place_index<Index>, concat_end_index());
				} else {
					return index(tc::in_place_index<Index>, tc::begin_index(std::get<Index>(m_baserng)));
				}
			}

			template<int Index>
			index create_end_index(std::integral_constant<int, Index>) const& noexcept {
				static_assert(0 <= Index);
				static_assert(Index < sizeof...(Rng));
				return index(tc::in_place_index<Index>, tc::end_index(std::get<Index>(m_baserng)));
			}

			template<int IndexFrom>
			void correct_index(index& idx) const& noexcept {
				tc::for_each(
					tc::make_integer_sequence<std::size_t, IndexFrom, sizeof...(Rng)>(),
					[&](auto nconstIndex) noexcept {
						if (tc::at_end_index( *std::get<nconstIndex()>(m_baserng), tc::get<nconstIndex()>(idx))) {
							idx = create_begin_index(std::integral_constant<std::size_t, nconstIndex() + 1>());
							return tc::continue_;
						} else {
							return tc::break_;
						}
					}
				);
			}

		public:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return modified(
					create_begin_index(std::integral_constant<std::size_t, 0>()),
					correct_index<0>(_)
				);
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return create_begin_index(std::integral_constant<std::size_t, sizeof...(Rng)>());
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return sizeof...(Rng) == idx.index();
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				_ASSERT(!this->at_end_index(idx));

				tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept { 
						tc::increment_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idx));
						correct_index<nconstIndex()>(idx);
					},
					idx.index()
				);
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
					[&](auto nconstIndexStart) noexcept {
						tc::for_each(
							tc::make_reverse_integer_sequence<int, 0, nconstIndexStart() + 1>(),
							[&](auto nconstIndex) noexcept {
								if constexpr (sizeof...(Rng) == nconstIndex()) {
									 idx = create_end_index(std::integral_constant<int, nconstIndex() - 1>());
									 return tc::continue_;
								} else {
									auto& idxCurrent = tc::get<nconstIndex()>(idx);
									if constexpr (0 == nconstIndex()) {
										_ASSERT(!tc::equal_index(*std::get<0>(m_baserng),tc::begin_index(std::get<0>(m_baserng)), idxCurrent));
									} else if (tc::equal_index(*std::get<nconstIndex()>(m_baserng),tc::begin_index(std::get<nconstIndex()>(m_baserng)), idxCurrent)) {
										idx = create_end_index(std::integral_constant<int, nconstIndex() - 1>());
										return tc::continue_;
									}
									// Remember early out above
									tc::decrement_index(*std::get<nconstIndex()>(m_baserng),idxCurrent);
									return tc::break_;
								}
							}
						);
					},
					idx.index()
				);
			}

#pragma warning (suppress: 4544) // 'Func2': default template argument ignored on this template declaration
			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept -> decltype(auto) {
				return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept  -> decltype(auto) { // return_decltype leads to ICE 
						return tc::dereference_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idx));
					},
					idx.index()
				);
			}

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept -> decltype(auto) {
				return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept -> decltype(auto) { // return_decltype leads to ICE 
						return tc::dereference_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idx));
					},
					idx.index()
				);
			}

			STATIC_FINAL_MOD(
				template<
					typename TypeListRng = tc::type_list<std::remove_reference_t<Rng>...> BOOST_PP_COMMA()
					std::enable_if_t<concat_adaptor_impl<false BOOST_PP_COMMA() Rng...>::template each_range_t<tc::has_equal_index BOOST_PP_COMMA() TypeListRng>::value>* = nullptr
				>,
				equal_index
			)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				if ( idxLhs.index() != idxRhs.index() ) return false;

				return tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
					[&](auto nconstIndex) noexcept {
						if constexpr (sizeof...(Rng) == nconstIndex()) {
							return true;
						} else {
							return tc::equal_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idxLhs), tc::get<nconstIndex()>(idxRhs));
						}
					},
					idxLhs.index()
				);
			}

			using difference_type = std::ptrdiff_t ; /* TODO :tc::common_type_t<
				boost::range_difference<Rng>::type ...
			>;*/

			STATIC_FINAL(advance_index)(index& idx, difference_type d) const& noexcept -> void {
				tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
					[&](auto nconstIndexStart) noexcept {
						if (d < 0) {
							tc::for_each(
								tc::make_reverse_integer_sequence<int, 0, nconstIndexStart() + 1>(),
								[&](auto nconstIndex) noexcept {
									if constexpr (sizeof...(Rng) == nconstIndex()) {
										idx = create_end_index(std::integral_constant<int, nconstIndex() - 1>());
										return tc::continue_;
									} else {
										if constexpr(0 == nconstIndex()) {
											tc::advance_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idx), d);
											return tc::break_;
										} else {
											auto dToBegin = tc::distance_to_index(*std::get<nconstIndex()>(m_baserng),
												tc::get<nconstIndex()>(idx),
												tc::begin_index(std::get<nconstIndex()>(m_baserng))
											);

											if (!(d < dToBegin)) {
												tc::advance_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idx), d);
												return tc::break_;
											} else {
												d -= dToBegin;
												idx = create_end_index(std::integral_constant<int, nconstIndex() - 1>());
												return tc::continue_;
											}
										}
									}
								}
							);
						} else {
							tc::for_each(
								tc::make_integer_sequence<std::size_t, nconstIndexStart(), sizeof...(Rng)>(),
								[&](auto nconstIndex) noexcept {
									if constexpr (nconstIndex() == sizeof...(Rng)-1) {
										tc::advance_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idx), d);
										correct_index<nconstIndex()>(idx);
										return tc::break_;
									} else {
										auto dToEnd = tc::distance_to_index(*std::get<nconstIndex()>(m_baserng),
											tc::get<nconstIndex()>(idx),
											tc::end_index(std::get<nconstIndex()>(m_baserng))
										);
										if (d < dToEnd) {
											tc::advance_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idx), d);
											return tc::break_;
										} else {
											d -= dToEnd;
											idx = create_begin_index(std::integral_constant<std::size_t, nconstIndex() + 1>());
											return tc::continue_;
										}
									}
								}
							);
						}
					},
					idx.index()
				);
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type {
				if (idxLhs.index() == idxRhs.index()) {
					return tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
						[&](auto nconstIndex) noexcept -> difference_type {
							if constexpr (nconstIndex() == sizeof...(Rng)) {
								return 0;
							} else {
								return tc::distance_to_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(idxLhs), tc::get<nconstIndex()>(idxRhs));
							}
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
										[&](auto nconstIndex) noexcept { return tc::explicit_cast<difference_type>(this->BaseRangeSize(nconstIndex)); },
										nIndex
									);
								}
							),
							tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
								[&](auto nconstIndex) noexcept -> difference_type {
									return tc::distance_to_index(*std::get<nconstIndex()>(m_baserng),tc::get<nconstIndex()>(lhs), tc::end_index(std::get<nconstIndex()>(m_baserng)));
								},
								lhs.index()
							) + 
							tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
								[&](auto nconstIndex) noexcept -> difference_type {
									if constexpr(nconstIndex() == sizeof...(Rng)) {
										return 0;
									} else {
										return tc::distance_to_index(*std::get<nconstIndex()>(m_baserng),tc::begin_index(std::get<nconstIndex()>(m_baserng)), tc::get<nconstIndex()>(rhs));
									}
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
		};
	}

	using no_adl::concat_adaptor;

	namespace no_adl {
		template<bool bConst, typename... Rng>
		struct range_reference_concat_adaptor {
			using type = tc::common_reference_t<
				reference_for_value_or_reference_with_index_range_t<Rng, bConst>...
			>;
		};

		template<typename... Rng>
		struct range_reference<concat_adaptor_impl<false, Rng...>> : range_reference_concat_adaptor<false, Rng...> {};

		template<typename... Rng>
		struct range_reference<concat_adaptor_impl<false, Rng...> const> : range_reference_concat_adaptor<true, Rng...> {};
	}

	template<typename... Rng>
	constexpr auto concat(Rng&&... rng) noexcept {
		static_assert(1 < sizeof...(Rng), "tc::concat can only be used with two or more ranges");
		return tc::concat_adaptor< Rng...>(std::forward<Rng>(rng)...);
	}

	namespace no_adl {
		template< typename Rng >
		struct is_concat_range final: std::false_type {};

		template<bool b, typename ... Rng>
		struct is_concat_range<tc::no_adl::concat_adaptor_impl<b, Rng ...>> final: std::true_type {};
	}
	using no_adl::is_concat_range;
}
