
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
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
#include "accumulate.h"
#include "transform.h"
#include "counting_range.h"
#include "variant.h"

namespace tc {
	namespace no_adl {
		template<typename TypeListRng, typename Enable=void>
		struct has_concat_iterator final: std::false_type {};

		template<typename TypeListRng>
		struct has_concat_iterator<TypeListRng, std::enable_if_t<tc::type::all_of<TypeListRng, tc::is_range_with_iterators>::value>> final : std::integral_constant<bool,
			tc::has_common_reference_prvalue_as_val<tc::type::transform_t<TypeListRng, tc::range_reference_t>>::value
		> {};
	}

	namespace concat_adaptor_adl {
		template<
			bool HasIterator,
			typename... Rng
		>
		struct concat_adaptor_impl;
	}

	template<typename... Rng>
	using concat_adaptor = concat_adaptor_adl::concat_adaptor_impl< no_adl::has_concat_iterator<tc::type::list<Rng...>>::value, Rng... >;

	namespace concat_adaptor_adl {
		template<
			typename... Rng
		>
		struct [[nodiscard]] concat_adaptor_impl<false, Rng...>: tc::value_type_base<tc::concat_adaptor<Rng...>> {
			std::tuple<
				tc::reference_or_value< Rng >...
			> m_baserng;

			template<typename... Rhs>
			constexpr concat_adaptor_impl(tc::aggregate_tag_t, Rhs&&... rhs) noexcept
				: m_baserng(
					tc::reference_or_value< Rng >(tc::aggregate_tag, std::forward<Rhs>(rhs))...
				)
			{}

			template< typename Func, typename Enable=tc::type::list<decltype(tc::for_each(*std::declval<tc::reference_or_value<Rng>&>(), std::declval<Func>()))...> >
			auto operator()(Func func) & MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*std::get<nconstIndex()>(m_baserng), func);
						}
					);
			}

			template< typename Func, typename Enable=tc::type::list<decltype(tc::for_each(*std::declval<tc::reference_or_value<Rng> const&>(), std::declval<Func>()))...> >
			auto operator()(Func func) const& MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*std::get<nconstIndex()>(m_baserng), func);
						}
					);
			}

			template< typename Func, typename Enable=tc::type::list<decltype(tc::for_each(*std::declval<tc::reference_or_value<Rng>&&>(), std::declval<Func>()))...> >
			auto operator()(Func func) && MAYTHROW {
				return 
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(*tc_move_always(std::get<nconstIndex()>(m_baserng)), func);
						}
					);
			}

			template<typename This, typename Func, std::void_t<decltype(tc::for_each(*std::declval<same_cvref_t<tc::reference_or_value<Rng>, This>>(), std::declval<Func>()))...>* = nullptr>
			static auto enumerate_reversed(This&& rngThis, Func&& func) MAYTHROW {
				return
					tc::for_each(
						std::index_sequence_for<Rng...>(),
						[&](auto nconstIndex) MAYTHROW {
							return tc::for_each(tc::reverse(*std::get<sizeof...(Rng) - 1 - nconstIndex()>(std::forward<This>(rngThis).m_baserng)), func);
						}
					);
			}

		protected:
			template<typename IntConstant>
			auto BaseRangeSize(IntConstant nconstIndex) const& noexcept {
				return tc::size_raw(*std::get<nconstIndex()>(this->m_baserng));
			}
		public:
			template< ENABLE_SFINAE, std::enable_if_t<std::conjunction<tc::has_size<SFINAE_TYPE(Rng)>...>::value>* = nullptr >
			auto size() const& noexcept {
				return 
					tc::accumulate(
						tc::transform(
							std::index_sequence_for<Rng...>(),
							[&](auto nconstIndex) noexcept { return BaseRangeSize(nconstIndex); }
						),
						boost::implicit_cast<tc::common_type_t<decltype(tc::size_raw(std::declval<Rng>()))...>>(0),
						fn_assign_plus()
					);
			}
		};

		struct concat_end_index final {};

		template<
			typename... Rng
		>
		struct [[nodiscard]] concat_adaptor_impl<true, Rng...> :
			concat_adaptor_impl<false, Rng...>,
			tc::range_iterator_from_index<
				concat_adaptor_impl<true, Rng...>,
				std::variant<
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
			constexpr concat_adaptor_impl(tc::aggregate_tag_t, Rhs&&... rhs) noexcept
				: concat_adaptor_impl<false, Rng...>(tc::aggregate_tag, std::forward<Rhs>(rhs)...)
			{}

		private:
			template<std::size_t Index>
			index create_begin_index(std::integral_constant<std::size_t, Index>) const& noexcept {
				static_assert(0 <= Index && Index <= sizeof...(Rng));
				if constexpr (sizeof...(Rng) == Index) {
					return index(std::in_place_index<Index>, concat_end_index());
				} else {
					return index(std::in_place_index<Index>, tc::begin_index(std::get<Index>(m_baserng)));
				}
			}

			template<int Index>
			index create_end_index(std::integral_constant<int, Index>) const& noexcept {
				static_assert(0 <= Index);
				static_assert(Index < sizeof...(Rng));
				return index(std::in_place_index<Index>, tc::end_index(std::get<Index>(m_baserng)));
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
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<std::conjunction<tc::has_equal_index<SFINAE_TYPE(Rng)>...>::value>* = nullptr
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
								tc::iota(lhs.index() + 1, rhs.index()),
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

	namespace no_adl {
		template<typename... Rng>
		struct value_type_base<tc::concat_adaptor_adl::concat_adaptor_impl<false, Rng...>, tc::void_t<tc::common_range_value_t<Rng...>>> {
			using value_type = tc::common_range_value_t<Rng...>;
		};
	}

	template<typename... Rng, std::enable_if_t<1<sizeof...(Rng)>* = nullptr>
	constexpr auto concat(Rng&&... rng) noexcept {
		return tc::concat_adaptor< std::remove_cv_t<Rng>...>(tc::aggregate_tag, std::forward<Rng>(rng)...);
	}

	namespace no_adl {
		template<typename Rng>
		struct is_concat_range final: std::false_type {};

		template<typename ... Rng>
		struct is_concat_range<tc::concat_adaptor<Rng ...>> final: std::true_type {};
	}
	using no_adl::is_concat_range;
}
