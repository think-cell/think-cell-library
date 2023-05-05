
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/utility.h"
#include "../base/invoke_with_constant.h"

#include "../algorithm/size.h"
#include "../algorithm/accumulate.h"
#include "../variant.h"
#include "../algorithm/quantifier.h"
#include "../algorithm/empty.h"
#include "../tuple.h"

#include "range_fwd.h"
#include "range_adaptor.h"
#include "index_range.h"
#include "meta.h"
#include "transform.h"
#include "iota_range.h"
#include "transform_adaptor.h"

namespace tc {
	namespace concat_adaptor_adl {
		template<bool HasIterator, typename... Rng>
		struct concat_adaptor_impl;
	}

	template<typename... Rng>
	concept concatenable_with_iterators = tc::has_common_reference_prvalue_as_val<std::iter_reference_t<tc::iterator_t<Rng>>...>;

	template<typename... Rng>
	using concat_adaptor = concat_adaptor_adl::concat_adaptor_impl<concatenable_with_iterators<Rng...>, Rng...>;

	namespace no_adl {
		template<typename Rng>
		struct is_concat_range final: tc::constant<false> {};

		template<typename... Rng>
		struct is_concat_range<tc::concat_adaptor<Rng ...>> final: tc::constant<true> {};
	}
	using no_adl::is_concat_range;

	namespace concat_detail {
		namespace no_adl {
			template<typename ConcatAdaptor, typename Sink, typename = std::remove_cvref_t<ConcatAdaptor>>
			struct has_for_each;

			template<typename ConcatAdaptor, typename Sink, typename... Rng>
			struct has_for_each<ConcatAdaptor, Sink, tc::concat_adaptor<Rng...>> : tc::constant<
				(tc::has_for_each<tc::apply_cvref_t<Rng, ConcatAdaptor>, tc::decay_t<Sink> const&> && ...)
			> {};

			template<typename Sink>
			struct sink {
				static_assert(tc::decayed<Sink>);
				using guaranteed_break_or_continue = guaranteed_break_or_continue_t<Sink>;
				Sink m_sink;

				template<typename RngAdaptor>
				constexpr auto operator()(RngAdaptor&& rngadaptor) const& return_MAYTHROW(
					tc::for_each(std::forward<RngAdaptor>(rngadaptor).base_range(), m_sink)
				)
			};
		}

		template<typename Sink>
		constexpr auto make_sink(Sink&& sink) noexcept { // not inline in for_each_impl because of MSVC
			return no_adl::sink<tc::decay_t<Sink>>{std::forward<Sink>(sink)};
		}
	}

	namespace concat_adaptor_adl {
		template<
			typename... Rng
		>
		struct [[nodiscard]] concat_adaptor_impl<false, Rng...>
		{
			static_assert(1<sizeof...(Rng)); // singleton range will be forwarded instead of packed into concat_adaptor
			tc::tuple<tc::range_adaptor_base_range<Rng>...> m_tupleadaptbaserng;

		public:
			template<typename... Rhs>
			constexpr concat_adaptor_impl(tc::aggregate_tag_t, Rhs&&... rhs) noexcept
				: m_tupleadaptbaserng{{ {{tc::aggregate_tag, std::forward<Rhs>(rhs)}}... }}
			{}

			using index_seq = std::make_index_sequence<sizeof...(Rng)>;

			template<typename ConcatRng, std::size_t... I>
			friend constexpr auto forward_base_ranges_as_tuple(ConcatRng&& rng, std::index_sequence<I...>) noexcept;

		protected:
			template<typename IntConstant>
			constexpr auto BaseRangeSize(IntConstant nconstIndex) const& noexcept {
				return tc::size_raw(tc::get<nconstIndex()>(m_tupleadaptbaserng).base_range());
			}
		public:
			constexpr auto size() const& noexcept requires (... && tc::has_size<Rng>) {
				return 
					tc::accumulate(
						tc::transform(
							std::index_sequence_for<Rng...>(),
							[&](auto nconstIndex) noexcept { return BaseRangeSize(nconstIndex); }
						),
						tc::explicit_cast<tc::common_type_t<decltype(tc::size_raw(std::declval<Rng>()))...>>(0),
						tc::fn_assign_plus()
					);
			}

			constexpr bool empty() const& noexcept {
				return tc::all_of(m_tupleadaptbaserng, [](auto const& adaptbaserng) noexcept { return tc::empty(adaptbaserng.base_range()); });
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, concat_adaptor_impl>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> tc::type::unique_t<tc::type::concat_t<
				tc::range_output_t<decltype(std::declval<tc::apply_cvref_t<tc::range_adaptor_base_range<Rng>, Self>>().base_range())>...
			>> {} // unevaluated
		};

		// Using return_decltype_MAYTHROW here and in concat_detail::no_adl::sink::operator() results
		// in MSVC 19.15 error C1202: Recursive type or function dependency context too complex.
		template<typename Self, typename Sink> requires concat_detail::no_adl::has_for_each<Self, Sink>::value
		constexpr auto for_each_impl(Self&& self, Sink&& sink) return_MAYTHROW(
			tc::for_each(
				std::forward<Self>(self).m_tupleadaptbaserng,
				concat_detail::make_sink(std::forward<Sink>(sink))
			)
		)

		template<typename Self, typename Sink> requires tc::is_concat_range<std::remove_cvref_t<Self>>::value
		constexpr auto for_each_reverse_impl(Self&& self, Sink&& sink) MAYTHROW {
			return tc::for_each(
				tc::reverse(std::forward<Self>(self).m_tupleadaptbaserng),
				[&](auto&& rngadaptor) MAYTHROW {
					return tc::for_each(tc::reverse(tc_move_if_owned(rngadaptor).base_range()), sink);
				}
			);
		}

		template<typename ConcatRng, std::size_t... I>
		[[nodiscard]] constexpr auto forward_base_ranges_as_tuple(ConcatRng&& rng, std::index_sequence<I...>) noexcept {
			return tc::forward_as_tuple(tc::get<I>(std::forward<ConcatRng>(rng).m_tupleadaptbaserng).base_range()...);
		}

		struct concat_end_index final {
			friend bool constexpr operator==(concat_end_index const&, concat_end_index const&) noexcept { return true; }
		};

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
				>
			>
		{
		private:
			using this_type = concat_adaptor_impl;

		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex=std::disjunction<tc::has_stashing_index<std::remove_reference_t<Rng>>...>::value;

			using difference_type = tc::common_type_t<typename boost::range_difference<Rng>::type...>;

			using concat_adaptor_impl<false, Rng...>::concat_adaptor_impl;

		private:
			template<std::size_t Index>
			constexpr tc_index create_begin_index(tc::constant<Index>) const& noexcept {
				static_assert(0 <= Index && Index <= sizeof...(Rng));
				if constexpr (sizeof...(Rng) == Index) {
					return tc_index(std::in_place_index<Index>, concat_end_index());
				} else {
					return tc_index(std::in_place_index<Index>, tc::get<Index>(this->m_tupleadaptbaserng).base_begin_index());
				}
			}

			template<int Index>
			constexpr tc_index create_end_index(tc::constant<Index>) const& noexcept {
				static_assert(0 <= Index);
				static_assert(Index < sizeof...(Rng));
				return tc_index(std::in_place_index<Index>, tc::get<Index>(this->m_tupleadaptbaserng).base_end_index());
			}

			template<int IndexFrom>
			constexpr void correct_index(tc_index& idx) const& noexcept {
				tc::for_each(
					tc::make_integer_sequence<std::size_t, IndexFrom, sizeof...(Rng)>(),
					[&](auto nconstIndex) noexcept {
						if (tc::at_end_index( tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(idx))) {
							idx = create_begin_index(tc::constant<nconstIndex() + 1>());
							return tc::continue_;
						} else {
							return tc::break_;
						}
					}
				);
			}

			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return tc_modified(
					create_begin_index(tc::constant<tc::explicit_cast<std::size_t>(0)>()),
					correct_index<0>(_)
				);
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				return create_begin_index(tc::constant<sizeof...(Rng)>());
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return sizeof...(Rng) == idx.index();
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				_ASSERT(!this->at_end_index(idx));

				tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) noexcept { 
						tc::increment_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(idx));
						correct_index<nconstIndex()>(idx);
					},
					idx.index()
				);
			}


			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& noexcept -> void
				requires
					(... && tc::has_decrement_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_end_index<std::remove_reference_t<Rng>>) &&
					tc::is_equality_comparable<tc_index>::value
			{
				tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
					[&](auto nconstIndexStart) noexcept {
						tc::for_each(
							tc::make_reverse_integer_sequence<int, 0, nconstIndexStart() + 1>(),
							[&](auto nconstIndex) noexcept {
								if constexpr (sizeof...(Rng) == nconstIndex()) {
									 idx = create_end_index(tc::constant<nconstIndex() - 1>());
									 return tc::continue_;
								} else {
									auto& idxCurrent = tc::get<nconstIndex()>(idx);
									if constexpr (0 == nconstIndex()) {
										_ASSERT( tc::get<0>(this->m_tupleadaptbaserng).base_begin_index() != idxCurrent );
									} else if (tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_begin_index() == idxCurrent) {
										idx = create_end_index(tc::constant<nconstIndex() - 1>());
										return tc::continue_;
									}
									// Remember early out above
									tc::decrement_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), idxCurrent);
									return tc::break_;
								}
							}
						);
					},
					idx.index()
				);
			}

MODIFY_WARNINGS(((suppress)(4544))) // 'Func2': default template argument ignored on this template declaration
			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) const& noexcept -> decltype(auto) {
				return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) constexpr noexcept -> decltype(auto) { // return_decltype leads to ICE
						return tc::dereference_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(idx));
					},
					idx.index()
				);
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) & noexcept -> decltype(auto) {
				return tc::invoke_with_constant<std::index_sequence_for<Rng...>>(
					[&](auto nconstIndex) constexpr noexcept -> decltype(auto) { // return_decltype leads to ICE
						return tc::dereference_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(idx));
					},
					idx.index()
				);
			}

			STATIC_FINAL_MOD(constexpr, advance_index)(tc_index& idx, difference_type d) const& noexcept -> void
				requires
					(... && tc::has_distance_to_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_end_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_advance_index<std::remove_reference_t<Rng>>)
			{
				tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
					[&](auto nconstIndexStart) noexcept {
						if (d < 0) {
							tc::for_each(
								tc::make_reverse_integer_sequence<int, 0, nconstIndexStart() + 1>(),
								[&](auto nconstIndex) noexcept {
									if constexpr (sizeof...(Rng) == nconstIndex()) {
										idx = create_end_index(tc::constant<nconstIndex() - 1>());
										return tc::continue_;
									} else {
										// As of Visual Studio compiler 19.15.26726, obtaining the range difference_type here as:
										//		using range_difference_t =
										//			typename boost::range_difference<decltype(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range())>::type;
										// triggers a compiler error, because VS does not properly exclude false statements from compilation when using
										// an 'if constexpr' inside a lambda, which means VS attempts to eval the type when nconstIndex==sizeof...(Rng).
										// Breaking the evaluation of the difference_type in two steps seems to work though.
										using range_t=decltype(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range());
										if constexpr(0 == nconstIndex()) {
											tc::advance_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(),
												tc::get<nconstIndex()>(idx),
												tc::explicit_cast<typename boost::range_difference<range_t>::type>(d)
											);
											return tc::break_;
										} else {
											auto const dToBegin = tc::distance_to_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(),
												tc::get<nconstIndex()>(idx),
												tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_begin_index()
											);

											if (!(d < dToBegin)) {
												tc::advance_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(),
													tc::get<nconstIndex()>(idx),
													tc::explicit_cast<typename boost::range_difference<range_t>::type>(d)
												);
												return tc::break_;
											} else {
												d -= dToBegin;
												idx = create_end_index(tc::constant<nconstIndex() - 1>());
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
									using range_difference_t = typename boost::range_difference<decltype(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range())>::type;
									if constexpr (nconstIndex() == sizeof...(Rng)-1) {
										tc::advance_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(),
											tc::get<nconstIndex()>(idx),
											tc::explicit_cast<range_difference_t>(d)
										);
										correct_index<nconstIndex()>(idx);
										return tc::break_;
									} else {
										auto const dToEnd = tc::distance_to_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(),
											tc::get<nconstIndex()>(idx),
											tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_end_index()
										);
										if (d < dToEnd) {
											tc::advance_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(),
												tc::get<nconstIndex()>(idx),
												tc::explicit_cast<range_difference_t>(d)
											);
											return tc::break_;
										} else {
											d -= dToEnd;
											idx = create_begin_index(tc::constant<nconstIndex() + 1>());
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

			STATIC_FINAL_MOD(constexpr, distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept -> difference_type
				requires
					(... && tc::has_distance_to_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_end_index<std::remove_reference_t<Rng>>)
			{
				if (idxLhs.index() == idxRhs.index()) {
					return tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
						[&](auto nconstIndex) noexcept -> difference_type {
							if constexpr (nconstIndex() == sizeof...(Rng)) {
								return 0;
							} else {
								return tc::distance_to_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(idxLhs), tc::get<nconstIndex()>(idxRhs));
							}
						},
						idxLhs.index()
					);
				} else {
					auto positive_distance = [&](tc_index const& lhs, tc_index const& rhs) noexcept {
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
									return tc::distance_to_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(lhs), tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_end_index());
								},
								lhs.index()
							) + 
							tc::invoke_with_constant<std::make_index_sequence<sizeof...(Rng)+1>>(
								[&](auto nconstIndex) noexcept -> difference_type {
									if constexpr(nconstIndex() == sizeof...(Rng)) {
										return 0;
									} else {
										return tc::distance_to_index(tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_range(), tc::get<nconstIndex()>(this->m_tupleadaptbaserng).base_begin_index(), tc::get<nconstIndex()>(rhs));
									}
								},
								rhs.index()
							),
							tc::fn_assign_plus()
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
		template<bool HasIterator, tc::has_constexpr_size... Rng>
		struct constexpr_size_impl<tc::concat_adaptor_adl::concat_adaptor_impl<HasIterator, Rng...>>
			: tc::constant<(... + tc::constexpr_size<Rng>::value)>
		{};

		template<typename... Rng>
		struct is_index_valid_for_move_constructed_range<tc::concat_adaptor_adl::concat_adaptor_impl<true, Rng...>>
			: std::conjunction<tc::is_index_valid_for_move_constructed_range<Rng>...>
		{};
	}

	namespace no_adl {
		struct fn_concat_impl final {
			[[nodiscard]] constexpr auto operator()() const& noexcept {
				return tc::empty_range();
			}

			template<typename Rng>
			[[nodiscard]] constexpr decltype(auto) operator()(Rng&& rng) const& noexcept {
				return std::forward<Rng>(rng);
			}

			template<typename... Rng> requires (1<sizeof...(Rng))
			[[nodiscard]] constexpr auto operator()(Rng&&... rng) const& noexcept {
				return tc::concat_adaptor<std::remove_cv_t<Rng>...>(tc::aggregate_tag, std::forward<Rng>(rng)...);
			}
		};
	}

	namespace concat_detail {
		template<typename Rng>
		[[nodiscard]] constexpr auto forward_range_as_tuple(Rng&& rng) noexcept {
			if constexpr( tc::safely_convertible_to<Rng&&, tc::empty_range> ) {
				return tc::tuple<>{};
			} else if constexpr( tc::is_concat_range<std::remove_cvref_t<Rng>>::value ) {
				return forward_base_ranges_as_tuple(std::forward<Rng>(rng), typename std::remove_cvref_t<Rng>::index_seq());
			} else {
				return tc::forward_as_tuple(std::forward<Rng>(rng));
			}
		}
	}

	template<typename... Rng>
	[[nodiscard]] constexpr decltype(auto) concat(Rng&&... rng) noexcept {
		return tc::apply(tc::no_adl::fn_concat_impl(), tc::tuple_cat(tc::concat_detail::forward_range_as_tuple(std::forward<Rng>(rng))...));
	}
}
