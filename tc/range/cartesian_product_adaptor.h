
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "reverse_adaptor.h"
#include "../array.h"
#include "../algorithm/accumulate.h"

namespace tc {
	namespace cartesian_product_adaptor_adl {
		template<bool HasIterator, typename... Rng>
		struct cartesian_product_adaptor;
	}

	template<typename... Rng>
	using cartesian_product_adaptor = cartesian_product_adaptor_adl::cartesian_product_adaptor<(0 < sizeof...(Rng)) && (tc::range_with_iterators<Rng> && ...), Rng...>;

	namespace cartesian_product_adaptor_detail {
		template<bool bLast>
		constexpr static decltype(auto) select_element(auto&& val) noexcept {
			if constexpr(bLast) {
				return tc_move_if_owned(val);
			} else {
				return tc_const_forward(val);
			}
		}

		// MSVC workaround: move out of cartesian_product_adaptor to shorten symbol names.
		namespace no_adl {
			template<typename Self, typename Sink, typename... Ts>
			struct cartesian_product_sink;
		}

		namespace no_adl {
			template<typename Self, typename Sink, typename... Ts>
			struct cartesian_product_sink {
				using guaranteed_break_or_continue = std::conditional_t<
					std::is_same<tc::constant<tc::continue_>, tc::guaranteed_break_or_continue_t<Sink>>::value,
					tc::constant<tc::continue_>,
					tc::break_or_continue
				>;

				Self& m_self;
				Sink const& m_sink;
				tc::tuple<Ts...>& m_ts;

				template<typename T>
				constexpr auto operator()(T&& val) const& return_decltype_MAYTHROW(
					cartesian_product_adaptor_for_each_impl( // recursive MAYTHROW
						tc::forward_like<Self>(m_self),
						m_sink,
						tc::tuple_cat(
							/*cast to const rvalue*/tc_move_always_even_const(tc::as_const(m_ts)),
							tc::tie(
								cartesian_product_adaptor_detail::select_element</*bLast*/std::remove_reference_t<Self>::c_nCartesianProductAdaptorFactors == sizeof...(Ts) + 1>(
									tc_move_if_owned(val)
								)
							)
						)
					)
				)
			};
		}
	}

	namespace cartesian_product_adaptor_adl {
		template<typename... Rng>
		struct [[nodiscard]] cartesian_product_adaptor</*HasIterator*/false, Rng...> {
		protected:
			tc::tuple<tc::range_adaptor_base_range<Rng>...> m_tupleadaptbaserng;

		public:
			constexpr cartesian_product_adaptor() = default;
			template<typename... Rhs>
			constexpr cartesian_product_adaptor(aggregate_tag_t, Rhs&&... rhs) noexcept
				: m_tupleadaptbaserng{{ {{aggregate_tag, tc_move_if_owned(rhs)}}... }}
			{}

			static bool constexpr c_bIsCartesianProductAdaptor = true;
			static auto constexpr c_nCartesianProductAdaptorFactors = sizeof...(Rng);

			constexpr auto size() const& MAYTHROW requires (... && tc::has_size<Rng>) {
				return tc_apply([](auto const& ... rng) MAYTHROW {
					return tc::compute_range_adaptor_size<[](auto const ... n) noexcept {
						return tc::as_unsigned((... * n));
					}>(rng.base_range()...);
				}, m_tupleadaptbaserng);
			}

		private:
			template<typename Self, typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) == sizeof...(Rng)>* = nullptr, std::enable_if_t<tc::decayed_derived_from<Self, cartesian_product_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend constexpr auto cartesian_product_adaptor_for_each_impl(Self const&, Sink const& sink, tc::tuple<Ts...> ts) return_decltype_MAYTHROW(
				tc::continue_if_not_break(sink, tc_move(ts)) // MAYTHROW
			)

			template<typename Self, typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) < sizeof...(Rng)>* = nullptr, std::enable_if_t<tc::decayed_derived_from<Self, cartesian_product_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend constexpr auto cartesian_product_adaptor_for_each_impl(Self&& self, Sink const& sink, tc::tuple<Ts...> ts) return_decltype_MAYTHROW(
				tc::for_each(
					tc::get<sizeof...(Ts)>(tc_move_if_owned(self).m_tupleadaptbaserng).base_range(),
					cartesian_product_adaptor_detail::no_adl::cartesian_product_sink<Self, Sink, Ts...>{self, sink, ts}
				) // recursive MAYTHROW
			)

			template<typename Self, typename Sink, std::enable_if_t<tc::decayed_derived_from<Self, cartesian_product_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend constexpr auto for_each_impl(Self&& self, Sink const sink) return_decltype_MAYTHROW(
				cartesian_product_adaptor_for_each_impl(tc_move_if_owned(self), sink, tc::make_tuple()) // MAYTHROW
			)

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, cartesian_product_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> boost::mp11::mp_list<tc::tuple<
				boost::mp11::mp_apply<
					tc::common_reference_t,
					tc::mp_transform<
						std::add_rvalue_reference_t,
						tc::range_output_t<decltype(*std::declval<tc::apply_cvref_t<tc::reference_or_value<Rng>, Self>>())>
					>
				>...
			>> {} // unevaluated
		};

		template <typename RangeReturn, IF_TC_CHECKS(typename CheckUnique,) typename CartesianProductAdaptor, typename... Ts> requires std::remove_reference_t<CartesianProductAdaptor>::c_bIsCartesianProductAdaptor
		[[nodiscard]] constexpr decltype(auto) find_first_or_unique_impl(std::type_identity<RangeReturn>, IF_TC_CHECKS(CheckUnique bCheckUnique,) CartesianProductAdaptor&& rngtpl, tc::tuple<Ts...> const& tpl) MAYTHROW {
			if constexpr( RangeReturn::requires_iterator ) {
				typename std::remove_reference_t<CartesianProductAdaptor>::tc_index idx;
				if (tc::continue_ == tc::for_each(
					tc::zip(std::remove_reference_t<CartesianProductAdaptor>::base_ranges(tc_move_if_owned(rngtpl)), tpl, idx),
					[&](auto&& baserng, auto const& t, auto& baseidx) MAYTHROW {
						if( auto it = tc::find_first_or_unique(std::type_identity<tc::return_element_or_null>(), IF_TC_CHECKS(bCheckUnique, ) tc_move_if_owned(baserng), t) ) { // MAYTHROW
							baseidx = tc::iterator2index<decltype(baserng)>(tc_move(it));
							return tc::continue_;
						} else {
							return tc::break_;
						}
					}
				) ) {
					return RangeReturn::pack_element(rngtpl.make_iterator(tc_move(idx)), tc_move_if_owned(rngtpl)); // MAYTHROW
				} else {
					return RangeReturn::template pack_no_element(tc_move_if_owned(rngtpl));
				}
			} else if constexpr( std::same_as<RangeReturn, tc::return_bool> ) {
				return tc::all_of(
					tc::zip(std::remove_reference_t<CartesianProductAdaptor>::base_ranges(tc_move_if_owned(rngtpl)), tpl),
					[&](auto&& baserng, auto const& t) MAYTHROW {
						return tc::find_first_or_unique(std::type_identity<tc::return_bool>(), IF_TC_CHECKS(bCheckUnique, ) tc_move_if_owned(baserng), t); // MAYTHROW
					}
				);
			} else {
				tc::range_value_t<CartesianProductAdaptor> tplFound; // TODO Support non-default constructible and non-assignable types.
				if (tc::continue_ == tc::for_each(
					tc::zip(std::remove_reference_t<CartesianProductAdaptor>::base_ranges(tc_move_if_owned(rngtpl)), tpl, tplFound),
					[&](auto&& baserng, auto const& t, auto& tFound) MAYTHROW {
						if( auto ot = tc::find_first_or_unique(std::type_identity<tc::return_value_or_none>(), IF_TC_CHECKS(bCheckUnique, ) tc_move_if_owned(baserng), t) ) { // MAYTHROW
							tFound = *tc_move(ot);
							return tc::continue_;
						} else {
							return tc::break_;
						}
					}
				)) {
					return RangeReturn::template pack_element<CartesianProductAdaptor>(tc_move_if_owned(tplFound)); // MAYTHROW
				} else {
					return RangeReturn::template pack_no_element<CartesianProductAdaptor>();
				}
			}
		}
	}

	namespace cartesian_product_adaptor_detail {
		 // MSVC workaround: move lambdas out of cartesian_product_adaptor to shorten symbol names: (every range would appear twice otherwise)
		auto constexpr fn_base_begin_index = tc_mem_fn(.base_begin_index);
		auto constexpr fn_end_index = [](auto const nconst, auto const& adaptbaserng) MAYTHROW {
			if constexpr( 0 == nconst() ) {
				return adaptbaserng.base_end_index(); // MAYTHROW
			} else {
				return adaptbaserng.base_begin_index(); // MAYTHROW
			}
		};
		auto constexpr fn_increment_index = [](auto const nconst, auto const& adaptbaserng, auto& baseidx) MAYTHROW {
			tc::increment_index(adaptbaserng.base_range(), baseidx); // MAYTHROW
			if constexpr( 0 != nconst() ) {
				if( tc::at_end_index(adaptbaserng.base_range(), baseidx) ) { // MAYTHROW
					baseidx = adaptbaserng.base_begin_index(); // MAYTHROW
					return tc::continue_;
				} else {
					return tc::break_;
				}
			}
		};
		auto constexpr fn_decrement_index = [](auto const nconst, auto const& adaptbaserng, auto& baseidx) noexcept {
			if constexpr( 0 != nconst() ) {
				if( adaptbaserng.base_begin_index() == baseidx ) { // MAYTHROW
					baseidx = adaptbaserng.base_end_index(); // MAYTHROW
					tc::decrement_index(adaptbaserng.base_range(), baseidx); // MAYTHROW
					return tc::continue_;
				}
			}
			tc::decrement_index(adaptbaserng.base_range(), baseidx); // MAYTHROW
			return tc::break_;
		};

		template<typename DifferenceType>
		auto constexpr fn_distance_to_index = [](auto& nAccu, auto const nconst, auto const& adaptbaserng, auto const& baseidxLhs, auto const& baseidxRhs) MAYTHROW {
			if constexpr( 0 < nconst() ) {
				tc::assign_mul(nAccu, tc::explicit_cast<DifferenceType>(tc::size_raw(adaptbaserng.base_range()))); // MAYTHROW
			}
			tc::assign_add(nAccu, tc::explicit_cast<DifferenceType>(tc::distance_to_index(adaptbaserng.base_range(), baseidxLhs, baseidxRhs))); // MAYTHROW
		};

		namespace no_adl {
			template<typename DifferenceType>
			struct fn_advance_index {
				DifferenceType& m_d;
				constexpr auto operator()(auto const nconst, auto&& adaptbaserng, auto& baseidx) const& MAYTHROW {
					if constexpr( 0 == nconst() ) {
						tc::advance_index(adaptbaserng.base_range(), baseidx, tc::explicit_cast<typename boost::range_difference<decltype(adaptbaserng.base_range())>::type>(m_d)); // MAYTHROW
					} else {
						auto const nSize = tc::explicit_cast<DifferenceType>(tc::size_raw(adaptbaserng.base_range()));
						auto const nOldIdx = tc::distance_to_index(adaptbaserng.base_range(), adaptbaserng.base_begin_index(), baseidx);
						auto nNewIdx = nOldIdx + m_d;
						m_d = nNewIdx / nSize;
						nNewIdx %= nSize;
						if(nNewIdx < 0) {
							nNewIdx+=nSize;
							--m_d;
						}
						tc::advance_index(adaptbaserng.base_range(), baseidx, tc::explicit_cast<typename boost::range_difference<decltype(adaptbaserng.base_range())>::type>(nNewIdx - nOldIdx)); // MAYTHROW
						return tc::continue_if(m_d != 0);
					}
				}
			};

			struct fn_middle_point {
				bool& m_bAdvanced;
				void operator()(auto const& adaptbaserng, auto& baseidx, auto const& baseidxEnd) const& MAYTHROW {
					if( m_bAdvanced ) {
						baseidx = adaptbaserng.base_begin_index(); // MAYTHROW
					} else if( baseidx != baseidxEnd ) {
						auto const baseidxBegin = baseidx;
						tc::middle_point(adaptbaserng.base_range(), baseidx, baseidxEnd); // MAYTHROW
						m_bAdvanced = baseidxBegin != baseidx;
					}
				}
			};
		}
	}

	namespace cartesian_product_adaptor_adl {
		template<typename Rng0, typename... Rng>
		struct [[nodiscard]] cartesian_product_adaptor</*HasIterator*/true, Rng0, Rng...>
			: product_index_range_adaptor<cartesian_product_adaptor, /*IndexTemplate*/tc::tuple, Rng0, Rng...> {
		private:
			using this_type = cartesian_product_adaptor;
			using base_ = typename cartesian_product_adaptor::product_index_range_adaptor;
		public:
			constexpr cartesian_product_adaptor() = default;
			using base_::base_;

			using typename base_::tc_index;
			using difference_type = std::ptrdiff_t; // Like .size(), which returns size_t.

			static constexpr bool c_bPrefersForEach = true;

		private:
			static bool constexpr c_bCommonRange = tc::has_end_index<std::remove_reference_t<Rng0>>;

			constexpr bool internal_at_end_index(tc_index const& idx) const& MAYTHROW {
				return tc::any_of(tc::zip(this->m_tupleadaptbaserng, idx), [](auto const& adaptbaserng, auto const& baseidx) MAYTHROW {
					return tc::at_end_index(adaptbaserng.base_range(), baseidx); // MAYTHROW
				});
			}

			STATIC_FINAL_MOD(constexpr, begin_index)() const& MAYTHROW -> tc_index {
				auto idx = tc::tuple_transform(this->m_tupleadaptbaserng, cartesian_product_adaptor_detail::fn_base_begin_index); // MAYTHROW
				if constexpr( c_bCommonRange ) {
					if( internal_at_end_index(idx) ) { // MAYTHROW
						tc::get<0>(idx) = tc::get<0>(this->m_tupleadaptbaserng).base_end_index(); // MAYTHROW
					}
				}
				return idx;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& MAYTHROW requires c_bCommonRange {
				return tc::tuple_transform(tc::enumerate(this->m_tupleadaptbaserng), cartesian_product_adaptor_detail::fn_end_index);
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& MAYTHROW -> bool {
				if constexpr( c_bCommonRange ) {
					return tc::at_end_index(tc::get<0>(this->m_tupleadaptbaserng).base_range(), tc::get<0>(idx)); // MAYTHROW
				} else {
					// TODO This is suboptimal.
					// We should represent end by setting the end iterator at the first common range or add a bool to the index, if no factor is a common range.
					// On may also argue that the index should always contain the bool because increment_index, decrement_index and advance_index usually know
					// whether the resulting index is at end.
					return internal_at_end_index(idx); // MAYTHROW
				}
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& MAYTHROW -> void {
				tc::for_each(tc::reverse(tc::enumerate(tc::zip(this->m_tupleadaptbaserng, idx))), cartesian_product_adaptor_detail::fn_increment_index);
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& MAYTHROW -> void
				requires
					(tc::has_decrement_index<std::remove_reference_t<Rng0>> && ... && tc::has_decrement_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_end_index<std::remove_reference_t<Rng>>)
			{
				tc::for_each(tc::reverse(tc::enumerate(tc::zip(this->m_tupleadaptbaserng, idx))), cartesian_product_adaptor_detail::fn_decrement_index);
			}

			STATIC_FINAL_MOD(constexpr, advance_index)(tc_index& idx, difference_type d) const& MAYTHROW -> void
				requires
					(tc::has_advance_index<std::remove_reference_t<Rng0>> && ... && tc::has_advance_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_distance_to_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_size<std::remove_reference_t<Rng>>)
			{
				if( d != 0 ) {
					tc::for_each(
						tc::reverse(tc::enumerate(tc::zip(this->m_tupleadaptbaserng, idx))),
						cartesian_product_adaptor_detail::no_adl::fn_advance_index<difference_type>{d}
					);
				}
			}

			STATIC_FINAL_MOD(constexpr, distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& MAYTHROW -> difference_type
				requires
					(tc::has_distance_to_index<std::remove_reference_t<Rng0>> && ... && tc::has_distance_to_index<std::remove_reference_t<Rng>>) &&
					(... && tc::has_size<std::remove_reference_t<Rng>>)
			{
				return tc::accumulate(
					tc::enumerate(tc::zip(this->m_tupleadaptbaserng, idxLhs, idxRhs)),
					tc::explicit_cast<difference_type>(0),
					cartesian_product_adaptor_detail::fn_distance_to_index<difference_type>
				);
			}

			STATIC_FINAL_MOD(constexpr, middle_point)(tc_index& idx, tc_index const& idxEnd) const& MAYTHROW -> void
				requires
					(tc::has_middle_point<std::remove_reference_t<Rng0>> && ... && tc::has_middle_point<std::remove_reference_t<Rng>>)
			{
				bool bAdvanced = false;
				tc::for_each(
					tc::zip(this->m_tupleadaptbaserng, idx, idxEnd),
					cartesian_product_adaptor_detail::no_adl::fn_middle_point{bAdvanced}
				);
			}
		};
	}

	template<typename... Rng>
	constexpr auto enable_stable_index_on_move<cartesian_product_adaptor_adl::cartesian_product_adaptor</*HasIterator*/true, Rng...>>
		= (tc::stable_index_on_move<Rng> && ...);

	template<typename... Rng>
	constexpr decltype(auto) cartesian_product(Rng&&... rng) noexcept {
		if constexpr( 0 == sizeof...(Rng) ) {
			return tc::single(tc::tuple<>());
		} else {
			return tc::cartesian_product_adaptor<Rng...>(tc::aggregate_tag, tc_move_if_owned(rng)...);
		}
	}
}
