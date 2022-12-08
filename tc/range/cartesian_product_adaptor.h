
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/const_forward.h"
#include "../algorithm/accumulate.h"

namespace tc {
	namespace cartesian_product_adaptor_detail {
		// MSVC 15.8 complains if declared inside cartesian_product_adaptor
		// Use variadic tc::mul and boost::multiprecision::number?
		template<typename... Rng>
		using size_result_t = std::enable_if_t<std::conjunction<typename std::is_convertible<decltype(tc::size_raw(std::declval<Rng const&>())), std::size_t>::type...>::value, std::size_t>;

		template<typename T>
		constexpr static T&& select_element(T&& val, tc::constant<true>) noexcept {
			return std::forward<T>(val);
		}

		template<typename T>
		constexpr static T const&& select_element(T&& val, tc::constant<false>) noexcept {
			return tc::const_forward<T>(val);
		}
	}

	namespace cartesian_product_adaptor_adl {
		template<typename... Rng>
		struct [[nodiscard]] cartesian_product_adaptor {
		private:
			tc::tuple<reference_or_value<Rng>...> m_baserng;

		public:
			template<typename... Rhs>
			constexpr cartesian_product_adaptor(aggregate_tag_t, Rhs&&... rhs) noexcept
				: m_baserng{{ {{aggregate_tag, std::forward<Rhs>(rhs)}}... }}
			{}

		private:
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
					std::remove_reference_t<Self>::internal_for_each_impl( // recursive MAYTHROW
						std::forward<Self>(m_self),
						m_sink,
						tc::tuple_cat(
							/*cast to const rvalue*/std::move(tc::as_const(m_ts)),
							tc::forward_as_tuple(
								cartesian_product_adaptor_detail::select_element(
									std::forward<T>(val),
									tc::constant<sizeof...(Rng) == sizeof...(Ts) + 1>()
								)
							)
						)
					)
				)
			};

			template<typename Self, typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) == sizeof...(Rng)>* = nullptr>
			static constexpr auto internal_for_each_impl(Self const&, Sink const& sink, tc::tuple<Ts...> ts) return_decltype_MAYTHROW(
				tc::continue_if_not_break(sink, tc_move(ts)) // MAYTHROW
			)

			template<typename Self, typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) < sizeof...(Rng)>* = nullptr>
			static constexpr auto internal_for_each_impl(Self&& self, Sink const& sink, tc::tuple<Ts...> ts) return_decltype_MAYTHROW(
				tc::for_each(*tc::get<sizeof...(Ts)>(std::forward<Self>(self).m_baserng), cartesian_product_sink<Self, Sink, Ts...>{self, sink, ts}) // recursive MAYTHROW
			)

		public:
			template<typename Self, typename Sink, std::enable_if_t<tc::is_base_of_decayed<cartesian_product_adaptor, Self>::value>* = nullptr>
			friend constexpr auto for_each_impl(Self&& self, Sink const sink) return_decltype_MAYTHROW(
				std::remove_reference_t<Self>::internal_for_each_impl(std::forward<Self>(self), sink, tc::make_tuple()) // MAYTHROW
			)

			template<typename Self, std::enable_if_t<tc::is_base_of_decayed<cartesian_product_adaptor, Self>::value>* = nullptr>
			friend auto range_output_t_impl(Self&&) -> tc::type::list<tc::tuple<
				tc::type::apply_t<
					tc::common_reference_xvalue_as_ref_t,
					tc::type::transform_t<
						tc::range_output_t<decltype(*std::declval<tc::apply_cvref_t<tc::reference_or_value<Rng>, Self>>())>,
						std::add_rvalue_reference_t
					>
				>...
			>> {} // unevaluated

			template<ENABLE_SFINAE>
			constexpr auto size() const& noexcept -> cartesian_product_adaptor_detail::size_result_t<SFINAE_TYPE(Rng)...> {
				using size_type = cartesian_product_adaptor_detail::size_result_t<Rng...>;
				return tc::accumulate(
					m_baserng,
					tc::explicit_cast<size_type>(1),
					[](size_type& nAccu, auto const& baserng) noexcept {
						size_type const nBase = tc::size_raw(*baserng);
						_ASSERT( nAccu <= std::numeric_limits<size_type>::max() / nBase );
						nAccu *= nBase;
					}
				);
			}

			constexpr decltype(auto) base_ranges() const& noexcept {
				return tc::tuple_transform(m_baserng, tc::fn_indirection());
			}
		};

		template <typename RangeReturn, IF_TC_CHECKS(typename CheckUnique,) typename... Rng, typename... Ts>
		[[nodiscard]] constexpr decltype(auto) find_first_or_unique_impl(tc::type::identity<RangeReturn>, IF_TC_CHECKS(CheckUnique bCheckUnique,) cartesian_product_adaptor<Rng...> const& rngtpl, tc::tuple<Ts...> const& tpl) noexcept {
			STATICASSERTEQUAL(sizeof...(Rng), sizeof...(Ts));
			using size_type = decltype(rngtpl.size());
			size_type nAccu = 0;
			if (tc::continue_ == tc::for_each(
				tc::tuple_zip(rngtpl.base_ranges(), tpl),
				[&](auto const& rngt, auto const& t) noexcept {
					size_type const nBase = tc::size_raw(rngt);
					_ASSERT(nAccu <= std::numeric_limits<size_type>::max() / nBase);
					nAccu *= nBase;
					if (auto const onDigit = tc::find_first_or_unique(tc::type::identity<tc::return_element_index_or_none>(), IF_TC_CHECKS(bCheckUnique, ) rngt, t)) {
						_ASSERT(nAccu <= std::numeric_limits<size_type>::max() - *onDigit);
						nAccu += *onDigit;
						return tc::continue_;
					} else {
						return tc::break_;
					}
				}
			)) {
				_ASSERTDEBUGEQUAL(tc::linear_at<tc::return_value>(rngtpl, nAccu), tpl);
				return RangeReturn::pack_element_index(nAccu, rngtpl);
			} else {
				return RangeReturn::template pack_no_element_index<size_type>(rngtpl);
			}
		}
	}
	using cartesian_product_adaptor_adl::cartesian_product_adaptor;

	namespace no_adl {
		template<typename... Rng>
		struct constexpr_size_base<cartesian_product_adaptor<Rng...>, tc::void_t<typename tc::constexpr_size<Rng>::type...>>
			: tc::constant<(... * tc::constexpr_size<Rng>::value)>
		{};
	}

	template<typename... Rng>
	constexpr cartesian_product_adaptor<Rng...> cartesian_product(Rng&&... rng) noexcept {
		return {tc::aggregate_tag, std::forward<Rng>(rng)...};
	}
}
