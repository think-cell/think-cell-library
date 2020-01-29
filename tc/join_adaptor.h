
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_adaptor.h"
#include "reference_or_value.h"
#include "index_range.h"
#include "for_each.h"
#include "accumulate.h"
#include "const_forward.h"

#include <functional>

namespace tc {
	namespace no_adl {
		template<
			typename RngRng
		>
		struct [[nodiscard]] join_adaptor {
		private:
			reference_or_value<RngRng> m_baserng;

		public:
			template<typename Rhs>
			explicit join_adaptor(aggregate_tag_t, Rhs&& rhs) noexcept
				: m_baserng( aggregate_tag, std::forward<Rhs>(rhs) )
			{}

			template< typename Func >
			auto operator()(Func func) const& MAYTHROW {
				return tc::for_each(*m_baserng, [&](auto&& _) MAYTHROW { return tc::for_each(std::forward<decltype(_)>(_), func); });
			}

			template<ENABLE_SFINAE>
			auto size() const& return_decltype_noexcept(
				// Use tc::mul and boost::multiprecision::number?
				tc::size_raw(static_cast<SFINAE_TYPE(RngRng const&)>(*m_baserng)) * tc::constexpr_size<tc::range_value_t<SFINAE_TYPE(RngRng)>>::value
			)
		};

		template<typename RngRng>
		struct constexpr_size_base<join_adaptor<RngRng>, std::void_t<typename tc::constexpr_size<RngRng>::type, typename tc::constexpr_size<tc::range_value_t<RngRng>>::type>>
			: std::integral_constant<std::size_t, tc::constexpr_size<RngRng>::value * tc::constexpr_size<tc::range_value_t<RngRng>>::value>
		{};

		template<typename JoinAdaptor, typename RngRng>
		struct range_value<JoinAdaptor, join_adaptor<RngRng>, tc::void_t<tc::range_value_t<tc::range_value_t<RngRng>>>> final {
			using type = tc::range_value_t<tc::range_value_t<RngRng>>;
		};
	}
	using no_adl::join_adaptor;

	template<typename RngRng>
	auto join(RngRng&& rng) return_ctor_noexcept(
		join_adaptor< RngRng >,
		(aggregate_tag, std::forward<RngRng>(rng))
	)

	namespace no_adl {
		namespace cartesian_product_adaptor_detail {
			// MSVC 15.8 complains if declared inside cartesian_product_adaptor
			// Use variadic tc::mul and boost::multiprecision::number?
			template<typename... Rng>
			using size_result_t = std::enable_if_t<std::conjunction<typename std::is_convertible<decltype(tc::size_raw(std::declval<Rng const&>())), std::size_t>::type...>::value, std::size_t>;

			template<typename T>
			static T&& select_element(T&& val, std::true_type) noexcept {
				return std::forward<T>(val);
			}

			template<typename T>
			static T const&& select_element(T&& val, std::false_type) noexcept {
				return tc::const_forward<T>(val);
			}
		}

		template<typename... Rng>
		struct [[nodiscard]] cartesian_product_adaptor {
		private:
			std::tuple<reference_or_value<Rng>...> m_baserng;

		public:
			template<typename... Rhs>
			cartesian_product_adaptor(aggregate_tag_t, Rhs&&... rhs) noexcept
				: m_baserng(reference_or_value<Rng>(aggregate_tag, std::forward<Rhs>(rhs))...)
			{}

		private:
			template<typename Sink, typename... Ts>
			struct fn_enumerate_impl {
				cartesian_product_adaptor<Rng...> const& m_adaptor;
				Sink& m_sink;
				std::tuple<Ts...>& m_ts;

				template<typename T>
				auto operator()(T&& val) const& return_decltype_MAYTHROW(
					m_adaptor.enumerate_impl( // recursive MAYTHROW
						m_sink,
						std::tuple_cat(
							/*cast to const rvalue*/std::move(tc::as_const(m_ts)),
							std::forward_as_tuple(
								cartesian_product_adaptor_detail::select_element(
									std::forward<T>(val),
									std::bool_constant<sizeof...(Rng) == sizeof...(Ts) + 1>()
								)
							)
						)
					)
				)
			};

			template<typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) == sizeof...(Rng)>* = nullptr>
			auto enumerate_impl(Sink& sink, std::tuple<Ts...> ts) const& return_decltype_MAYTHROW(
				tc::continue_if_not_break(sink, tc_move(ts)) // MAYTHROW
			)

			template<typename Sink, typename... Ts, std::enable_if_t<sizeof...(Ts) < sizeof...(Rng)>* = nullptr>
			auto enumerate_impl(Sink& sink, std::tuple<Ts...> ts) const& return_decltype_MAYTHROW(
				tc::for_each(*std::get<sizeof...(Ts)>(m_baserng), fn_enumerate_impl<Sink, Ts...>{*this, sink, ts}) // recursive MAYTHROW
			)

		public:
			template<typename Sink>
			auto operator()(Sink sink) const& return_decltype_MAYTHROW(
				enumerate_impl(sink, std::tuple<>()) // MAYTHROW
			)

			template<ENABLE_SFINAE>
			auto size() const& noexcept -> cartesian_product_adaptor_detail::size_result_t<SFINAE_TYPE(Rng)...> {
				using size_type = cartesian_product_adaptor_detail::size_result_t<Rng...>;
				return tc::accumulate(
					m_baserng,
					tc::implicit_cast<size_type>(1),
					[](size_type& nAccu, auto const& baserng) noexcept {
						size_type const nBase = tc::size_raw(*baserng);
						_ASSERT( nAccu <= std::numeric_limits<std::size_t>::max() / nBase );
						nAccu *= nBase;
					}
				);
			}
		};

		template<typename... Rng>
		struct constexpr_size_base<cartesian_product_adaptor<Rng...>, tc::void_t<typename tc::constexpr_size<Rng>::type...>>
			: std::integral_constant<std::size_t, (... * tc::constexpr_size<Rng>::value)>
		{};

		template<typename CartesianProductAdaptor, typename... Rng>
		struct range_value<CartesianProductAdaptor, cartesian_product_adaptor<Rng...>, tc::void_t<tc::range_value_t<Rng>...>> final {
			using type = std::tuple<tc::range_value_t<Rng>...>;
		};
	}
	using no_adl::cartesian_product_adaptor;

	template<typename... Rng>
	cartesian_product_adaptor<Rng...> cartesian_product(Rng&&... rng) noexcept {
		return {tc::aggregate_tag, std::forward<Rng>(rng)...};
	}
}
