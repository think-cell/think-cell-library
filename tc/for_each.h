
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"

#include "break_or_continue.h"
#include "index_range.h"
#include "meta.h"
#include "scope.h"
#include "utility.h"
#include "conditional.h"

#include <optional>

namespace tc {

	namespace no_adl {
		template<typename Sink, typename Enable=void>
		struct sink_value_type_base {};

		template<typename Sink>
		struct sink_value_type_base<Sink, tc::void_t<typename std::remove_reference_t<Sink>::sink_value_type>> {
			using sink_value_type = typename std::remove_reference_t<Sink>::sink_value_type;
		};
	}
	using no_adl::sink_value_type_base;

	template<typename Sink>
	using sink_value_t = typename sink_value_type_base<Sink>::sink_value_type;

	namespace no_adl {
		template<typename ...T>
		struct common_type_break_or_continue;

		template<>
		struct common_type_break_or_continue<> final {
			using type = INTEGRAL_CONSTANT(tc::continue_);
		};

		template<typename ...T>
		struct common_type_break_or_continue<INTEGRAL_CONSTANT(tc::continue_), T...> {
			using type = typename common_type_break_or_continue<T...>::type;
		};

		template<typename ...T>
		struct common_type_break_or_continue<INTEGRAL_CONSTANT(tc::break_), T...> {
			using type = INTEGRAL_CONSTANT(tc::break_);
		};

		template<typename ...T>
		struct common_type_break_or_continue<tc::break_or_continue, T...> {
			using type = std::conditional_t<
				std::is_same<INTEGRAL_CONSTANT(tc::break_), typename common_type_break_or_continue<T...>::type>::value,
				INTEGRAL_CONSTANT(tc::break_),
				tc::break_or_continue
			>;
		};
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// for_each

	namespace no_adl {
		template< typename Rng, typename Func, typename RngDecayed, typename Enable=void >
		struct ForEachSpecialRange {};

		template< typename Enumset, typename Func, typename Enum >
		struct ForEachSpecialRange<Enumset, Func, tc::enumset<Enum>, tc::void_t<tc::common_type_t<decltype(tc::continue_if_not_break(std::declval<tc::decay_t<Func>&>(), std::declval<Enum&>())), INTEGRAL_CONSTANT(tc::continue_)>>> {
			static constexpr auto fn(Enumset&& enumset, tc::decay_t<Func> func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<Enum&>())), INTEGRAL_CONSTANT(tc::continue_)> {
				for (Enum e = tc::contiguous_enum<Enum>::begin(); e != tc::contiguous_enum<Enum>::end(); ++e) {
					if ((enumset&e)) {
						RETURN_IF_BREAK(tc::continue_if_not_break(func, e));
					}
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};

		template< typename Func, typename TypeList, typename Enable=void >
		struct ForEarchSpecialRangeParameterPack;

		template< typename Func, typename ...T >
		struct ForEarchSpecialRangeParameterPack<Func, tc::type::list<T...>, tc::void_t<typename common_type_break_or_continue<decltype(tc::continue_if_not_break(std::declval<tc::decay_t<Func>&>(), T()))...>::type>> final {
			static constexpr auto fn(tc::decay_t<Func> func) MAYTHROW {
				using result_type = typename common_type_break_or_continue<decltype(tc::continue_if_not_break(func, T()))...>::type;

				if constexpr (std::is_same<INTEGRAL_CONSTANT(tc::continue_), result_type>::value) {
					(func(T()), ...);
					return INTEGRAL_CONSTANT(tc::continue_)();
				} else {
					auto const breakorcontinue = ((tc::continue_ == tc::continue_if_not_break(func, T())) && ...) ? tc::continue_ : tc::break_;

					if constexpr (std::is_same<INTEGRAL_CONSTANT(tc::break_), result_type>::value) {
						return INTEGRAL_CONSTANT(tc::break_)();
					} else {
						return breakorcontinue;
					}
				}
			}
		};

		template< typename IntSequence, typename Func, typename TIndex, TIndex... Is >
		struct ForEachSpecialRange<IntSequence, Func, std::integer_sequence<TIndex, Is...>> {
			static constexpr auto fn(IntSequence&&, Func&& func) MAYTHROW return_decltype(
				ForEarchSpecialRangeParameterPack<Func BOOST_PP_COMMA() tc::type::list<std::integral_constant<TIndex BOOST_PP_COMMA() Is>...>>::fn(std::forward<Func>(func))
			)
		};

		template< typename TypeList, typename Func, typename... Ts >
		struct ForEachSpecialRange<TypeList, Func, tc::type::list<Ts...>> {
			static constexpr auto fn(TypeList&&, Func&& func) MAYTHROW return_decltype(
				ForEarchSpecialRangeParameterPack<Func BOOST_PP_COMMA() tc::type::list<tc::type::identity<Ts>...>>::fn(std::forward<Func>(func))
			)
		};

		template<typename T, T... I>
		struct has_value_type<std::integer_sequence<T, I...>> final: std::false_type {};

		template< typename Rng, typename Func, typename RngDecayed, typename Enable=void >
		struct ForEachElement: ForEachSpecialRange<Rng, Func, RngDecayed> {};
		
		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<
			std::is_void<decltype(std::declval<Rng>()(std::declval<Func>()))>::value ||
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), INTEGRAL_CONSTANT(tc::continue_)>::value
		>> {
			static constexpr INTEGRAL_CONSTANT(tc::continue_) fn(Rng&& rng, Func&& func) MAYTHROW {
				std::forward<Rng>(rng)( tc::make_ensure_non_breaking_functor<Func>(std::forward<Func>(func)) );
				return {};
			}
		};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), INTEGRAL_CONSTANT(tc::break_)>::value
		>> {
			static constexpr INTEGRAL_CONSTANT(tc::break_) fn(Rng&& rng, Func&& func) MAYTHROW {
				std::forward<Rng>(rng)( tc::make_ensure_always_breaking_functor<Func>(std::forward<Func>(func)) );
				return {};
			}
		};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), tc::break_or_continue>::value
		>> {
			static constexpr tc::break_or_continue fn(Rng&& rng, Func&& func) MAYTHROW {
				return std::forward<Rng>(rng)( std::forward<Func>(func) );
			}
		};

		template<typename Rng, typename Func, typename RngDecayed, typename Enable=void>
		struct is_invocable_on_range_reference final: std::false_type {};

		template<typename Rng, typename Func, typename RngDecayed>
		struct is_invocable_on_range_reference<Rng, Func, RngDecayed, std::enable_if_t<tc::is_range_with_iterators<RngDecayed>::value && !tc::has_index<RngDecayed>::value>> final: std::integral_constant<bool,
			std::is_invocable<tc::decay_t<Func>&, tc::range_reference_t<Rng>>::value
		> {};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<is_invocable_on_range_reference<Rng, Func, RngDecayed>::value>> {
			static constexpr auto fn(Rng&& rng, tc::decay_t<Func> func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
				auto const itEnd=tc::end(rng);
				for(auto it = tc::begin(rng); it!= itEnd; ++it) {
					RETURN_IF_BREAK( tc::continue_if_not_break(func, *it) );
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};

		template< typename Rng, typename Func, typename RngDecayed, typename Enable=void >
		struct ForEachChunk final: ForEachElement<Rng, Func, RngDecayed> {};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachChunk<Rng, Func, RngDecayed, std::enable_if_t<tc::has_mem_fn_chunk<tc::decay_t<Func>&, Rng>::value>> final {
			static auto fn(Rng&& rng, tc::decay_t<Func> func) MAYTHROW {
				return tc::continue_if_not_break([&]() MAYTHROW { return func.chunk(std::forward<Rng>(rng)); });
			}
		};
	}

	// Primary for_each dispatcher
	template<typename Rng, typename Func>
	constexpr auto for_each(Rng&& rng, Func&& func) MAYTHROW return_decltype(
		tc::no_adl::ForEachChunk<Rng, Func, std::conditional_t<std::is_array<std::remove_reference_t<Rng>>::value, tc::remove_cvref_t<Rng>, tc::decay_t<Rng>>>::fn(std::forward<Rng>(rng), std::forward<Func>(func))
	)

	namespace no_adl {
		template< typename Tuple, typename Func, typename... Ts >
		struct ForEachSpecialRange<Tuple, Func, std::tuple<Ts...>> {
			static constexpr auto fn(Tuple&& tuple, tc::decay_t<Func> func) MAYTHROW {
				return tc::for_each(
					std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple>>::value>(),
					[&](auto nconstIndex) MAYTHROW {
						return func(std::get<nconstIndex()>(std::forward<Tuple>(tuple)));
					}
				);
			}
		};
	}
}
