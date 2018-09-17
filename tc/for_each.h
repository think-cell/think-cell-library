
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"

#include "break_or_continue.h"
#include "index_range.h"
#include "meta.h"
#include "scope.h"
#include "size.h"
#include "utility.h"
#include "conditional.h"

#include <boost/optional.hpp>

#include <boost/variant.hpp> // needed for parameter_storage

namespace tc {
	template<typename Func>
	using sink_value_t = typename std::remove_reference_t<Func>::sink_value_type;

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
	template<typename ...T>
	using common_type_break_or_continue_t = typename no_adl::common_type_break_or_continue<tc::decay_t<T>...>::type;

	//-------------------------------------------------------------------------------------------------------------------------
	// for_each

	namespace no_adl {
		template< typename Rng, typename Func, typename RngDecayed, typename Enable=void >
		struct ForEachSpecialRange;

		template< typename Enumset, typename Func, typename Enum >
		struct ForEachSpecialRange<Enumset, Func, tc::enumset<Enum>, void> final {
			using type = ForEachSpecialRange;
			constexpr static auto fn(Enumset&& enumset, tc::decay_t<Func> func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<Enum&>())), INTEGRAL_CONSTANT(tc::continue_)> {
				for (Enum e = tc::contiguous_enum<Enum>::begin(); e != tc::contiguous_enum<Enum>::end(); ++e) {
					if ((enumset&e)) {
						RETURN_IF_BREAK(tc::continue_if_not_break(func, e));
					}
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};

		template< typename IntSequence, typename Func, typename TIndex, TIndex... Is >
		struct ForEachSpecialRange<IntSequence, Func, std::integer_sequence<TIndex, Is...>, void> final {
			using type = ForEachSpecialRange;
			constexpr static auto fn(IntSequence&&, tc::decay_t<Func> func) MAYTHROW {
				using swallow = std::initializer_list<bool>;
				using result_type = tc::common_type_break_or_continue_t<decltype( tc::continue_if_not_break( func, std::integral_constant< TIndex, Is >() ) )...>;

				if constexpr (std::is_same<INTEGRAL_CONSTANT(tc::continue_), result_type>::value) {
					(func(std::integral_constant<TIndex, Is>()), ...);
					return INTEGRAL_CONSTANT(tc::continue_)();
				} else {
					auto const breakorcontinue = ((tc::continue_ == tc::continue_if_not_break(func, std::integral_constant<TIndex, Is>())) && ...) ? tc::continue_ : tc::break_;

					if constexpr (std::is_same<INTEGRAL_CONSTANT(tc::break_), result_type>::value) {
						return INTEGRAL_CONSTANT(tc::break_)();
					} else {
						return breakorcontinue;
					}
				}
			}
		};

		template< typename Rng, typename Func, typename RngDecayed, typename Enable=void >
		struct ForEachElement final {
			using type = typename ForEachSpecialRange<Rng, Func, RngDecayed>::type;
		};
		
		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<
			std::is_void<decltype(std::declval<Rng>()(std::declval<Func>()))>::value ||
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), INTEGRAL_CONSTANT(tc::continue_)>::value
		>> final {
			using type = ForEachElement;
			constexpr static INTEGRAL_CONSTANT(tc::continue_) fn(Rng&& rng, Func&& func) MAYTHROW {
				std::forward<Rng>(rng)( tc::make_ensure_non_breaking_functor<Func>(std::forward<Func>(func)) );
				return {};
			}
		};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), INTEGRAL_CONSTANT(tc::break_)>::value
		>> final {
			using type = ForEachElement;
			constexpr static INTEGRAL_CONSTANT(tc::break_) fn(Rng&& rng, Func&& func) MAYTHROW {
				std::forward<Rng>(rng)( tc::make_ensure_always_breaking_functor<Func>(std::forward<Func>(func)) );
				return {};
			}
		};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), tc::break_or_continue>::value
		>> final {
			using type = ForEachElement;
			constexpr static tc::break_or_continue fn(Rng&& rng, Func&& func) MAYTHROW {
				return std::forward<Rng>(rng)( std::forward<Func>(func) );
			}
		};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachElement<Rng, Func, RngDecayed, std::enable_if_t<tc::is_range_with_iterators< RngDecayed >::value && !tc::has_index< RngDecayed >::value>> final {
			using type = ForEachElement;
			constexpr static auto fn(Rng&& rng, tc::decay_t<Func> func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
				auto const itEnd=tc::end(rng);
				for(auto it = tc::begin(rng); it!= itEnd; ++it) {
					RETURN_IF_BREAK( tc::continue_if_not_break(func, *it) );
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};

		template< typename Rng, typename Func, typename RngDecayed, typename Enable=void >
		struct ForEachChunk final {
			using type = typename ForEachElement<Rng, Func, RngDecayed>::type;
		};

		template< typename Rng, typename Func, typename RngDecayed >
		struct ForEachChunk<Rng, Func, RngDecayed, std::enable_if_t<tc::has_mem_fn_chunk<tc::decay_t<Func>&, Rng>::value>> final {
			using type = ForEachChunk;
			static auto fn(Rng&& rng, Func&& func) MAYTHROW {
				return tc::continue_if_not_break([](Rng&& rng, tc::decay_t<Func> func) MAYTHROW { return func.chunk(std::forward<Rng>(rng)); }, std::forward<Rng>(rng), std::forward<Func>(func));
			}
		};
	}

	// Primary for_each dispatcher
	template<typename Rng, typename Func>
	constexpr auto for_each(Rng&& rng, Func&& func) MAYTHROW {
		return tc::no_adl::ForEachChunk<Rng, Func, std::conditional_t<std::is_array<std::remove_reference_t<Rng>>::value, tc::remove_cvref_t<Rng>, tc::decay_t<Rng>>>::type::fn(std::forward<Rng>(rng), std::forward<Func>(func));
	}

	namespace no_adl {
		template< typename Tuple, typename Func, typename... Ts >
		struct ForEachSpecialRange<Tuple, Func, std::tuple<Ts...>, void> final {
			using type = ForEachSpecialRange;
			constexpr static auto fn(Tuple&& tuple, tc::decay_t<Func> func) MAYTHROW {
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
