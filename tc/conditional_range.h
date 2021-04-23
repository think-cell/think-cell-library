
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "index_range.h"
#include "meta.h"
#include "size.h"
#include "type_list.h"
#include "variant.h"
#include "interval.h"

namespace tc {
	namespace select_range_adaptor_adl {
		template<typename... Rng>
		struct select_range_adaptor {
		//private: // public because for_each_impl is non-friend for _MSC_VER
			std::variant<decltype(tc::make_reference_or_value(std::declval<Rng>()))...> m_ubaserng;

			template<typename... FuncRng>
			explicit select_range_adaptor(aggregate_tag_t, int n, FuncRng&&... funcrng) noexcept
				: m_ubaserng([&]() noexcept {
					STATICASSERTEQUAL( sizeof...(Rng), sizeof...(FuncRng) );
					_ASSERT( tc::make_interval(0, tc::explicit_cast<int>(sizeof...(Rng))).contains(n) );
					return tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
						[&](auto nconstIndex) noexcept {
							return decltype(m_ubaserng)(
								std::in_place_index<nconstIndex()>, tc::aggregate_tag, tc::select_nth<nconstIndex()>(std::forward<FuncRng>(funcrng)...)()
							);
						},
						n
					);
				}())
			{}

			template<ENABLE_SFINAE>
			auto size() const& return_decltype_NOEXCEPT(
				tc::visit(SFINAE_VALUE(m_ubaserng), tc::projected(tc::fn_size_raw(), fn_indirection()))
			)

			bool empty() const& noexcept {
				return tc::visit(
					m_ubaserng,
					[](auto const& baserng) noexcept {
						return tc::empty(*baserng);
					}
				);
			}
		};

		// Must be non-friend for MSVC 15.8, but should be a hidden friend 
		template<typename Self, typename Sink, std::enable_if_t<tc::is_instance<select_range_adaptor, std::remove_reference_t<Self>>::value>* = nullptr>
		auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
			return tc::visit(std::forward<Self>(self).m_ubaserng, [&](auto&& rng) MAYTHROW {
				return tc::for_each(*tc_move_if_owned(rng), sink);
			});
		}

		template<typename Self, typename Sink, std::enable_if_t<tc::is_instance<select_range_adaptor, std::remove_reference_t<Self>>::value>* = nullptr>
		auto for_each_reverse_impl(Self&& self, Sink&& sink) MAYTHROW {
			return tc::visit(std::forward<Self>(self).m_ubaserng, [&](auto&& rng) MAYTHROW {
				return tc::for_each(tc::reverse(*tc_move_if_owned(rng)), sink);
			});
		}
	}
	using select_range_adaptor_adl::select_range_adaptor;

	namespace no_adl {
		template<typename SelectRangeAdaptor, typename... Rng>
		// Using tc::void_t<...> instead of decltype(std::declval<...>(), void()) does not compile with MSVC 15.8
		struct range_value<SelectRangeAdaptor, select_range_adaptor<Rng...>, decltype(std::declval<tc::common_range_value_t<decltype(std::declval<Rng>())...>>(), void())> final {
			using type = tc::common_range_value_t<decltype(std::declval<Rng>())...>;
		};
	}

	template<typename... FuncRng>
	auto select_range_impl(std::true_type, int n, FuncRng&&... funcrng) noexcept -> tc::common_reference_xvalue_as_ref_t<decltype(std::declval<FuncRng>()())...>
	{
#ifdef TC_WIN
		// The following assert must not hold: A function pointer to a function that returns a fixed size array by reference must also return a fixed size array by reference, not by value!
		// If MSVC fixed that bug, please unify select_range.
		static_assert(
			std::is_same<
				decltype(std::declval<
					int (&())[3]
				>()()),
				int [3]
			>::value
		);

		tc::storage_for<tc::common_reference_xvalue_as_ref_t<decltype(std::declval<FuncRng>()())...>> result;
		scope_exit(result.dtor());

		_ASSERT(0<=n && n<sizeof...(FuncRng));
		tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
			[&](auto nconstIndex) noexcept {
				result.ctor(tc::select_nth<nconstIndex()>(std::forward<FuncRng>(funcrng)...)());
			},
			n
		);
		return *tc_move(result);
#else
		static_assert(
			std::is_same<
				decltype(std::declval<
					int (&())[3]
				>()()),
				int (&)[3]
			>::value
		);

		return tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
			[&](auto nconstIndex) noexcept -> tc::common_reference_xvalue_as_ref_t<decltype(std::declval<FuncRng>()())...> {
				return tc::select_nth<nconstIndex()>(std::forward<FuncRng>(funcrng)...)();
			},
			n
		);
#endif
	}

	template<typename... FuncRng>
	auto select_range_impl(std::false_type, int n, FuncRng&&... funcrng) return_ctor_noexcept(
		select_range_adaptor<decltype(std::declval<FuncRng>()())...>,
		(aggregate_tag, n, std::forward<FuncRng>(funcrng)...)
	)

	template<typename... FuncRng>
	auto select_range(int n, FuncRng&&... funcrng) noexcept {
		return tc::select_range_impl(tc::has_common_reference_xvalue_as_ref<tc::type::list<decltype(std::forward<FuncRng>(funcrng)())...>>(), n, std::forward<FuncRng>(funcrng)...);
	}

	template<typename FuncRngTrue, typename FuncRngFalse>
	auto conditional_range(tc::bool_context b, FuncRngTrue&& funcrngTrue, FuncRngFalse&& funcrngFalse) noexcept {
		return tc::select_range(b ? 0 : 1, std::forward<FuncRngTrue>(funcrngTrue), std::forward<FuncRngFalse>(funcrngFalse));
	}

	template<typename FuncRngTrue, typename FuncRngFalse>
	constexpr decltype(auto) conditional_range(std::true_type, FuncRngTrue funcrngTrue, FuncRngFalse&& /*funcrngFalse*/) noexcept {
		return funcrngTrue();
	}

	template<typename FuncRngTrue, typename FuncRngFalse>
	constexpr decltype(auto) conditional_range(std::false_type, FuncRngTrue&& /*funcrngTrue*/, FuncRngFalse funcrngFalse) noexcept {
		return funcrngFalse();
	}

	template<typename Bool, typename FuncRngTrue>
	constexpr auto conditional_range(Bool&& b, FuncRngTrue&& funcrngTrue) noexcept {
		return tc::conditional_range(std::forward<Bool>(b), std::forward<FuncRngTrue>(funcrngTrue), MAKE_LAZY(tc::empty_range()));
	}
}

#include <boost/vmd/assert.hpp>

// BOOST_PP_VARIADIC_SIZE returns always at least 1, so there is no point of checking against 0. However, empty __VA_ARGS__ will trigger a compilation error in MAKE_LAZY
#define tc_conditional_range(b, ...) \
	BOOST_VMD_ASSERT(BOOST_PP_LESS_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2)) \
	tc::conditional_range(b, \
		MAKE_LAZY(BOOST_PP_VARIADIC_ELEM(0, __VA_ARGS__)) \
		BOOST_PP_COMMA_IF(BOOST_PP_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2)) \
		BOOST_PP_EXPR_IF(BOOST_PP_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2), MAKE_LAZY(BOOST_PP_VARIADIC_ELEM(1, __VA_ARGS__))) \
	)

namespace tc {
	template<typename Bool, typename Rng, typename Fn>
	constexpr decltype(auto) transform_range_if(Bool&& b, Rng&& rng, Fn fn) noexcept {
		return tc::conditional_range(std::forward<Bool>(b),
			/*funcrngTrue*/[&]() noexcept -> decltype(auto) { return fn(std::forward<Rng>(rng)); },
			/*funcrngFalse*/[&]() noexcept -> decltype(auto) { return std::forward<Rng>(rng); }
		);
	}

	namespace switch_range_detail {
		template<typename Enum, typename FuncRng, std::size_t... I>
		auto switch_range_impl(Enum const& e, FuncRng&& funcRng, std::index_sequence<I...>) noexcept {
			return tc::select_range(
				tc::explicit_cast<int>(tc::all_values<Enum>::index_of(e)),
				(I, [&]() noexcept -> decltype(auto) {
					return funcRng(INTEGRAL_CONSTANT(tc_at_nodebug(tc::all_values<Enum>(), I))());
				})...
			);
		}
	}

	template<typename Enum, typename... FuncRng>
	auto switch_range(Enum const& e, FuncRng&&... funcrng) noexcept {
		return switch_range_detail::switch_range_impl(e, tc::make_overload(std::forward<FuncRng>(funcrng)...), std::make_index_sequence<tc::constexpr_size<tc::all_values<Enum>>::value>());
	}
}
