
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/type_list.h"
#include "../base/invoke_with_constant.h"

#include "../algorithm/size.h"
#include "../variant.h"
#include "../interval.h"

#include "range_adaptor.h"
#include "index_range.h"
#include "meta.h"
#include "subrange.h"

#include <boost/preprocessor/punctuation/comma_if.hpp>

namespace tc {
	namespace no_adl {
		template <typename Sink>
		struct select_range_visitor { // MSVC workaround: not a lambda for shorter symbol names
			Sink m_sink;

			template<typename Rng>
			constexpr auto operator()(Rng&& rng) const& MAYTHROW {
				return tc::for_each(*tc_move_if_owned(rng), m_sink);
			}
		};
	}

	namespace select_range_adaptor_adl {
		template <bool HasIterator, typename... Rng>
		struct select_range_adaptor;

		template <typename... Rng>
		struct select_range_adaptor<false, Rng...> {
			static_assert( (!std::is_rvalue_reference<Rng>::value && ...) );
			friend select_range_adaptor<true, Rng...>;

		private:
			std::variant<tc::reference_or_value<Rng, /*bBestAccess*/true>...> m_ubaserng;

		public:
			template<typename... FuncRng>
			constexpr explicit select_range_adaptor(aggregate_tag_t, int n, FuncRng&&... funcrng) MAYTHROW
				: m_ubaserng(tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
					[&](auto const nconstIndex) MAYTHROW {
						STATICASSERTEQUAL(sizeof...(Rng), sizeof...(FuncRng));
						return decltype(m_ubaserng)(
							std::in_place_index<nconstIndex()>, tc::aggregate_tag, tc::select_nth<nconstIndex()>(tc_move_if_owned(funcrng)...)()
						);
					},
					n
				))
			{}

		private:
			static constexpr auto same_constexpr_size() noexcept requires (... && tc::has_constexpr_size<Rng>){
#if 0
				// Ideally, but doesn't compile under MSVC - bogus read of uninitialized symbol in constexpr context.
				return tc::all_same_element<tc::return_value_or_none>(tc::make_tuple(tc::constexpr_size<Rng>()...));
#endif

				std::optional<std::size_t> result;
				auto const all_same = ((result ? *result == tc::constexpr_size<Rng>() : (result = tc::constexpr_size<Rng>(), true)) && ...);
				return all_same ? result : std::nullopt;
			}

		public:
			constexpr auto size() const& MAYTHROW requires (... && tc::has_size<Rng>) {
				auto const runtime_size = [&]() MAYTHROW {
					return tc::fn_visit(tc::projected(tc::fn_size_raw(), fn_indirection()))(m_ubaserng);
				};

				if constexpr (( ... && tc::has_constexpr_size<Rng>)) {
					auto constexpr same_size = same_constexpr_size();
					if constexpr (same_size) {
						return tc::least_uint_constant<*same_size>{};
					} else {
						return runtime_size();
					}
				} else {
					return runtime_size();
				}
			}

			constexpr bool empty() const& noexcept {
				return tc::fn_visit([](auto const& baserng) noexcept {
					return tc::empty(*baserng);
				})(m_ubaserng);
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, select_range_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> boost::mp11::mp_unique<boost::mp11::mp_append<
				tc::range_output_t<decltype(*std::declval<tc::apply_cvref_t<tc::reference_or_value<Rng, /*bBestAccess*/true>, Self>>())>...
			>> {} // unevaluated

			template<typename Self, typename Sink> requires tc::decayed_derived_from<Self, select_range_adaptor>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				return tc::fn_visit(no_adl::select_range_visitor<tc::decay_t<Sink>>{tc_move_if_owned(sink)})(tc_move_if_owned_msvc_workaround(Self&&, self).m_ubaserng);
			}

			template<typename Self, typename Sink> requires tc::decayed_derived_from<Self, select_range_adaptor>
			friend constexpr auto for_each_reverse_impl(Self&& self, Sink&& sink) MAYTHROW {
				return tc::fn_visit([&](auto&& rng) MAYTHROW {
					return tc::for_each(tc::reverse(*tc_move_if_owned(rng)), sink);
				})(tc_move_if_owned_msvc_workaround(Self&&, self).m_ubaserng);
			}
		};

		template <typename ... Rng>
		using select_range_adaptor_index = boost::mp11::mp_apply<std::variant, boost::mp11::mp_unique<boost::mp11::mp_list<tc::index_t<Rng>...>>>;

		template <typename... Rng>
		struct select_range_adaptor<true, Rng...>
			: select_range_adaptor<false, Rng...>
			, tc::range_iterator_from_index< select_range_adaptor<true, Rng...>, select_range_adaptor_index<Rng...>>
		{
		private:
			using this_type = select_range_adaptor;

		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex = (... || tc::has_stashing_index<std::remove_reference_t<Rng>>::value);
			static constexpr bool c_bPrefersForEach = true;

			using difference_type = tc::common_type_t<typename boost::range_difference<Rng>::type...>;

			using select_range_adaptor<false, Rng...>::select_range_adaptor;

		private:
#pragma push_macro("forward_to_active")
#define forward_to_active(preamble, ...) \
	tc::fn_visit([&](auto& baserng) MAYTHROW -> decltype(auto) { \
		using range_t = decltype(baserng.best_access()); \
		using index_t = tc::index_t<range_t>; \
		preamble; \
		return __VA_ARGS__; \
	})(this->m_ubaserng)
#pragma push_macro("with_range_and_index")
#define with_range_and_index auto&& rng = *baserng; auto&& idx=tc::get<index_t>(tc_move_if_owned(idx_));

			STATIC_FINAL_MOD(constexpr, begin_index)() const& MAYTHROW {
				return forward_to_active(auto&& rng = baserng.best_access(), tc::implicit_cast<tc_index>(tc::begin_index(rng)));
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& MAYTHROW
				requires (... && tc::has_end_index<Rng>)
			{
				return forward_to_active(auto&& rng = baserng.best_access(), tc::implicit_cast<tc_index>(tc::end_index(rng)));
			}
			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx_) const& MAYTHROW {
				return forward_to_active(with_range_and_index, tc::at_end_index(rng, idx));
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(auto&& idx_) & MAYTHROW -> decltype(auto) {
				return forward_to_active(with_range_and_index,
					tc::explicit_cast<tc::common_reference_t<std::iter_reference_t<tc::iterator_t<Rng>>...>>(tc::dereference_index(rng, tc_move_if_owned(idx)))
				);
			}
			STATIC_FINAL_MOD(constexpr, dereference_index)(auto&& idx_) const& MAYTHROW -> decltype(auto) {
				return forward_to_active(with_range_and_index,
					tc::explicit_cast<tc::common_reference_t<std::iter_reference_t<tc::iterator_t<Rng const&>>...>>(tc::dereference_index(rng, tc_move_if_owned(idx)))
				);
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx_) const& MAYTHROW {
				forward_to_active(with_range_and_index,tc::increment_index(rng, idx));
			}
			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx_) const& MAYTHROW
				requires (... && tc::has_decrement_index<Rng>)
			{
				forward_to_active(with_range_and_index, tc::decrement_index(rng, idx));
			}
			STATIC_FINAL_MOD(constexpr, advance_index)(tc_index& idx_, difference_type d) const& MAYTHROW
				requires (... && tc::has_advance_index<Rng>)
			{
				forward_to_active(with_range_and_index, tc::advance_index(rng, idx, tc::explicit_cast<typename boost::range_difference<std::remove_cvref_t<range_t>>::type>(d)));
			}
			STATIC_FINAL_MOD(constexpr, distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& MAYTHROW
				requires (... && tc::has_distance_to_index<Rng>)
			{
				return forward_to_active(auto&& rng = *baserng, tc::distance_to_index(rng, tc::get<index_t>(idxLhs), tc::get<index_t>(idxRhs)));
			}
			STATIC_FINAL_MOD(constexpr, middle_point)( tc_index& idxBegin, tc_index const& idxEnd ) const& MAYTHROW
				requires (... && tc::has_middle_point<Rng>)
			{
				forward_to_active(auto&& rng = *baserng, tc::middle_point(rng, tc::get<index_t>(idxBegin), tc::get<index_t>(idxEnd)));
			}

#pragma pop_macro("with_range_and_index")
#pragma pop_macro("forward_to_active")
		};

		// While we normally try to use `tc::subrange` in the case of all ranges having the same iterator type,
		// we can't do that if we have rvalues that we need to aggregate.
		template <typename... Rng>
			requires (1 == boost::mp11::mp_size<boost::mp11::mp_unique<boost::mp11::mp_list<tc::iterator_t<Rng>...>>>::value)
		struct select_range_adaptor<true, Rng...>
			: select_range_adaptor<false, Rng...>
		{
		public:
			static constexpr bool c_bHasStashingIndex = (... || tc::has_stashing_index<std::remove_reference_t<Rng>>::value);

			using select_range_adaptor<false, Rng...>::select_range_adaptor;

			auto begin() & MAYTHROW {
				return tc::fn_visit([](auto& refrng) MAYTHROW { return tc::begin(*refrng); })(this->m_ubaserng);
			}
			auto begin() const& MAYTHROW {
				return tc::fn_visit([](auto& refrng) MAYTHROW { return tc::begin(*refrng); })(this->m_ubaserng);
			}

			auto end() & MAYTHROW {
				return tc::fn_visit([](auto& refrng) MAYTHROW { return tc::end(*refrng); })(this->m_ubaserng);
			}
			auto end() const& MAYTHROW {
				return tc::fn_visit([](auto& refrng) MAYTHROW { return tc::end(*refrng); })(this->m_ubaserng);
			}
		};
	}
	template <typename ... Rng>
	using select_range_adaptor = select_range_adaptor_adl::select_range_adaptor<tc::ranges_with_common_reference<Rng...>, Rng...>;

	template<typename... FuncRng> requires requires { typename tc::common_reference_t<decltype(std::declval<FuncRng>()())...>; }
	constexpr auto select_range(int n, FuncRng&&... funcrng) MAYTHROW -> tc::common_reference_t<decltype(std::declval<FuncRng>()())...> {
#ifdef _MSC_VER
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

		tc::storage_for<tc::common_reference_t<decltype(std::declval<FuncRng>()())...>> result;
		tc_scope_exit { result.dtor(); };

		tc::invoke_with_constant<std::index_sequence_for<FuncRng...>>(
			[&](auto const nconstIndex) MAYTHROW {
				result.ctor(tc::select_nth<nconstIndex()>(tc_move_if_owned(funcrng)...)());
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
			[&](auto const nconstIndex) MAYTHROW -> tc::common_reference_t<decltype(std::declval<FuncRng>()())...> {
				return tc::select_nth<nconstIndex()>(tc_move_if_owned(funcrng)...)();
			},
			n
		);
#endif
	}
	template<typename... FuncRng>
	constexpr auto select_range(int n, FuncRng&&... funcrng) return_ctor_MAYTHROW(
		select_range_adaptor<tc::remove_rvalue_reference_t<decltype(std::declval<FuncRng>()())>...>,
		(aggregate_tag, n, tc_move_if_owned(funcrng)...)
	)

	template<typename FuncRngTrue, typename FuncRngFalse>
	constexpr auto conditional_range(tc::bool_context b, FuncRngTrue&& funcrngTrue, FuncRngFalse&& funcrngFalse) return_decltype_allow_xvalue_MAYTHROW(
		tc::select_range(b ? 0 : 1, tc_move_if_owned(funcrngTrue), tc_move_if_owned(funcrngFalse))
	)
	template<typename FuncRngTrue, typename FuncRngFalse>
	constexpr auto conditional_range(tc::constant<true>, FuncRngTrue funcrngTrue, FuncRngFalse&& /*funcrngFalse*/) return_decltype_allow_xvalue_MAYTHROW(
		funcrngTrue()
	)

	template<typename FuncRngTrue, typename FuncRngFalse>
	constexpr auto conditional_range(tc::constant<false>, FuncRngTrue&& /*funcrngTrue*/, FuncRngFalse funcrngFalse) return_decltype_allow_xvalue_MAYTHROW(
		funcrngFalse()
	)

	template<typename Bool, typename FuncRngTrue>
	constexpr auto conditional_range(Bool&& b, FuncRngTrue&& funcrngTrue) return_decltype_allow_xvalue_MAYTHROW(
		tc::conditional_range(tc_move_if_owned(b), tc_move_if_owned(funcrngTrue), tc::fn_explicit_cast<tc::empty_range>())
	)
}

#include <boost/vmd/assert.hpp>

// BOOST_PP_VARIADIC_SIZE returns always at least 1, so there is no point of checking against 0. However, empty __VA_ARGS__ will trigger a compilation error in tc_lazy
#define tc_conditional_range(b, ...) \
	BOOST_VMD_ASSERT(BOOST_PP_LESS_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2)) \
	tc::conditional_range(b, \
		tc_lazy(BOOST_PP_VARIADIC_ELEM(0, __VA_ARGS__)) \
		BOOST_PP_COMMA_IF(BOOST_PP_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2)) \
		BOOST_PP_EXPR_IF(BOOST_PP_EQUAL(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 2), tc_lazy(BOOST_PP_VARIADIC_ELEM(1, __VA_ARGS__))) \
	)

namespace tc {
	template<typename Bool, typename Rng, typename Fn>
	constexpr decltype(auto) transform_range_if(Bool&& b, Rng&& rng, Fn fn) noexcept {
		return tc::conditional_range(tc_move_if_owned(b),
			/*funcrngTrue*/[&]() noexcept -> decltype(auto) { return fn(tc_move_if_owned(rng)); },
			/*funcrngFalse*/[&]() noexcept -> decltype(auto) { return tc_move_if_owned(rng); }
		);
	}

#if defined(__clang__) || defined(_MSC_VER) // gcc internal compiler error
	namespace switch_range_detail {
		template<typename Enum, typename FuncRng, std::size_t... I>
		constexpr auto switch_range_impl(Enum const& e, FuncRng&& funcRng, std::index_sequence<I...>) noexcept {
			return tc::select_range(
				tc::explicit_cast<int>(tc::all_values<Enum>::index_of(e)),
				(I, [&]() noexcept -> decltype(auto) {
					return funcRng(tc::constant<tc_at_nodebug(tc::all_values<Enum>(), I)>());
				})...
			);
		}
	}

	template<typename Enum, typename... FuncRng>
	constexpr auto switch_range(Enum const& e, FuncRng&&... funcrng) noexcept {
		return switch_range_detail::switch_range_impl(
			e, tc::make_overload(tc_move_if_owned(funcrng)...),
			std::make_index_sequence<tc::constexpr_size<tc::all_values<Enum>>()>()
		);
	}
#endif
}
