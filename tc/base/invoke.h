
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "utility.h"
#include "tag_type.h"

#include <boost/preprocessor/punctuation/comma_if.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/variadic/to_seq.hpp>

namespace tc {
	//////////////////////////////////////////////////////////////////////////
	// invoke
	namespace invoke_detail {
		namespace no_adl {
			template <typename Arg>
			struct expanded final {
				using indices = std::index_sequence<0>;

				template <std::size_t I>
				static constexpr Arg&& select(Arg&& arg) noexcept {
					STATICASSERTEQUAL(I, 0);
					return tc_move_if_owned(arg);
				}
			};

			template <typename Tuple> requires tc::tuple_like<tc::remove_ref_temporary_t<Tuple>>
			struct expanded<Tuple> final {
				using indices = std::make_index_sequence<std::tuple_size<tc::remove_ref_temporary_t<Tuple>>::value>;

				template <std::size_t I>
				static constexpr decltype(auto) select(Tuple&& tpl) noexcept {
					// While tc_decrement_lifetime, tc_increment_lifetime are correct here, it increases compile time of tcaddin by 20% on Mac.
					return /*tc_decrement_lifetime(*/tc::get<I>(/*tc_increment_lifetime(*/tc_move_if_owned(tpl))/*))*/;
				}
			};
		}
		using no_adl::expanded;

		// We use ADL to workaround cyclic dependencies.
		namespace expanding_invoke_adl {
			DEFINE_ADL_TAG_TYPE(tag)
		}

		namespace no_adl {
			struct overload_temporary {};
			struct overload_remove_temporary {};
			struct overload_expanded {};
		}
		using no_adl::overload_temporary;
		using no_adl::overload_remove_temporary;
		using no_adl::overload_expanded;

		// Simplify using requires on invoke once xcode implements CWG issue 2369.
		template <bool bForceExpand, typename Func, typename ... Args>
		constexpr auto select_overload() noexcept {
			if constexpr (bForceExpand) {
				return overload_expanded{};
			} else if constexpr (requires(Func&& func, Args&&... args) { tc_move_if_owned(func)(tc_move_if_owned(args)...); }) {
				using result_type = decltype(std::declval<Func>()(std::declval<Args>()...));
				static_assert(!tc::instance_tn<std::remove_reference_t<result_type>, tc::temporary>, "function returns a dangling temporary");
				if constexpr (std::is_rvalue_reference<result_type>::value) {
					return overload_temporary{}; // if the return type when called with xvalues is an xvalue, function might want to react to temporaries
				} else {
					return overload_remove_temporary{};
				}
			} else if constexpr ((tc::tuple_like<Args&&> || ...)) {
				return overload_expanded{};
			}
		}
		template <bool bForceExpand, typename Func, typename ... Args>
		using selected_overload = decltype(select_overload<bForceExpand, Func, tc::unwrap_temporary_t<Args>...>());

#pragma push_macro("return_invoke")
// We do want to return a tc::temporary<T, 0>, so can't use the return_decltype_* macros
// (We have to use decltype(auto) + requires due to a GCC compiler bug.)
#define return_invoke(...) noexcept(noexcept(__VA_ARGS__)) -> decltype(auto) requires requires { __VA_ARGS__; } { return __VA_ARGS__; }

		template <bool bForceExpand, typename Func, typename ... Args, /* not requires because of CWG issue 2369 */std::enable_if_t<std::same_as<selected_overload<bForceExpand, Func, Args...>, overload_temporary>>* = nullptr>
		constexpr auto invoke(Func&& func, Args&&... args) return_invoke(
			tc_decrement_lifetime(tc_move_if_owned(func)(tc_increment_lifetime(tc_move_if_owned(args))...))
		)

		template <bool bForceExpand, typename Func, typename ... Args, /* not requires because of CWG issue 2369 */std::enable_if_t<std::same_as<selected_overload<bForceExpand, Func, Args...>, overload_remove_temporary>>* = nullptr>
		constexpr auto invoke(Func&& func, Args&&... args) return_decltype_MAYTHROW(
			tc_move_if_owned(func)(tc_unwrap_temporary(tc_move_if_owned(args))...)
		)

		template <bool bForceExpand, typename Func, typename ... Args, /* not requires because of CWG issue 2369 */std::enable_if_t<std::same_as<selected_overload<bForceExpand, Func, Args...>, overload_expanded>>* = nullptr>
		constexpr auto invoke(Func&& func, Args&&... args) return_invoke(
			/* ADL to resolve cyclic dependency */expanding_invoke_impl(expanding_invoke_adl::tag,
				tc::repeat_integer_sequence<std::index_sequence<expanded<Args&&>::indices::size()...>, std::index_sequence_for<Args&&...>>{},
				tc::concat_index_sequence<typename expanded<Args&&>::indices...>{},
				tc_move_if_owned(func), tc_move_if_owned(args)...
			)
		)

		namespace expanding_invoke_adl {
			template <std::size_t ... ArgIdxs, std::size_t ... InnerIdxs, typename Func, typename ... Args>
			constexpr auto expanding_invoke_impl(tag_t, std::index_sequence<ArgIdxs...>, std::index_sequence<InnerIdxs...>, Func&& func, Args&&... args) return_invoke(
				tc::invoke_detail::invoke</* bForceExpand */false>(tc_move_if_owned(func),
					tc_prvalue_as_temporary(expanded<boost::mp11::mp_at_c<boost::mp11::mp_list<Args&&...>, ArgIdxs>>::template select<InnerIdxs>(tc::select_nth<ArgIdxs>(tc_move_if_owned(args)...)))...
				)
			)
		}

#pragma pop_macro("return_invoke")
	}

	#define tc_invoke_argument(r, data, i, expr) BOOST_PP_COMMA_IF(i) tc_prvalue_as_temporary(expr)
	#define tc_invoke_argument_list(...) BOOST_PP_SEQ_FOR_EACH_I(tc_invoke_argument, _, BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__))

	// Calls a function, expanding tuple arguments into components (even recursively), if necessary.
	#define tc_invoke(func, ...) (tc::invoke_detail::invoke</* bForceExpand */false>(func, tc_invoke_argument_list(__VA_ARGS__)))
	#define tc_invoke_pack(func, ...) (tc::invoke_detail::invoke</* bForceExpand */false>(func, tc_invoke_argument_list(__VA_ARGS__) ... ))

	// Like tc_invoke, but expands all arguments at least once.
	#define tc_apply(func, ...) (tc::invoke_detail::invoke</* bForceExpand */true>(func, tc_invoke_argument_list(__VA_ARGS__)))
	#define tc_apply_pack(func, ...) (tc::invoke_detail::invoke</* bForceExpand */true>(func, tc_invoke_argument_list(__VA_ARGS__) ... ))

	//////////////////////////////////////////////////////////////////////////
	// concepts
	template <typename Func, typename... Args>
	using invoke_result_t = decltype(tc_invoke_pack(std::declval<Func>(), std::declval<tc::prvalue_as_temporary_t<Args>>()));

	template <typename Func, typename... Args>
	concept invocable = requires { typename invoke_result_t<Func, Args...>; };
	template <typename Func, typename... Args>
	concept nothrow_invocable = tc::invocable<Func, Args...> && noexcept(tc_invoke_pack(std::declval<Func>(), std::declval<tc::prvalue_as_temporary_t<Args>>()));
}
