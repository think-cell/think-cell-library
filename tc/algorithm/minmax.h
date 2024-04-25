
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/conditional.h"
#include "../base/change.h"
#include "size.h"

namespace tc {
	namespace no_adl {	
		template <typename Better>
		struct TC_EMPTY_BASES fn_best : private std::remove_cvref_t<Better> {
			constexpr fn_best() noexcept = default;
			template<typename BetterRef>
			constexpr fn_best(BetterRef&& better) noexcept : std::remove_cvref_t<Better>(tc_move_if_owned(better))
			{}

			template <typename T>
			[[nodiscard]] constexpr decltype(auto) operator()(T&& t) const& noexcept {
				return tc_move_if_owned(t);
			}
			
			template <typename T0, typename T1, typename... Args>
			[[nodiscard]] constexpr decltype(auto) operator()(T0&& t0, T1&& t1, Args&&... args) const& noexcept {
				// analogous to std::min/std::max: if equivalent, return the first parameter
				auto b = tc_invoke(static_cast<std::remove_cvref_t<Better> const&>(*this), tc::as_const(t1), tc::as_const(t0));
				if constexpr (std::is_same<tc::constant<true>, decltype(b)>::value) {
					// t1 is better
					return operator()(tc_move_if_owned(t1), tc_move_if_owned(args)...);
				} else if constexpr (std::is_same<tc::constant<false>, decltype(b)>::value) {
					// t0 is better or equal
					return operator()(tc_move_if_owned(t0), tc_move_if_owned(args)...);
				} else {
					STATICASSERTSAME(decltype(b), bool);
					return tc_conditional_prvalue_as_val(
						b,
						/*t1 is better*/operator()(tc_move_if_owned(t1), tc_move_if_owned(args)...),
						/*t0 is better or equal*/operator()(tc_move_if_owned(t0), tc_move_if_owned(args)...)
					);
				}
			}
		};
	}
	using no_adl::fn_best;
	using fn_min = fn_best<tc::fn_less>;
	using fn_max = fn_best<tc::fn_greater>;

	template <typename Better, typename ... Args>
	[[nodiscard]] constexpr auto best(Better&& better, Args&&... args) return_decltype_allow_xvalue_noexcept(
		fn_best<std::remove_cvref_t<Better>>(tc_move_if_owned(better))(tc_move_if_owned(args)...)
	)

	template <typename ... Args>
	[[nodiscard]] constexpr auto min(Args&&... args) return_decltype_allow_xvalue_noexcept(
		best(tc::fn_less(), tc_move_if_owned(args)...)
	)

	template <typename ... Args>
	[[nodiscard]] constexpr auto max(Args&&... args) return_decltype_allow_xvalue_noexcept(
		best(tc::fn_greater(), tc_move_if_owned(args)...)
	)
}

