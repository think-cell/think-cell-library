
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_fwd.h"
#include "assign.h"
#include "size.h"
#include "conditional.h"

namespace tc {
	namespace no_adl {	
		template<typename Better>
		struct best_impl final {

			template<typename BetterRef>
			constexpr best_impl(BetterRef&& better) : m_better(std::forward<BetterRef>(better))
			{}

		private:
			static_assert( tc::is_decayed<Better>::value );
			Better m_better;
			
			// Allow best(x, y) to work where x and y have no common reference type,
			// if m_better(x, y) is known statically.
			// These overloads should be lambdas passed to make_overload in operator(),
			// but that caused LLDB to crash (mrabkin, 2018-01-26).
			template<typename T0, typename T1, typename... Args>
			constexpr auto dispatch(std::true_type, T0&& t0, T1&& t1, Args&&... args) const& noexcept -> decltype(auto) {
				return operator()(std::forward<T0>(t0), std::forward<Args>(args)...);
			}
			
			template<typename T0, typename T1, typename... Args>
			constexpr auto dispatch(std::false_type, T0&& t0, T1&& t1, Args&&... args) const& noexcept -> decltype(auto) {
				return operator()(std::forward<T1>(t1), std::forward<Args>(args)...);
			}
			
			template<typename T0, typename T1, typename... Args>
			constexpr auto dispatch(bool b, T0&& t0, T1&& t1, Args&&... args) const& noexcept -> decltype(auto) {
				return CONDITIONAL_PRVALUE_AS_VAL(
					b,
					operator()(std::forward<T0>(t0), std::forward<Args>(args)...),
					operator()(std::forward<T1>(t1), std::forward<Args>(args)...)
				);
			}

		public:
			template<typename T>
			constexpr auto operator()(T&& t) const& noexcept -> T&& {
				return std::forward<T>(t);
			}
			
			template<typename T0, typename T1, typename... Args>
			constexpr auto operator()(T0&& t0, T1&& t1, Args&&... args) const& noexcept -> decltype(auto) {
				return dispatch(m_better(t0, t1), std::forward<T0>(t0), std::forward<T1>(t1), std::forward<Args>(args)...);
			}
		};
	}

	template<
		typename Better,
		typename... Args
	>
	constexpr auto best(Better&& better, Args&&... args) return_decltype_xvalue_by_ref(
		no_adl::best_impl<tc::decay_t<Better>>(std::forward<Better>(better))(std::forward<Args>(args)...)
	)

	template<typename... Args>
	constexpr auto min(Args&&... args) return_decltype_xvalue_by_ref(
		best(tc::fn_less(), std::forward<Args>(args)...)
	)

	template<typename... Args>
	constexpr auto max(Args&&... args) return_decltype_xvalue_by_ref(
		best(tc::fn_greater(), std::forward<Args>(args)...)
	)

	DEFINE_FN(min);
	std::true_type returns_reference_to_argument(tc::fn_min) noexcept;

	DEFINE_FN(max);
	std::true_type returns_reference_to_argument(tc::fn_max) noexcept;
}

