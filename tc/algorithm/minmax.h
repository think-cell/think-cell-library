
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/conditional.h"
#include "../base/assign.h"
#include "size.h"

namespace tc {
	namespace no_adl {	
		template<typename Better>
		struct best_impl final {

			template<typename BetterRef>
			constexpr best_impl(BetterRef&& better) noexcept : m_better(tc_move_if_owned(better))
			{}

		private:
			static_assert(tc::decayed<Better>);
			Better m_better;

		public:
			template<typename T>
			constexpr auto operator()(T&& t) const& noexcept -> T&& {
				return tc_move_if_owned(t);
			}
			
			template<typename T0, typename T1, typename... Args>
			constexpr auto operator()(T0&& t0, T1&& t1, Args&&... args) const& noexcept -> decltype(auto) {
				// analogous to std::min/std::max: if equivalent, return the first parameter
				auto b = m_better(tc::as_const(t1), tc::as_const(t0));
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

	template<
		typename Better,
		typename... Args
	>
	[[nodiscard]] constexpr auto best(Better&& better, Args&&... args) return_decltype_xvalue_by_ref_noexcept(
		no_adl::best_impl<tc::decay_t<Better>>(tc_move_if_owned(better))(tc_move_if_owned(args)...)
	)

	template<typename... Args>
	[[nodiscard]] constexpr auto min(Args&&... args) return_decltype_xvalue_by_ref_noexcept(
		best(tc::fn_less(), tc_move_if_owned(args)...)
	)

	template<typename... Args>
	[[nodiscard]] constexpr auto max(Args&&... args) return_decltype_xvalue_by_ref_noexcept(
		best(tc::fn_greater(), tc_move_if_owned(args)...)
	)

	tc_define_fn(min);
	tc::constant<true> returns_reference_to_argument(tc::fn_min) noexcept;

	tc_define_fn(max);
	tc::constant<true> returns_reference_to_argument(tc::fn_max) noexcept;

	namespace no_adl {
		template <typename Iter>
		struct forward_iter : Iter {
			explicit forward_iter(Iter const& iter) : Iter(iter) {}
		};
	}
}

namespace std {
	template <typename BaseIter>
	struct iterator_traits<tc::no_adl::forward_iter<BaseIter>> : iterator_traits<BaseIter> {
		using iterator_category = std::forward_iterator_tag;
	};
}

namespace tc {
	template <typename Iter>
	auto treat_as_forward_iterator(Iter&& iter) noexcept {
		return no_adl::forward_iter<std::remove_reference_t<Iter>>{tc_move_if_owned(iter)};
	}

	template <typename T>
	auto treat_as_forward_iterator(T* ptr) noexcept {
		return ptr;
	}

	template<typename Rng>
	[[nodiscard]] auto minmax_element(Rng& rng) noexcept {
		return std::minmax_element(
			treat_as_forward_iterator(tc::begin(rng)),
			treat_as_forward_iterator(tc::end(rng))
		);
	}

	template<typename Rng, typename Pred>
	[[nodiscard]] auto minmax_element(Rng& rng, Pred&& pred) noexcept {
		return std::minmax_element(
			treat_as_forward_iterator(tc::begin(rng)),
			treat_as_forward_iterator(tc::end(rng)),
			tc_move_if_owned(pred)
		);
	}
}

