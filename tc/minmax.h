
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
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
			constexpr best_impl(BetterRef&& better) noexcept : m_better(std::forward<BetterRef>(better))
			{}

		private:
			static_assert( tc::is_decayed<Better>::value );
			Better m_better;

		public:
			template<typename T>
			constexpr auto operator()(T&& t) const& noexcept -> T&& {
				return std::forward<T>(t);
			}
			
			template<typename T0, typename T1, typename... Args>
			constexpr auto operator()(T0&& t0, T1&& t1, Args&&... args) const& noexcept -> decltype(auto) {
				// analogous to std::min/std::max: if equivalent, return the first parameter
				auto b = m_better(tc::as_const(t1), tc::as_const(t0));
				if constexpr (std::is_same<std::true_type, decltype(b)>::value) {
					// t1 is better
					return operator()(std::forward<T1>(t1), std::forward<Args>(args)...);
				} else if constexpr (std::is_same<std::false_type, decltype(b)>::value) {
					// t0 is better or equal
					return operator()(std::forward<T0>(t0), std::forward<Args>(args)...);
				} else {
					STATICASSERTSAME(decltype(b), bool);
					return CONDITIONAL_PRVALUE_AS_VAL(
						b,
						/*t1 is better*/operator()(std::forward<T1>(t1), std::forward<Args>(args)...),
						/*t0 is better or equal*/operator()(std::forward<T0>(t0), std::forward<Args>(args)...)
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
		no_adl::best_impl<tc::decay_t<Better>>(std::forward<Better>(better))(std::forward<Args>(args)...)
	)

	template<typename... Args>
	[[nodiscard]] constexpr auto min(Args&&... args) return_decltype_xvalue_by_ref_noexcept(
		best(tc::fn_less(), std::forward<Args>(args)...)
	)

	template<typename... Args>
	[[nodiscard]] constexpr auto max(Args&&... args) return_decltype_xvalue_by_ref_noexcept(
		best(tc::fn_greater(), std::forward<Args>(args)...)
	)

	DEFINE_FN(min);
	std::true_type returns_reference_to_argument(tc::fn_min) noexcept;

	DEFINE_FN(max);
	std::true_type returns_reference_to_argument(tc::fn_max) noexcept;

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
		return no_adl::forward_iter<std::remove_reference_t<Iter>>{std::forward<Iter>(iter)};
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
			std::forward<Pred>(pred)
		);
	}
}

