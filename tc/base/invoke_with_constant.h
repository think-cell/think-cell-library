
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once 

#include "type_traits_fwd.h"
#include "utility.h"
#include "../algorithm/size.h"
#include "../range/iota_range.h"

namespace tc {
	namespace invoke_with_constant_impl {
		template<typename TResult, typename TIndex, TIndex I, typename Func, typename... Args>
		TResult invoke_impl(Func func, Args&&... args) MAYTHROW {
			return func(tc::constant<I>(), tc_move_if_owned(args)...);
		}

		template<typename TIndex, TIndex I, TIndex... Is>
		inline constexpr TIndex IdxFirst = I;

		template<typename T>
		struct invoke_with_constant_impl {};

		template<typename TIndex, TIndex... Is>
		struct invoke_with_constant_impl<std::integer_sequence<TIndex, Is...>> {
			template<typename Func, typename... Args>
			struct inner {
				using result_type = tc::common_reference_t<
					decltype(std::declval<std::decay_t<Func>&>()(tc::constant<Is>(), std::declval<Args>()...))...
				>;

				template<TIndex I, TIndex... IsRemaining>
				static result_type constexpr invoke_constexpr(Func&& func, TIndex nIndex, Args&&... args) MAYTHROW {
					if (I == nIndex) {
						return func(tc::constant<I>(), tc_move_if_owned(args)...); // MAYTHROW
					} else if constexpr (0 < sizeof...(IsRemaining)) {
						return invoke_constexpr<IsRemaining...>(tc_move_if_owned(func), nIndex, tc_move_if_owned(args)...); // MAYTHROW
					} else {
						_ASSERTFALSE;
						return func(tc::constant<I>(), tc_move_if_owned(args)...);
					}
				}

				static result_type constexpr invoke_constexpr_outer(Func&& func, TIndex nIndex, Args&&... args) MAYTHROW {
					return invoke_constexpr<Is...>(tc_move_if_owned(func), nIndex, tc_move_if_owned(args)...); // MAYTHROW (but won't because constexpr)
				}

			private:
				// TODO: move back as static local after MSVC compiler bug is fixed: https://developercommunity.visualstudio.com/t/code-generation-bug-on-static-variable-i/10541326
				// tc_static_auto_constexpr with std::array triggers pdb problem.
				static constexpr std::add_pointer_t<result_type(std::remove_reference_t<Func>, Args&&...)> c_apfn[] = {
					invoke_impl<result_type, TIndex, Is, std::decay_t<Func>, Args...>
					...
				};
			public:
				static result_type invoke_non_constexpr(Func&& func, TIndex nIndex, Args&&... args) MAYTHROW {
					auto const nTableIndex = nIndex - IdxFirst<TIndex, Is...>;
					_ASSERTNORETURN(0 <= nTableIndex && nTableIndex < tc::size(c_apfn));

					return c_apfn[nTableIndex](tc_move_if_owned(func), tc_move_if_owned(args)...); // MAYTHROW
				}
			};
		};
	}

	template<typename ContiguousIntegerSequence, typename Func, typename... Args>
	decltype(auto) constexpr invoke_with_constant(Func&& func, typename ContiguousIntegerSequence::value_type nIndex, Args&&... args) MAYTHROW {
		static_assert(tc::is_contiguous_integer_sequence<ContiguousIntegerSequence>::value);

		if constexpr (0 < ContiguousIntegerSequence::size()) {
			using impl_type = typename invoke_with_constant_impl::invoke_with_constant_impl<ContiguousIntegerSequence>::template inner<Func, Args...>;
			if (std::is_constant_evaluated()) {
				return impl_type::invoke_constexpr_outer(tc_move_if_owned(func), nIndex, tc_move_if_owned(args)...);
			} else {
				return impl_type::invoke_non_constexpr(tc_move_if_owned(func), nIndex, tc_move_if_owned(args)...);
			}
		}
	}

	template<typename Enum, typename Func, typename... Args>
	decltype(auto) invoke_with_constant(Func func, Enum e, Args&&... args) MAYTHROW {
		return tc::invoke_with_constant<std::make_index_sequence<tc::size(tc::all_values<Enum>())>>(
			[&](auto constn) MAYTHROW -> decltype(auto) {
				return tc_invoke_pack(func, tc::constant<tc_at_nodebug(tc::all_values<Enum>(), decltype(constn)::value)>(), tc_move_if_owned(args));
			},
			tc::all_values<Enum>().index_of(e)
		);
	}
}
