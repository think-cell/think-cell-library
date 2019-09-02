
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once 

#include "size.h"
#include "type_traits.h"
#include "utility.h"

namespace tc {
	namespace invoke_with_constant_impl {
		template<typename TResult, typename TIndex, TIndex I, typename Func, typename... Args>
		TResult invoke_impl(Func func, Args&&... args) MAYTHROW {
			return func(std::integral_constant<TIndex, I>(), std::forward<Args>(args)...);
		}

		template<typename TIndex, TIndex I, TIndex... Is>
		inline constexpr TIndex IdxFirst = I;

		template<typename TIndex, TIndex... Is, typename Func, typename... Args>
		decltype(auto) invoke_with_constant_impl(std::integer_sequence<TIndex, Is...>, Func&& func, TIndex nIndex, Args&&... args) MAYTHROW {
			using result_type = tc::common_reference_prvalue_as_val_t<
				std::invoke_result_t<std::decay_t<Func>&, std::integral_constant<TIndex, Is>, Args&&...>...
			>;

			static constexpr std::add_pointer_t<result_type(std::remove_reference_t<Func>, Args&&...)> apfn[] = {
				invoke_impl<result_type, TIndex, Is, std::decay_t<Func>, Args...>
				...
			};

			auto const nTableIndex = nIndex - IdxFirst<TIndex, Is...>;
			_ASSERT(0 <= nTableIndex && nTableIndex < tc::size(apfn));

			return apfn[nTableIndex](std::forward<Func>(func), std::forward<Args>(args)...);
		}
	}

	template<typename ContiguousIntegerSequence, typename Func, typename... Args>
	decltype(auto) invoke_with_constant(Func&& func, typename ContiguousIntegerSequence::value_type nIndex, Args&&... args) MAYTHROW {
		static_assert(tc::is_contiguous_integer_sequence<ContiguousIntegerSequence>::value && 0 < ContiguousIntegerSequence::size());

		return invoke_with_constant_impl::invoke_with_constant_impl(ContiguousIntegerSequence(), std::forward<Func>(func), nIndex, std::forward<Args>(args)...);
	}
}