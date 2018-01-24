//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

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
		constexpr TIndex IdxFirst = I;

		template<typename TIndex, TIndex... Is, typename Func, typename... Args>
		decltype(auto) invoke_with_constant_impl(std::integer_sequence<TIndex, Is...>, Func&& func, TIndex nIndex, Args&&... args) MAYTHROW {
			using result_type = common_reference_t<
#ifdef __clang__
				std::result_of_t<std::decay_t<Func>&(std::integral_constant<TIndex, Is>, Args&&...)>... // C++11, deprecated in C++17
#else
				std::invoke_result_t<std::decay_t<Func>&, std::integral_constant<TIndex, Is>, Args&&...>... // C++17
#endif
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