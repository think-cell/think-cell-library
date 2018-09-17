
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "inherit_ctors.h"
#include "type_traits.h"

#include <utility>

#include <boost/fusion/include/pair.hpp>

namespace tc {
	//////////////////////////////////////////////////////////////////////////
	// make_integer_sequence/make_reverse_integer_sequence

	namespace offset_integer_sequence_impl {
		template<typename TIndex, TIndex IdxFrom, TIndex IdxTo, bool bIncreasing>
		struct offset_integer_sequence final {
		private:
			static_assert(IdxFrom <= IdxTo);

			template<TIndex IdxFirst, TIndex... Is>
			static constexpr std::integer_sequence<TIndex, (bIncreasing ? IdxFirst + Is : IdxFirst - Is)...> make(std::integer_sequence<TIndex, Is...>);
		public:
			using type = decltype(make<(bIncreasing ? IdxFrom : IdxTo - 1)>(std::make_integer_sequence<TIndex, IdxTo - IdxFrom>()));
		};
	}

	template<typename TIndex, TIndex IdxFrom, TIndex IdxTo>
	using make_integer_sequence = typename offset_integer_sequence_impl::offset_integer_sequence<TIndex, IdxFrom, IdxTo, /* bIncreasing */ true>::type;

	template<typename TIndex, TIndex IdxFrom, TIndex IdxTo>
	using make_reverse_integer_sequence = typename offset_integer_sequence_impl::offset_integer_sequence<TIndex, IdxFrom, IdxTo, /* bIncreasing */ false>::type;

	namespace make_integer_sequence_test {
		static_assert(std::is_same<tc::make_integer_sequence<int, -1, 3>, std::integer_sequence<int, -1, 0, 1, 2>>::value);
		static_assert(std::is_same<tc::make_integer_sequence<int, 2, 2>, std::integer_sequence<int>>::value);

		static_assert(std::is_same<tc::make_reverse_integer_sequence<int, -1, 3>, std::integer_sequence<int, 2, 1, 0, -1>>::value);
		static_assert(std::is_same<tc::make_reverse_integer_sequence<int, 2, 2>, std::integer_sequence<int>>::value);
	}

	//////////////////////////////////////////////////////////////////////////
	// is_contiguous_integer_sequence

	namespace is_contiguous_integer_sequence_impl {
		template<typename TIndex, TIndex IFirst, TIndex... Is>
		constexpr std::is_same<std::integer_sequence<TIndex, IFirst, Is...>, tc::make_integer_sequence<TIndex, IFirst, IFirst + sizeof...(Is) + 1>> is_contiguous_integer_sequence(std::integer_sequence<TIndex, IFirst, Is...>);

		template<typename TIndex>
		constexpr std::true_type is_contiguous_integer_sequence(std::integer_sequence<TIndex>);

		constexpr std::false_type is_contiguous_integer_sequence(...);
	}

	template<typename IntSequence>
	using is_contiguous_integer_sequence = decltype(is_contiguous_integer_sequence_impl::is_contiguous_integer_sequence(std::declval<IntSequence>()));

	namespace is_contiguous_integer_sequence_test {
		static_assert(is_contiguous_integer_sequence<std::make_index_sequence<0>>::value);
		static_assert(is_contiguous_integer_sequence<std::make_index_sequence<1>>::value);
		static_assert(is_contiguous_integer_sequence<std::make_index_sequence<2>>::value);
		static_assert(is_contiguous_integer_sequence<std::make_index_sequence<10>>::value);

		static_assert(is_contiguous_integer_sequence<make_integer_sequence<int, 1, 1>>::value);
		static_assert(is_contiguous_integer_sequence<make_integer_sequence<int, 1, 5>>::value);
		static_assert(!is_contiguous_integer_sequence<make_reverse_integer_sequence<int, 1, 3>>::value);

		static_assert(is_contiguous_integer_sequence<std::integer_sequence<int, -2, -1, 0, 1, 2, 3>>::value);
		static_assert(!is_contiguous_integer_sequence<std::integer_sequence<int, 0, 2>>::value);
		static_assert(!is_contiguous_integer_sequence<std::integer_sequence<int, 0, 2, 3>>::value);

		static_assert(!is_contiguous_integer_sequence<int>::value);
	}

	//////////////////////////////////////////////////////////////////////////
	// type_by_index

	template<std::size_t N, typename T0, typename... Ts>
	struct type_by_index final {
		using type = typename type_by_index<N - 1, Ts...>::type;
	};

	template<typename T0, typename... Ts>
	struct type_by_index<0, T0, Ts...> final {
		using type = T0;
	};

	template<std::size_t N, typename... Ts>
	using type_by_index_t = typename type_by_index<N, Ts...>::type;

	//////////////////////////////////////////////////////////////////////////
	// tagged_type

	template<typename TTag, typename T>
	struct tagged_type : boost::fusion::pair<TTag, T> {
		using base_ = boost::fusion::pair<TTag, T>;
		using this_type = tagged_type<TTag, T>;

		tagged_type() = default;
		INHERIT_CTORS_ASSIGN(tagged_type, base_)

		operator T&() & noexcept { return this->second; }
		operator T const&() const& noexcept { return this->second; }
	};

	//////////////////////////////////////////////////////////////////////////
	// INTEGRAL_CONSTANT

	// TODO c++17
	// namespace tc {
	//	template<auto v>
	//	using integral_constant = std::integral_constant<std::remove_const_t<decltype(v)>, v>;
	// }
	//
	// USAGE:
	// tc::integral_constant<tc::break_> etc.
	#define INTEGRAL_CONSTANT(val) std::integral_constant<tc::decay_t<decltype(val)>, val>
}