
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <utility>
#include <boost/mp11/algorithm.hpp>
#include <boost/mp11/bind.hpp>
#include <boost/mp11/list.hpp>
#include <boost/mp11/set.hpp>

namespace tc {
	template <typename T>
	using mp_identity = T;

	namespace no_adl {
		template <typename List>
		struct mp_only_impl {};
		template <template <typename...> typename List, typename T>
		struct mp_only_impl<List<T>> {
			using type = T;
		};
	}
	template <typename List>
	using mp_only = typename no_adl::mp_only_impl<List>::type;

	// F1<F2<F3<...Fn<Args>...>>>
	template <template <typename...> typename ... F>
	using mp_chained = boost::mp11::mp_reverse<boost::mp11::mp_compose_q<boost::mp11::mp_quote<F>...>>;

	// Boost.MP11's mp_transform is not SFINAE-friendly
	namespace no_adl {
		template <template <typename...> typename F, typename ... List>
		struct mp_transform_impl {
			static_assert(sizeof...(List) == 1, "TODO: implement variadic mp_transform when we need it");
		};

		template <template <typename...> typename F, template <typename...> typename List, typename ... T>
			requires requires { typename List<F<T>...>; }
		struct mp_transform_impl<F, List<T...>> final {
			using type = List<F<T>...>;
		};
	}
	template <template <typename...> typename F, typename ... List>
	using mp_transform = typename no_adl::mp_transform_impl<F, List...>::type; 

	template<typename ... List>
	using mp_zip = boost::mp11::mp_transform<boost::mp11::mp_list, List...>; // our transform isn't variadic yet and we don't need SFINAE friendlieness

	namespace no_adl {
		template<typename IntSeq>
		struct mp_integer_list_impl;

		template<typename TIndex, TIndex... Is>
		struct mp_integer_list_impl<std::integer_sequence<TIndex, Is...>> {
			using type = boost::mp11::mp_list<std::integral_constant<TIndex, Is>...>;
		};
	}
	template<typename IntSeq>
	using mp_integer_list = typename no_adl::mp_integer_list_impl<IntSeq>::type;

	template <typename Head, typename ... Tail>
	using mp_enumerate = mp_zip<
		// We pick the size of the first list, mp_zip is only defined if all lists have the same size.
		mp_integer_list<std::make_integer_sequence<int, boost::mp11::mp_size<Head>::value>>,
		Head, Tail...
	>;

	// Boost.MP11's mp_fold is not SFINAE-friendly
	namespace no_adl {
		template <typename List, typename V, template <typename...> typename F>
		struct mp_fold_impl {};

		template <template <typename...> typename List, typename V, template <typename...> typename F>
		struct mp_fold_impl<List<>, V, F> {
			using type = V;
		};

		template <template <typename...> typename List, typename T0, typename ... T, typename V, template <typename...> typename F>
			requires requires { typename F<V, T0>; }
		struct mp_fold_impl<List<T0, T...>, V, F> : mp_fold_impl<List<T...>, F<V, T0>, F> {};
	}
	template <typename List, typename V, template <typename...> typename F>
	using mp_fold = typename no_adl::mp_fold_impl<List, V, F>::type;

	template <typename List, template <typename...> typename F>
	using mp_fold_with_front = mp_fold<boost::mp11::mp_pop_front<List>, boost::mp11::mp_front<List>, F>;

	namespace no_adl {
		template <typename List, typename V>
		struct mp_find_unique : boost::mp11::mp_size<List> {
			static auto constexpr found = false;
		};
		// Note: mp_set_contains will cause a hard-error if List contains the same value multiple times, so no need to check unique match.
		template <typename List, typename V> requires boost::mp11::mp_set_contains<List, V>::value
		struct mp_find_unique<List, V> : boost::mp11::mp_find<List, V> {
			static auto constexpr found = true;
			static auto constexpr index = boost::mp11::mp_find<List, V>::value;
		};

		template <typename List, template<typename...> typename P>
		struct mp_find_unique_if : boost::mp11::mp_size<List> {
			static auto constexpr found = false;
		};
		template <typename List, template <typename...> typename P> requires (0 < boost::mp11::mp_count_if<List, P>::value)
		struct mp_find_unique_if<List, P> : boost::mp11::mp_find_if<List, P> {
			static_assert(1 == boost::mp11::mp_count_if<List, P>::value);
			static auto constexpr found = true;
			static auto constexpr index = boost::mp11::mp_find_if<List, P>::value;
		};
	}
	using no_adl::mp_find_unique;
	using no_adl::mp_find_unique_if;

	namespace no_adl {
		template <typename Lhs, typename Rhs>
		struct mp_common_prefix_impl { // no prefix, different kinds of list
			using type = boost::mp11::mp_list<>;
		};

		template <template <typename...> typename List, typename ... Lhs, typename ... Rhs>
		struct mp_common_prefix_impl<List<Lhs...>, List<Rhs...>> { // no prefix, same kind of list
			using type = List<>;
		};

		template <typename Head, template <typename...> typename LhsList, typename ... Lhs, template <typename...> typename RhsList, typename ... Rhs>
		struct mp_common_prefix_impl<LhsList<Head, Lhs...>, RhsList<Head, Rhs...>> { // prefix, different kinds of list
			using type = boost::mp11::mp_push_front<typename mp_common_prefix_impl<LhsList<Lhs...>, RhsList<Rhs...>>::type, Head>;
		};

		template <typename Head, template <typename...> typename List, typename ... Lhs, typename ... Rhs>
		struct mp_common_prefix_impl<List<Head, Lhs...>, List<Head, Rhs...>> { // prefix, same kind of list (to resolve ambiguity)
			using type = boost::mp11::mp_push_front<typename mp_common_prefix_impl<List<Lhs...>, List<Rhs...>>::type, Head>;
		};
	}

	template <typename ... List>
	using mp_common_prefix = mp_fold_with_front<boost::mp11::mp_list<List...>, boost::mp11::mp_quote_trait<no_adl::mp_common_prefix_impl>::template fn>;

	template <typename ... List>
	using mp_common_suffix = boost::mp11::mp_reverse<mp_common_prefix<boost::mp11::mp_reverse<List>...>>;
}
