
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <utility>

// TODO c++20 consteval
#define TC_CONSTEVAL constexpr

namespace tc {
#ifdef __clang__
	namespace no_adl {
		// If {template< typename... Args > using void_t = void;} is used, the test fails to compile with:
		// error: redefinition of 'Foo<type-parameter-0-0, void>'
		//	struct Foo<T, void_t<typename T::type2>> {
		//         ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// note: previous definition is here
		//	struct Foo<T, void_t<typename T::type1>> {
		//         ^
		//------------------------------
		// see also https://bugs.llvm.org/show_bug.cgi?id=26086
		// The implementation at http://en.cppreference.com/w/cpp/types/void_t solves the problem
		template<typename... Args>
		struct make_void {
			using type = void;
		};
	}

	template< typename... Args >
	using void_t = typename no_adl::make_void<Args...>::type;
#else
	using std::void_t;
#endif

	namespace type {
		namespace no_adl {
			template<template<typename...> typename Func, typename Lhs>
			struct curry final {
				template<typename Rhs>
				using type = Func<Lhs, Rhs>;
			};

			template<template<typename...> typename Func, typename Rhs>
			struct rcurry final {
				template<typename Lhs>
				using type = Func<Lhs, Rhs>;
			};

			template<typename T>
			struct identity {
				using type = T;
			};
		}
		using no_adl::curry;
		using no_adl::rcurry;
		using no_adl::identity;

		template<typename T>
		using deducible_identity_t = T;

		namespace no_adl {
			template<typename... T>
			struct list final {};

			template<typename List>
			struct size;

			template<typename... T>
			struct size<list<T...>> final {
				static constexpr auto value = sizeof...(T);
			};

			template<typename List, std::size_t N>
			struct at;

			template<typename T0, typename... T>
			struct at<list<T0, T...>, 0> final {
				using type = T0;
			};

			template<typename T0, typename... T, std::size_t N>
			struct at<list<T0, T...>, N> final {
				using type = typename at<list<T...>, N-1>::type;
			};

			template<typename List>
			struct front;

			template<typename T0, typename... T>
			struct front<list<T0, T...>> final {
				using type = T0;
			};

			template<typename List>
			struct only;

			template<typename T>
			struct only<list<T>> final {
				using type = T;
			};
		}
		using no_adl::list;
		using no_adl::size;
		using no_adl::at;
		using no_adl::front;
		using no_adl::only;

		template<typename List, std::size_t N>
		using at_t = typename at<List, N>::type;

		template<typename List>
		using front_t = typename front<List>::type;

		template<typename List>
		using only_t = typename only<List>::type;

		namespace no_adl {
			template<typename... List>
			struct concat;

			template<>
			struct concat<> final {
				using type = list<>;
			};

			template<typename... T>
			struct concat<list<T...>> final {
				using type = list<T...>;
			};

			template<typename... T, typename... U, typename... List>
			struct concat<list<T...>, list<U...>, List...> final {
				using type = typename concat<list<T..., U...>, List...>::type;
			};
		}
		using no_adl::concat;

		template<typename... List>
		using concat_t = typename concat<List...>::type;

		namespace no_adl {
			template<typename List>
			struct join;

			template<typename... List>
			struct join<list<List...>> final {
				using type = concat_t<List...>;
			};
		}
		using no_adl::join;

		template<typename List>
		using join_t = typename join<List>::type;

		namespace no_adl {
			template<typename List, typename T>
			struct push_front;

			template<typename... T, typename T0>
			struct push_front<list<T...>, T0> final {
				using type = list<T0, T...>;
			};

			template<typename List, template<typename...> typename Pred>
			struct partition;

			template<template<typename...> typename Pred>
			struct partition<list<>, Pred> final {
				using true_part = list<>;
				using false_part = list<>;
			};

			template<typename T0, typename... T, template<typename...> typename Pred>
			struct partition<list<T0, T...>, Pred> final {
			private:
				using tail_partition = partition<list<T...>, Pred>;
				using pair = typename std::conditional<Pred<T0>::value,
					std::pair<
						typename push_front<typename tail_partition::true_part, T0>::type,
						typename tail_partition::false_part
					>,
					std::pair<
						typename tail_partition::true_part,
						typename push_front<typename tail_partition::false_part, T0>::type
					>
				>::type;

			public:
				using true_part = typename pair::first_type;
				using false_part = typename pair::second_type;
			};

			template<typename List, template<typename...> typename Pred>
			struct filter final {
				using type = typename partition<List, Pred>::true_part;
			};
		}
		using no_adl::push_front;
		using no_adl::partition;
		using no_adl::filter;

		template<typename List, typename T>
		using push_front_t = typename push_front<List, T>::type;

		template<typename List, template<typename...> typename Pred>
		using filter_t = typename filter<List, Pred>::type;

		namespace find_unique_if_result {
			namespace no_adl {
				struct type_not_found final {
					static constexpr auto found = false;
				};

				template<typename T, std::size_t I>
				struct unique_type final {
					using type = T;
					static constexpr auto index = I;
					static constexpr auto found = true;
				};
			}
			using no_adl::type_not_found;
			using no_adl::unique_type;
		};

		namespace no_adl {
			template<typename List, template<typename...> typename Pred, std::size_t I>
			struct find_unique_if_impl;

			template<template<typename...> typename Pred, std::size_t I>
			struct find_unique_if_impl<list<>, Pred, I> final {
				using result_type = find_unique_if_result::type_not_found;
			};

			template<typename T0, typename... T, template<typename...> typename Pred, std::size_t I>
			struct find_unique_if_impl<list<T0, T...>, Pred, I> final {
				using tail_result_type = typename find_unique_if_impl<list<T...>, Pred, I+1>::result_type;
				using pred = Pred<T0>;
				using result_type = typename std::conditional<pred::value,
					find_unique_if_result::unique_type<T0, I>,
					tail_result_type
				>::type;

				static_assert(
					!pred::value || std::is_same<find_unique_if_result::type_not_found, tail_result_type>::value,
					"There must be only one type matching the predicate"
				);
			};
		}

		template<typename List, template<typename...> typename Pred>
		using find_unique_if = typename no_adl::find_unique_if_impl<List, Pred, 0>::result_type;

		template<typename List, template<typename...> typename Pred>
		using find_unique_if_t = typename find_unique_if<List, Pred>::type;

		template<typename List, typename T>
		using find_unique = find_unique_if<List, curry<std::is_same, T>::template type>;

		namespace no_adl {
			template<typename List, template<typename...> typename Pred>
			struct all_of;

			template<typename... T, template<typename...> typename Pred>
			struct all_of<list<T...>, Pred> final : std::conjunction<Pred<T>...> {};
		}
		using no_adl::all_of;

		namespace no_adl {
			template<typename List, template<typename...> typename Pred>
			struct any_of;

			template<typename... T, template<typename...> typename Pred>
			struct any_of<list<T...>, Pred> final: std::disjunction<Pred<T>...> {};
		}
		using no_adl::any_of;

		namespace no_adl {
			template<typename List, template<typename...> typename F, typename Enable=void>
			struct transform;

			template<typename... T, template<typename...> typename F>
			struct transform<list<T...>, F, tc::void_t<list<F<T>...>>> final {
				using type = list<F<T>...>;
			};
		}
		using no_adl::transform;

		template<typename List, template<typename...> typename F>
		using transform_t = typename transform<List, F>::type;

		namespace no_adl {
			template<template<typename...> typename F, typename List, typename = void>
			struct apply;

			template<template<typename...> typename F, typename... T>
			struct apply<F, list<T...>, std::void_t<F<T...>>> final {
				using type = F<T...>;
			};
		}
		using no_adl::apply;

		template<template<typename...> typename F, typename List>
		using apply_t = typename apply<F, List>::type;

		namespace no_adl {
			template<typename T0, typename List, template<typename...> typename F, typename Enable=void>
			struct accumulate {};

			template<typename T0, template<typename...> typename F>
			struct accumulate<T0, list<>, F> {
				using type = T0;
			};

			template<typename T0, typename T1, typename... T, template<typename...> typename F>
			struct accumulate<T0, list<T1, T...>, F, tc::void_t<typename accumulate<F<T0, T1>, list<T...>, F>::type>> {
				using type = typename accumulate<F<T0, T1>, list<T...>, F>::type;
			};

			template<typename List, template<typename...> typename F>
			struct accumulate_with_front;

			// Existing use cases do not do the right thing for single element lists.
			// Before extending this function to single element lists (by simply removing T1), check the use cases.
			template<typename T0, typename T1, typename... T, template<typename...> typename F>
			struct accumulate_with_front<list<T0, T1, T...>, F> : accumulate<T0, list<T1, T...>, F> {};
		}
		using no_adl::accumulate;
		using no_adl::accumulate_with_front;

		template<typename T0, typename List, template<typename...> typename F>
		using accumulate_t = typename no_adl::accumulate<T0, List, F>::type;

		template<typename List, template<typename...> typename F>
		using accumulate_with_front_t = typename no_adl::accumulate_with_front<List, F>::type;

		namespace no_adl {
			template<typename List, std::size_t N, typename Enable=void>
			struct take_first;

			template<typename... T>
			struct take_first<list<T...>, 0, void> final {
				using type = list<>;
			};

			template<typename T0, typename... T, std::size_t N>
			struct take_first<list<T0, T...>, N, std::enable_if_t<0<N && N<=size<list<T0, T...>>::value>> final {
				using type = concat_t<list<T0>, typename take_first<list<T...>, N-1>::type>;
			};
		}

		template<typename List, std::size_t N=1>
		using take_first_t = typename no_adl::take_first<List, N>::type;

		namespace no_adl {
			template<typename List, std::size_t N, typename Enable=void>
			struct drop_first;

			template<typename... T>
			struct drop_first<list<T...>, 0, void> final {
				using type = list<T...>;
			};

			template<typename T0, typename... T, std::size_t N>
			struct drop_first<list<T0, T...>, N, std::enable_if_t<0<N && N<=size<list<T0, T...>>::value>> final {
				using type = typename drop_first<list<T...>, N-1>::type;
			};
		}

		template<typename List, std::size_t N=1>
		using drop_first_t = typename no_adl::drop_first<List, N>::type;

		template<typename List, std::size_t N=1>
		using take_last_t = drop_first_t<List, size<List>::value-N>;

		template<typename List, std::size_t N=1>
		using drop_last_t = take_first_t<List, size<List>::value-N>;

		namespace no_adl {
			template<typename... Lists>
			struct cartesian_product;
		}
		template<typename... Lists>
		using cartesian_product_t = typename no_adl::cartesian_product<Lists...>::type;

		namespace no_adl {
			template<>
			struct cartesian_product<> final {
				using type = list<list<>>;
			};

			template<typename List, typename... Lists>
			struct cartesian_product<List, Lists...> final {
			private:
				template<typename T>
				using product_for_element = transform_t<cartesian_product_t<Lists...>, rcurry<push_front_t, T>::template type>;
			public:
				using type = join_t<transform_t<List, product_for_element>>;
			};
		}

		namespace no_adl {
			template<std::size_t N, typename T>
			struct repeat_n;
		}

		template<std::size_t N, typename T>
		using repeat_n_t = typename no_adl::repeat_n<N, T>::type;

		namespace no_adl {
			template<typename T>
			struct repeat_n<0, T> {
				using type = list<>;
			};
			template<typename T>
			struct repeat_n<1, T> {
				using type = list<T>;
			};
			template<std::size_t N, typename T>
			struct repeat_n {
				using type = concat_t<repeat_n_t<(N + 1) / 2, T>, repeat_n_t<N / 2, T>>;
			};
		}
	}
}