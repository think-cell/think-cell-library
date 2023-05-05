
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/invoke.h"
#include "base/tc_move.h"
#include "range/range_fwd.h"

namespace tc {
	// std::tuple from Microsofts STL is using a recursive implementation which is very slow.
	// We use the same technique to implement tuple as libc++, but omit all constructors
	// (std::tuple has 18 constructors and 14 are conditionally explicit) and use an almost
	// trivial get implementation (at the expense of not being able to do the empty base optimization).
	namespace tuple_member_adl {
		template<std::size_t n, typename T, bool bEmpty = tc::empty_type<T>>
		struct tuple_member;

		template<std::size_t n, typename T>
		struct tuple_member<n, T, /*bEmpty*/false> { T m_t; };

		template<std::size_t n, typename T>
		struct TC_EMPTY_BASES tuple_member<n, T, /*bEmpty*/true> {
			tuple_member() = default;
			constexpr tuple_member(T) noexcept {}
		};

		template<std::size_t n, typename T>
		struct tuple_member<n, T&, /*bEmpty*/false> {
			T& m_t;

			constexpr tuple_member(T& t) noexcept : m_t(t) {}
			constexpr tuple_member(tuple_member const&) noexcept = default;
			constexpr void operator=(tuple_member const& src) /*no &*/ noexcept requires std::is_assignable<T&, T&>::value {
				m_t = src.m_t;
			}
		};

		// Do not provide tuple_member<n, T&&>::operator=(tuple_member &&/const&) for now, i.e., they are implicitly deleted.

		// Structured bindings use adl lookup
#pragma push_macro("GET_IMPL")
#define GET_IMPL(param1, param2, cvref) \
		template<param1, param2> \
		[[nodiscard]] constexpr auto get(tuple_member<n, T, /*bEmpty*/false> cvref elem) noexcept -> T cvref { \
			return static_cast<T cvref>(elem.m_t); \
		}

		GET_IMPL(std::size_t n, typename T, &)
		GET_IMPL(std::size_t n, typename T, &&)
		GET_IMPL(std::size_t n, typename T, const&)
		GET_IMPL(std::size_t n, typename T, const&&)
		GET_IMPL(typename T, std::size_t n, &)
		GET_IMPL(typename T, std::size_t n, &&)
		GET_IMPL(typename T, std::size_t n, const&)
		GET_IMPL(typename T, std::size_t n, const&&)
		// no volatile overloads, like std::get(std::tuple)
#pragma pop_macro("GET_IMPL")

		template<std::size_t n, typename T>
		constexpr T get(tuple_member<n, T, /*bEmpty*/true>) noexcept { return {}; }
		template<typename T, std::size_t n>
		constexpr T get(tuple_member<n, T, /*bEmpty*/true>) noexcept { return {}; }
	}

	namespace tuple_adl {
		template<typename IndexSeq, typename... T>
		struct tc_tuple_impl;

		// Leave this as a class to have a shorter type in function signatures.
		template<typename... T>
		struct tuple : tc_tuple_impl<std::make_index_sequence<sizeof...(T)>, T...> {
			// To initialize every element from a single expression, use this syntax:
			//   tc::tuple<int, int> tuplenn = {1,2};
			// Otherwise, use the "double-brace" syntax:
			//   tc::tuple<X, X> tuplexx = {{ {{a,b}}, {{c, d}} }};
			using tc_tuple_impl<std::make_index_sequence<sizeof...(T)>, T...>::operator=;
		};

		template<std::size_t... n, typename... T>
		struct tc_tuple_impl<std::index_sequence<n...>, T...> : tuple_member_adl::tuple_member<n, T>... {
			// We use std::remove_reference_t<Tuple>::tc_tuple_impl::size to restrict the argument Tuple&& to be tc::tuple in overloads found via adl.
			static constexpr std::size_t size = sizeof...(T);

			template<ENABLE_SFINAE, typename Src> requires (std::tuple_size<std::remove_cvref_t<Src>>::value == sizeof...(T))
			constexpr auto/*=void, returning tuple& may turn xvalue into lvalue*/ operator=(Src&& src) /*no &*/ return_decltype_MAYTHROW(
				(void(tc::get<n>(*SFINAE_VALUE(this)) = tc::get<n>(std::forward<Src>(src))), ...) // assignment MAYTHROW
			)

#pragma push_macro("DEFINE_CONVERSION_OPERATOR")
#define DEFINE_CONVERSION_OPERATOR(cvref) \
			template<typename... U> requires (tc::safely_convertible_to<T cvref, U> && ...) \
			constexpr operator tuple<U...>() cvref noexcept(std::conjunction<std::is_nothrow_constructible<U, T cvref>...>::value) { \
				return {{ {tc::get<n>(static_cast<tc_tuple_impl cvref>(*this))}... }}; \
			}

			DEFINE_CONVERSION_OPERATOR(&)
			DEFINE_CONVERSION_OPERATOR(const&)
			DEFINE_CONVERSION_OPERATOR(&&)
			DEFINE_CONVERSION_OPERATOR(const&&)
#pragma pop_macro("DEFINE_CONVERSION_OPERATOR")
		};

		template<std::size_t... n, typename... T, typename... U>
		[[nodiscard]] constexpr bool operator==(tc_tuple_impl<std::index_sequence<n...>, T...> const& lhs, tc_tuple_impl<std::index_sequence<n...>, U...> const& rhs) noexcept {
			return ((tc::get<n>(lhs) == tc::get<n>(rhs)) && ...);
		}

		template<std::size_t... n, typename... T>
		constexpr void swap(tc_tuple_impl<std::index_sequence<n...>, T&...>&& lhs, tc_tuple_impl<std::index_sequence<n...>, T&...>&& rhs) noexcept {
			static_assert((std::is_nothrow_swappable<T>::value && ...));
			(tc::swap(tc::get<n>(lhs), tc::get<n>(rhs)), ...);
		}

		template<std::size_t... n, typename... T>
		constexpr void swap(tc_tuple_impl<std::index_sequence<n...>, T...>& lhs, tc_tuple_impl<std::index_sequence<n...>, T...>& rhs) noexcept {
			static_assert((std::is_nothrow_swappable<T>::value && ...));
			(tc::swap(tc::get<n>(lhs), tc::get<n>(rhs)), ...);
		}

		// </<= operators defined in compare.h
		// for_each defined in for_each.h
	}
	using tuple_adl::tuple;

	template<typename T>
	using unwrap_ref_decay_t = typename std::unwrap_reference<tc::decay_t<T>>::type;

	// Provide supporting functions that come with std::tuple.
	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<tc::unwrap_ref_decay_t<T>...> make_tuple(T&&... t) MAYTHROW {
		return {{ {tc::unwrap_ref_decay_t<T>(std::forward<T>(t))}... }};
	}

	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<T&&...> forward_as_tuple(T&&... t) noexcept {
		return {{ {std::forward<T>(t)}... }};
	}

	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<T&...> tie(T&... t) noexcept {
		return {{ {t}... }};
	}

	template<typename F, typename... Tuple>
	constexpr auto apply(F&& f, Tuple&&... tuple) return_decltype_xvalue_by_ref_MAYTHROW(
		// Like std::apply uses std::invoke, tc::apply uses tc::invoke.
		tc::expanding_invoke_adl::expanding_invoke_impl(
			tc::expanding_invoke_adl::expand_tag,
			typename tc::invoke_no_adl::expanded_arguments<Tuple...>::index_sequence(),
			std::forward<F>(f),
			std::forward<Tuple>(tuple)...
		)
	)

	template<typename... Tuple>
	[[nodiscard]] constexpr auto tuple_cat(Tuple&&... tuple) MAYTHROW {
		return tc::apply( // MAYTHROW
			[](auto&&... args) noexcept -> tc::type::apply_t<tc::tuple, tc::type::concat_t<typename tc::is_instance<tc::decay_t<Tuple>, tc::tuple>::arguments...>> {
				return {{ {tc_move_if_owned(args)}... }}; // MAYTHROW
			},
			std::forward<Tuple>(tuple)...
		);
	}

	template<typename Tuple, typename Fn>
	[[nodiscard]] constexpr auto tuple_transform(Tuple&& tuple, Fn&& fn) MAYTHROW {
		return tc::apply(
			[fn=std::forward<Fn>(fn)](auto&&... args) MAYTHROW {
				return tc::tuple<decltype(tc::invoke(fn, tc_move_if_owned(args)))...>{{ {tc::invoke(fn, tc_move_if_owned(args))}... }};
			},
			std::forward<Tuple>(tuple)
		);
	}

	namespace tuple_detail {
		template<std::size_t I, tuple_like... Tuple>
		constexpr auto zip_get(Tuple&&... tuple) noexcept {
			// Tuple elements are tc::apply_cvref_t<std::tuple_element_t<I, std::remove_cvref_t<Tuple>>, Tuple>... unless tuple is tc::tuple and element types is empty.
			return tc::tuple<std::conditional_t<
				std::is_rvalue_reference<std::tuple_element_t<I, std::remove_cvref_t<Tuple>>>::value,
				decltype(tc::get<I>(std::declval<Tuple>())),
				tc::remove_rvalue_reference_t<decltype(tc::get<I>(std::declval<Tuple>()))>
			>...>{{
				{tc::get<I>(std::forward<Tuple>(tuple))}...
			}};
		}

		template<std::size_t... I, tuple_like... Tuple>
		constexpr auto zip_impl(std::index_sequence<I...>, Tuple&&... tuple) noexcept {
			return tc::tuple<decltype(tuple_detail::zip_get<I>(std::forward<Tuple>(tuple)...))...>{{ // tc::make_tuple without extra copy
				{tuple_detail::zip_get<I>(std::forward<Tuple>(tuple)...)}...
			}};
		}
	}

	template<tuple_like Tuple0, tuple_like... Tuple>
		requires (!tc::range_with_iterators<Tuple0> || ... || !tc::range_with_iterators<Tuple>) // Prefer zip_adaptor for zip(std::array...)
	[[nodiscard]] constexpr auto zip(Tuple0&& tuple0, Tuple&&... tuple) noexcept {
		static_assert( ((std::tuple_size<std::remove_reference_t<Tuple0>>::value == std::tuple_size<std::remove_reference_t<Tuple>>::value) && ...) );
		return tuple_detail::zip_impl(
			std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple0>>::value>(),
			std::forward<Tuple0>(tuple0),
			std::forward<Tuple>(tuple)...
		);
	}

	template<typename TIndex, TIndex... Is>
	[[nodiscard]] consteval tc::tuple<tc::constant<Is>...> index_sequence_as_tuple(std::integer_sequence<TIndex, Is...>) {
		return {};
	}

	template<tuple_like Tuple> requires (!tc::range_with_iterators<Tuple>)
	[[nodiscard]] constexpr auto enumerate(Tuple&& tuple) noexcept {
		return tc::zip(
			tc::index_sequence_as_tuple(std::make_integer_sequence<int, std::tuple_size<std::remove_reference_t<Tuple>>::value>()),
			std::forward<Tuple>(tuple)
		);
	}


	namespace tuple_detail {
		template<std::size_t n, typename T>
		T std_tuple_element_impl(tuple_member_adl::tuple_member<n, T> const&); // declaration only

		template<typename Tuple>
		concept tuple_or_derived = std::same_as<Tuple, std::remove_cvref_t<Tuple>> && requires { Tuple::tc_tuple_impl::size; };
	}
}

namespace std {
	// Structured bindings use std::tuple_size and std::tuple_element
	template<tc::tuple_detail::tuple_or_derived Tuple>
	struct tuple_size<Tuple> : tc::constant<Tuple::tc_tuple_impl::size> {};

	template<std::size_t n, tc::tuple_detail::tuple_or_derived Tuple>
	struct tuple_element<n, Tuple> {
		using type = decltype(tc::tuple_detail::std_tuple_element_impl<n>(std::declval<Tuple>()));
	};
}
