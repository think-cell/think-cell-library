
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "invoke.h"

namespace tc {
	// std::tuple from Microsofts STL is using a recursive implementation which is very slow.
	// We use the same technique to implement tuple as libc++, but omit all constructors
	// (std::tuple has 18 constructors and 14 are conditionally explicit) and use an almost
	// trivial get implementation (at the expense of not being able to do the empty base optimization).
	namespace tuple_adl {
		// tc::tuple must not derive from T, because that would make tc::tuple<T> convertible to T.
		// Even private inheritance is not a viable option, because these conversions are still considered during
		// overload resolution and cause unexpected template instantiations.
		// The C++20 [[no_unique_address]] attribute will enable the empty base optimization for this implementation.
		template<std::size_t n, typename T>
		struct tuple_member { T m_t; };

		// Structured bindings use adl lookup
#pragma push_macro("GET_IMPL")
#define GET_IMPL(param1, param2, cvref) \
		template<param1, param2> \
		[[nodiscard]] constexpr auto get(tuple_member<n, T> cvref elem) noexcept -> T cvref { \
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
		// no volatile overloads, like std::get
#pragma pop_macro("GET_IMPL")
	}
	using tuple_adl::get;

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
		struct tc_tuple_impl<std::index_sequence<n...>, T...> : tuple_member<n, T>... {
			// We use std::remove_reference_t<Tuple>::tc_tuple_impl::size to restrict the argument Tuple&& to be tc::tuple in overloads found via adl.
			static constexpr std::size_t size = sizeof...(T);

			template<ENABLE_SFINAE, typename Src, std::enable_if_t<std::tuple_size<tc::remove_cvref_t<Src>>::value == sizeof...(T)>* = nullptr>
			constexpr auto/*=void, returning tuple& may turn xvalue into lvalue*/ operator=(Src&& src) /*no &*/ return_decltype_MAYTHROW(
				(void(tc::get<n>(*SFINAE_VALUE(this)) = /*adl*/get<n>(std::forward<Src>(src))), ...) // assignment MAYTHROW
			)

#pragma push_macro("DEFINE_CONVERSION_OPERATOR")
#define DEFINE_CONVERSION_OPERATOR(cvref) \
			template<typename... U, std::enable_if_t< \
				std::conjunction<tc::is_safely_convertible<T cvref, U>...>::value \
			>* = nullptr> \
			operator tuple<U...>() cvref noexcept(std::conjunction<std::is_nothrow_constructible<U, T cvref>...>::value) { \
				return {{ {{tc::get<n>(static_cast<tc_tuple_impl cvref>(*this))}}... }}; \
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

		template<typename IndexSeq, typename... T, typename... U>
		[[nodiscard]] constexpr bool operator!=(tc_tuple_impl<IndexSeq, T...> const& lhs, tc_tuple_impl<IndexSeq, U...> const& rhs) noexcept {
			return !(lhs == rhs);
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

	// Hook tc::invoke
	namespace invoke_no_adl {
		template<typename ArgTuple, typename... Elems>
		struct expanded<ArgTuple, tc::tuple<Elems...>> final {
			using arguments = tc::type::list<tc::apply_cvref_t<Elems, ArgTuple>...>;

			template<std::size_t I>
			static constexpr decltype(auto) select(ArgTuple&& arg) noexcept {
				return tc::get<I>(std::forward<ArgTuple>(arg));
			}
		};
	}

	// Provide supporting functions that come with std::tuple.
	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<tc::decay_t<T>...> make_tuple(T&&... t) MAYTHROW {
		// std::make_tuple uses std::decay_t + std::reference_wrapper<T> to T&
		return {{ {T{std::forward<T>(t)}}... }};
	}

	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<T&&...> forward_as_tuple(T&&... t) noexcept {
		return {{ {{std::forward<T>(t)}}... }};
	}

	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<T&...> tie(T&... t) noexcept {
		return {{ {{t}}... }};
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
			[](auto&&... args) noexcept -> tc::type::apply_t<tc::tuple, tc::type::concat_t<typename tc::is_instance<tc::tuple, tc::decay_t<Tuple>>::arguments...>> {
				return {{ {{tc_move_if_owned(args)}}... }}; // MAYTHROW
			},
			std::forward<Tuple>(tuple)...
		);
	}

	template<typename Tuple, typename Fn>
	[[nodiscard]] constexpr auto tuple_transform(Tuple&& tuple, Fn&& fn) MAYTHROW {
		return tc::apply(
			[fn=std::forward<Fn>(fn)](auto&&... args) MAYTHROW {
				return tc::tuple<decltype(tc::invoke(fn, tc_move_if_owned(args)))...>{{ {{tc::invoke(fn, tc_move_if_owned(args))}}... }};
			},
			std::forward<Tuple>(tuple)
		);
	}

	namespace tuple_detail {
		template<std::size_t I, typename... Tuple>
		auto zip_get(Tuple&&... tuple) noexcept {
			return tc::forward_as_tuple(tc::get<I>(std::forward<Tuple>(tuple))...);
		}

		template<std::size_t... I, typename... Tuple>
		auto zip_impl(std::index_sequence<I...>, Tuple&&... tuple) noexcept {
			return tc::make_tuple(
				tuple_detail::zip_get<I>(std::forward<Tuple>(tuple)...)...
			);
		}
	}

	template<typename Tuple0, typename... Tuple>
	[[nodiscard]] constexpr auto tuple_zip(Tuple0&& tuple0, Tuple&&... tuple) noexcept {
		static_assert(
			((std::tuple_size<std::remove_reference_t<Tuple0>>::value == std::tuple_size<std::remove_reference_t<Tuple>>::value) && ...)
		);
		return tuple_detail::zip_impl(
			std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple0>>::value>(),
			std::forward<Tuple0>(tuple0),
			std::forward<Tuple>(tuple)...
		);
	}

	namespace tuple_detail {
		template<std::size_t n, typename T>
		T std_tuple_element_impl(tuple_adl::tuple_member<n, T> const&); // declaration only
	}
}

namespace std {
	// Structured bindings use std::tuple_size and std::tuple_element
	template<typename... T>
	struct tuple_size<tc::tuple<T...>> : INTEGRAL_CONSTANT(sizeof...(T)) {};

	template<std::size_t n, typename... T>
	struct tuple_element<n, tc::tuple<T...>> {
		using type = decltype(tc::tuple_detail::std_tuple_element_impl<n>(std::declval<tc::tuple<T...>>()));
	};
}

// Enable unqualified get<...> for tc::tuple and std::pair in C++17. The correct overload is found via ADL.
// Needed in generic context where both can appear.
template<typename...> void get(tc::define_fn_dummy_t) = delete;
