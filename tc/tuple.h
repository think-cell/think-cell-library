
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/invoke.h"
#include "base/move.h"
#include "base/explicit_cast_fwd.h"

namespace tc {
	template<typename Tuple>
	concept tuple_or_derived = std::same_as<Tuple, std::remove_cvref_t<Tuple>> && requires { Tuple::tc_tuple_size; };

	// std::tuple from Microsofts STL is using a recursive implementation which is very slow.
	// We use the same technique to implement tuple as libc++, but omit all constructors
	// (std::tuple has 18 constructors and 14 are conditionally explicit) and use an almost
	// trivial get implementation.
	namespace tuple_detail {
		namespace no_adl {
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

			template<typename IndexSeq, typename... T>
			struct tuple_elements;

			template<std::size_t... n, typename... T>
			struct tuple_elements<std::index_sequence<n...>, T...> : tuple_member<n, T>... {};
		}

		using no_adl::tuple_member;
		using no_adl::tuple_elements;

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
		constexpr T get(tuple_member<n, T, /*bEmpty*/true>) noexcept { return T{}; }
		template<typename T, std::size_t n>
		constexpr T get(tuple_member<n, T, /*bEmpty*/true>) noexcept { return T{}; }

		template<std::size_t n, typename T>
		T std_tuple_element_impl(tuple_member<n, T> const&); // declaration only

		template<std::size_t... n, typename... T, typename Rhs>
		constexpr void assign(tuple_elements<std::index_sequence<n...>, T...>& lhs, Rhs&& rhs) // return_decltype_MAYTHROW confuses MSVC 19.39
			noexcept((noexcept(tuple_detail::get<n>(lhs) = tc::get<n>(tc_move_if_owned(rhs))) && ...))
			requires((requires{tuple_detail::get<n>(lhs) = tc::get<n>(tc_move_if_owned(rhs));}) && ...)
		{
			(void(tuple_detail::get<n>(lhs) = tc::get<n>(tc_move_if_owned(rhs))), ...);
		}

		template<std::size_t... n, typename... T>
		constexpr auto convert(std::type_identity<tuple_elements<std::index_sequence<n...>, T...>>, auto&& tplelemsrc) return_MAYTHROW(
			tuple_elements<std::index_sequence<n...>, T...>{ {T{tuple_detail::get<n>(tc_move_if_owned(tplelemsrc))}}... }
		)

		template<std::size_t... n, typename... T, typename... U>
		constexpr bool equals(tuple_elements<std::index_sequence<n...>, T...> const& lhs, tuple_elements<std::index_sequence<n...>, U...> const& rhs) noexcept {
			return ((tuple_detail::get<n>(lhs) == tuple_detail::get<n>(rhs)) && ...);
		}

		template<std::size_t... n, typename... T>
		constexpr void swap(tuple_elements<std::index_sequence<n...>, T...>& lhs, tuple_elements<std::index_sequence<n...>, T...>& rhs) noexcept {
			(tc::swap(tuple_detail::get<n>(lhs), tuple_detail::get<n>(rhs)), ...);
		}
	}

	namespace tuple_adl {
		// Leave this as a class to have a shorter type in function signatures.
		template<typename... T>
		struct tuple {
			// To initialize every element from a single expression, use this syntax:
			//   tc::tuple<int, int> tuplenn = {1,2};
			// Otherwise, use the "double-brace" syntax:
			//   tc::tuple<X, X> tuplexx = {{ {a,b}, {c, d} }};
			[[no_unique_address]] tuple_detail::tuple_elements<std::make_index_sequence<sizeof...(T)>, T...> m_elements; // private except for get, aggregate initialization and usage as NTTP.

			// We use std::remove_reference_t<Tuple>::tc_tuple_size to restrict the argument Tuple&& to be tc::tuple in overloads found via adl.
			static constexpr std::size_t tc_tuple_size = sizeof...(T);

			template<ENABLE_SFINAE, typename Src> requires
				(std::tuple_size<std::remove_cvref_t<Src>>::value == sizeof...(T)) &&
				(!tc::derived_from<std::remove_reference_t<Src>, tuple>) // make sure default assignment operator wins in overload resolution. e.g. tc::tuple is a mutable member.
			constexpr auto/*=void, returning tuple& may turn xvalue into lvalue*/ operator=(Src&& src) /*no &*/ return_decltype_MAYTHROW(
				tuple_detail::assign(m_elements, tc_move_if_owned(src))
			)

#pragma push_macro("DEFINE_CONVERSION_OPERATOR")
#define DEFINE_CONVERSION_OPERATOR(cvref) \
			template<typename... U> requires (tc::safely_convertible_to<T cvref, U> && ...) \
			constexpr operator tuple<U...>() cvref noexcept(std::conjunction<std::is_nothrow_constructible<U, T cvref>...>::value) { \
				return {tuple_detail::convert( \
					std::type_identity<tuple_detail::tuple_elements<std::make_index_sequence<sizeof...(U)>, U...>>(), \
					static_cast<tuple cvref>(*this).m_elements \
				)}; \
			}

			DEFINE_CONVERSION_OPERATOR(&)
			DEFINE_CONVERSION_OPERATOR(const&)
			DEFINE_CONVERSION_OPERATOR(&&)
			DEFINE_CONVERSION_OPERATOR(const&&)
#pragma pop_macro("DEFINE_CONVERSION_OPERATOR")
		};

		template<typename... T, typename... TSource, std::enable_if_t<sizeof...(T)==sizeof...(TSource)>* = nullptr>
		constexpr auto explicit_convert_impl(std::type_identity<tuple<T...>>, TSource&&... src) return_decltype_MAYTHROW(
			tuple<T...>{
				tc::explicit_cast<T>(tc_move_if_owned(src))... // tc_move_if_owned doesn't compile with VS17.9
			}
		)

		template<std::size_t n, typename Tuple> requires tuple_or_derived<std::remove_cvref_t<Tuple>>
		[[nodiscard]] constexpr decltype(auto) get(Tuple&& tuple) noexcept {
			return tuple_detail::get<n>(tc_move_if_owned(tuple).m_elements);
		}

		template<typename T, typename Tuple> requires tuple_or_derived<std::remove_cvref_t<Tuple>>
		[[nodiscard]] constexpr decltype(auto) get(Tuple&& tuple) noexcept {
			return tuple_detail::get<T>(tc_move_if_owned(tuple).m_elements);
		}

		template<std::size_t... n, typename... T, typename... U>
		[[nodiscard]] constexpr bool operator==(tuple<T...> const& lhs, tuple<U...> const& rhs) noexcept {
			return tuple_detail::equals(lhs.m_elements, rhs.m_elements);
		}

		template<typename... T>
		constexpr void swap(tuple<T&...>&& lhs, tuple<T&...>&& rhs) noexcept {
			tc::swap(lhs, rhs);
		}

		template<typename... T>
		constexpr void swap(tuple<T...>& lhs, tuple<T...>& rhs) noexcept {
			static_assert((std::is_nothrow_swappable<T>::value && ...));
			return tuple_detail::swap(lhs.m_elements, rhs.m_elements);
		}

		// </<= operators defined in compare.h
		// for_each defined in for_each.h
	}
	using tuple_adl::tuple;

	// tc::safely_constructible_from<tc::tuple, ...>
	namespace no_adl {
		// Consider every construction unsafe, except construction from ... 
		template <typename... T, typename... Args>
		struct is_class_safely_constructible<tc::tuple<T...>, Args...> : tc::constant<false> {};

		//  - exact number of safely_constructible_from arguments
		template <typename... T, typename... Args> requires (sizeof...(T) == sizeof...(Args))
		struct is_class_safely_constructible<tc::tuple<T...>, Args...> : tc::constant<(tc::safely_constructible_from<T, Args> && ...)> {};

		//  - tuple-like with matching number of safely_constructible_from elements
		template <typename TupleArg, typename IndexSeq, typename... T>
		struct is_tuple_safely_constructible;

		template <typename TupleArg, std::size_t... n, typename... T>
		struct is_tuple_safely_constructible<TupleArg, std::index_sequence<n...>, T...>
			: tc::constant<(tc::safely_constructible_from<T, decltype(tc::get<n>(std::declval<TupleArg>()))> && ...)>
		{};

		template <typename... T, typename TupleArg>
			requires (sizeof...(T) == std::tuple_size<tc::remove_ref_temporary_t<TupleArg>>::value)
		struct is_class_safely_constructible<tc::tuple<T...>, TupleArg> : is_tuple_safely_constructible<TupleArg, std::index_sequence_for<T...>, T...> {};
	}

	// TODO tc::decay_t<tc::tuple>, reevaluate deep decay and adjust tc::common_type_t<tc::tuple> appropriately.

	// tc::common_type_t<tc::tuple, tc::tuple>
	namespace no_adl {
		template<typename... T, typename... U> requires requires { typename tc::tuple<tc::common_reference_t<T, U>...>; }
		struct common_type_decayed_impl<tc::tuple<T...>, tc::tuple<U...>> : std::type_identity<
			tc::tuple<tc::common_reference_t<T, U>...>
		> {};
	}

	// tc::common_reference_t<tc::tuple, tc::tuple>
	namespace tuple_detail {
		template<typename Tuple0, typename Tuple1, std::size_t... n>
		tc::tuple<
			tc::common_reference_t<
				tc::apply_cvref_t<std::tuple_element_t<n, std::remove_reference_t<Tuple0>>, Tuple0>,
				tc::apply_cvref_t<std::tuple_element_t<n, std::remove_reference_t<Tuple1>>, Tuple1>
			>...
		> common_reference_tuple_impl(std::index_sequence<n...>); // declaration only
	}

	namespace no_adl {
		template<typename Tuple0, typename Tuple1>
		requires tc::instance<std::remove_reference_t<Tuple0>, tc::tuple> && tc::instance<std::remove_reference_t<Tuple1>, tc::tuple>
			&& (std::tuple_size<std::remove_reference_t<Tuple0>>::value == std::tuple_size<std::remove_reference_t<Tuple1>>::value)
			&& (!std::same_as<std::remove_cvref_t<Tuple0>, std::remove_cvref_t<Tuple1>>)
			&& requires {
				tc::tuple_detail::common_reference_tuple_impl<Tuple0, Tuple1>(std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple0>>::value>());
			}
		struct common_reference_impl<Tuple0, Tuple1> : std::type_identity<decltype(
			tc::tuple_detail::common_reference_tuple_impl<Tuple0, Tuple1>(std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple0>>::value>())
		)> {};
	}

	template<typename T>
	using unwrap_ref_decay_t = typename std::unwrap_reference<tc::decay_t<T>>::type;

	// Provide supporting functions that come with std::tuple.
	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<tc::unwrap_ref_decay_t<T>...> make_tuple(T&&... t) MAYTHROW {
		return {{ {tc::unwrap_ref_decay_t<T>(tc_move_if_owned(t))}... }};
	}

	template<typename... T>
	[[nodiscard]] constexpr tc::tuple<T&&...> tie(T&&... t) noexcept {
		return {{ {tc_move_if_owned(t)}... }};
	}

	namespace tuple_detail::no_adl {
		template<typename Result>
		struct fn_tuple_cat final { // MSVC workaround: not a lambda for shorter symbol names
			constexpr auto operator()(auto&&... args) MAYTHROW {
				return Result{{ {tc_move_if_owned(args)}... }}; // MAYTHROW
			}
		};
	}

	template<typename... Tuple>
	[[nodiscard]] constexpr auto tuple_cat(Tuple&&... tuple) MAYTHROW {
		using tuple_type = boost::mp11::mp_apply<tc::tuple, boost::mp11::mp_append<typename tc::is_instance<tc::decay_t<Tuple>, tc::tuple>::arguments...>>;
		return tc_apply_pack(tuple_detail::no_adl::fn_tuple_cat<tuple_type>(), tc_move_if_owned(tuple)); // MAYTHROW
	}

	namespace tuple_detail::no_adl {
		template<typename Fn>
		struct fn_tuple_transform final { // MSVC workaround: not a lambda for shorter symbol names
			Fn m_fn;
			constexpr auto operator()(auto&&... args) MAYTHROW {
				return tc::tuple<decltype(tc_invoke(m_fn, tc_move_if_owned(args)))...>{{ {tc_invoke(m_fn, tc_move_if_owned(args))}... }};
			}
		};
	}

	template<typename Tuple, typename Fn>
	[[nodiscard]] constexpr auto tuple_transform(Tuple&& tuple, Fn&& fn) MAYTHROW {
		return tc_apply(tuple_detail::no_adl::fn_tuple_transform<tc::decay_t<Fn>>{tc_move_if_owned(fn)}, tc_move_if_owned(tuple));
	}
}

namespace std {
	// Structured bindings use std::tuple_size and std::tuple_element
	template<tc::tuple_or_derived Tuple>
	struct tuple_size<Tuple> : tc::constant<Tuple::tc_tuple_size> {};

	template<std::size_t n, tc::tuple_or_derived Tuple>
	struct tuple_element<n, Tuple> {
		using type = decltype(tc::tuple_detail::std_tuple_element_impl<n>(std::declval<Tuple>().m_elements));
	};
}
