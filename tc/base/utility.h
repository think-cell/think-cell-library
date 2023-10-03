
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "return_decltype.h"
#include "tc_move.h"

#include <utility>

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

	//////////////////////////////////////////////////////////////////////////
	// is_contiguous_integer_sequence

	namespace is_contiguous_integer_sequence_impl {
		template<typename TIndex, TIndex IFirst, TIndex... Is>
		constexpr std::is_same<std::integer_sequence<TIndex, IFirst, Is...>, tc::make_integer_sequence<TIndex, IFirst, IFirst + sizeof...(Is) + 1>> is_contiguous_integer_sequence(std::integer_sequence<TIndex, IFirst, Is...>);

		template<typename TIndex>
		constexpr tc::constant<true> is_contiguous_integer_sequence(std::integer_sequence<TIndex>);

		constexpr tc::constant<false> is_contiguous_integer_sequence(...);
	}

	template<typename IntSequence>
	using is_contiguous_integer_sequence = decltype(is_contiguous_integer_sequence_impl::is_contiguous_integer_sequence(std::declval<IntSequence>()));

	//////////////////////////////////////////////////////////////////////////
	// select_nth

	template<std::size_t n, typename Arg, typename... Args>
	[[nodiscard]] constexpr decltype(auto) select_nth(Arg&& arg, Args&&... args) noexcept {
		if constexpr( 0 == n ) {
			return std::forward<Arg>(arg);
		} else {
			return tc::select_nth<n - 1>(std::forward<Args>(args)...);
		}
	}

	//////////////////////////////////////////////////////////////////////////
	// forward_like

	template<typename Cvref, typename T>
	[[nodiscard]] constexpr decltype(auto) forward_like(T& t) noexcept {
		return std::forward<tc::apply_cvref_t<T, Cvref>>(t);
	}
}

//////////////////////////////////////////////////////////////////////////
// get

namespace tc {
	namespace no_adl {
		template<typename IndexSequence, typename Tuple>
		struct tuple_types_impl;

		template<std::size_t... I, typename Tuple>
		struct tuple_types_impl<std::index_sequence<I...>, Tuple> final {
			using type = tc::type::list<std::tuple_element_t<I, Tuple>...>;
		};
	}

	template<typename Tuple>
	struct is_tuple_like /*not final*/: tc::constant<false> {};

	template<typename Tuple> requires
		tc::actual_integer<decltype(std::tuple_size<std::remove_reference_t<Tuple>>::value)> &&
		requires { typename no_adl::tuple_types_impl<std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple>>::value>, std::remove_cvref_t<Tuple>>::type; }
	struct is_tuple_like<Tuple> /*not final*/ : tc::constant<true>
	{
		using types = typename no_adl::tuple_types_impl<std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple>>::value>, std::remove_cvref_t<Tuple>>::type;
	};

	template<typename T>
	concept tuple_like = tc::is_tuple_like<T>::value;
}

namespace tc_get_impl_adl { // Outside tc namespace to avoid finding tc::get leading to infinite recursion.
#ifndef __clang__
	// Suppress unqualified lookup, because of https://developercommunity.visualstudio.com/t/vc-unqualified-lookup-finds-function-in-unrelated/1570914
	// GCC 12 also crashes here
	template<typename T> void get() = delete;
	template<std::size_t I> void get() = delete;
#endif
	template<typename T, typename Tuple, std::enable_if_t<tc::type::find_unique<typename tc::is_tuple_like<Tuple>::types, T>::found>* = nullptr>
	constexpr auto get_impl(Tuple&& tpl) return_decltype_xvalue_by_ref_noexcept(
		/*adl*/get<T>(std::forward<Tuple>(tpl))
	)

	template<std::size_t I, typename Tuple, std::enable_if_t<I < std::tuple_size<std::remove_reference_t<Tuple>>::value>* = nullptr>
	constexpr auto get_impl(Tuple&& tpl) return_decltype_xvalue_by_ref_noexcept(
		/*adl*/get<I>(std::forward<Tuple>(tpl))
	)
}

namespace tc_get_impl {
	template<typename T, typename Tuple>
	[[nodiscard]] constexpr auto get(Tuple&& tpl) return_decltype_xvalue_by_ref_noexcept(
		tc_get_impl_adl::get_impl<T>(std::forward<Tuple>(tpl))
	)

	template<std::size_t I, typename Tuple>
	[[nodiscard]] constexpr auto get(Tuple&& tpl) return_decltype_xvalue_by_ref_noexcept(
		tc_get_impl_adl::get_impl<I>(std::forward<Tuple>(tpl))
	)

	template<typename T, typename Src> requires std::same_as<T, std::remove_cvref_t<Src>>
	[[nodiscard]] constexpr decltype(auto) get(Src&& src) noexcept {
		return std::forward<Src>(src);
	}
}

namespace tc {
	// Introduces a tc::get in a way that is invisible to ADL.
	using namespace tc_get_impl;
}

namespace tc {
	namespace no_adl {
		struct deduce_tag;
	}
	using no_adl::deduce_tag;
}

//////////////////////////////////////////////////////////////////////////
// swap
//
// Must be outside the tc namespace, so that it won't call tc::swap recursively.
// tc::swap could still be called via ADL, if a type is missing an ADL barrier namespace.
namespace tc_swap_impl
{
	template<typename T> requires std::is_trivially_move_constructible<T>::value && std::is_trivially_move_assignable<T>::value && std::is_trivially_destructible<T>::value
	constexpr void swap_impl(T& a, T& b) noexcept {
		T temp(tc_move_always(a));
		a = tc_move_always(b);
		b = tc_move(temp);
	}

	using std::swap;

	template<typename T>
	constexpr void swap_impl(T&& a, T&& b) return_MAYTHROW(
		swap(std::forward<T>(a), std::forward<T>(b))
	)

	namespace named_swap
	{
		// Note: Using two template arguments here ensures that if this function and std::swap are both visible (via "using namespace") then
		// std::swap is chosen because it is more specialized.
		// T1 and T2 still have to be the same type because swap_impl takes two parameters with the same type.
		template<typename T1, typename T2>
		constexpr void swap(T1&& a, T2&& b) return_MAYTHROW(
			swap_impl(std::forward<T1>(a), std::forward<T2>(b))
		)
	}
}

namespace tc
{
	// Introduces a tc::swap name in a way that is invisible to ADL.
	using tc_swap_impl::named_swap::swap;


	// TODO C++20 : std::pair and std::tuple cannot be constexpr swapped until C++20
	template <typename First, typename Second>
	constexpr void swap(std::pair<First, Second>& lhs, std::pair<First, Second>& rhs ) noexcept {
		tc::swap(lhs.first, rhs.first);
		tc::swap(lhs.second, rhs.second);
	}

	namespace swap_tuple_impl {
		template <typename Tuple, std::size_t... I>
		constexpr void swap_tuple_impl(Tuple& lhs, Tuple& rhs, std::index_sequence<I...>) noexcept {
			(tc::swap(tc::get<I>(lhs), tc::get<I>(rhs)), ...);
		}
	}

	template <typename... T>
	constexpr void swap(std::tuple<T...>& lhs, std::tuple<T...>& rhs) noexcept {
		swap_tuple_impl::swap_tuple_impl(lhs, rhs, std::make_index_sequence<sizeof...(T)>());
	}
}

//////////////////////////////////////////////////////////////////////////
// tc_as_constexpr

namespace tc::as_constexpr_no_adl {
	namespace {
		template<typename TInit>
		struct SValueHolder final {
#ifndef __clang__
			static constexpr auto value = TInit::value();
#else
			static constexpr auto value = TInit::value()();
#endif
		};
	}
}

#if defined(__clang__) // https://bugs.llvm.org/show_bug.cgi?id=32766
#define tc_as_constexpr(...) \
	([]() constexpr noexcept -> auto const& { \
		struct SConstexprInit final { \
			static constexpr auto value() noexcept { return []() constexpr noexcept { return __VA_ARGS__; }; } \
		}; \
		return tc::as_constexpr_no_adl::SValueHolder<SConstexprInit>::value; \
	}())
#else
#define tc_as_constexpr(...) \
	([]() constexpr noexcept -> auto const& { \
		struct SConstexprInit final { \
			static constexpr auto value() noexcept { return __VA_ARGS__; } \
		}; \
		return tc::as_constexpr_no_adl::SValueHolder<SConstexprInit>::value; \
	}())
#endif

namespace tc {
	// Pass cheaply-copied types by value and others by reference to const.
	template<typename T>
	using in_arg_opt_t = std::conditional_t<
		sizeof(std::remove_cvref_t<T>) <= sizeof(void*) * 2 && std::is_trivially_constructible<std::remove_cvref_t<T>, T&&>::value,
		std::remove_cvref_t<T>,
		std::conditional_t<
			std::is_lvalue_reference<T>::value,
			std::remove_reference_t<T> const&,
			T&&
		>
	>;
}

//////////////////////////////////////////////////////////////////////////
// make_lazy/tc_lazy

namespace tc {
	namespace no_adl {
		template<typename Func>
		struct make_lazy /*not final*/ : Func {
			static_assert(tc::decayed<Func>);

			constexpr make_lazy() noexcept = default;
			constexpr explicit make_lazy(Func func) noexcept : Func(tc_move(func)) {}

			constexpr operator decltype(std::declval<Func const&>()())() const& MAYTHROW {
				return (*this)();
			}
		};
	}
	using no_adl::make_lazy;
}

// lazy rvalues are returned by value - avoid decltype on __VA_ARGS__, because expression usually contains lambdas
#define tc_lazy( ... ) tc::make_lazy([&](auto&&...) MAYTHROW -> decltype(auto) { return tc::lvalue_or_decay(__VA_ARGS__); })

//////////////////////////////////////////////////////////////////////////
// tc::unused

namespace tc {
	namespace no_adl {
		struct unused final {
			constexpr unused() noexcept = default;
			template<typename T>
			constexpr unused(T const&) noexcept {}
		};
	}
	using no_adl::unused;
}
