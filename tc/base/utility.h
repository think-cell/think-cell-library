
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "return_decltype.h"

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

	namespace concat_integer_sequence_impl {
		template <typename T, T ... Lhs, typename U, U ... Rhs>
		std::integer_sequence<tc::common_type_t<T, U>, Lhs..., Rhs...> operator+(std::integer_sequence<T, Lhs...>, std::integer_sequence<U, Rhs...>);
		template <typename T, T ... Lhs>
		std::integer_sequence<T, Lhs...> operator+(std::integer_sequence<T, Lhs...>, std::integer_sequence<bool>); // std::integer_sequence<bool> is the identity element

		template <typename ... Sequence>
		consteval auto concat_integer_sequence(Sequence... seqs) noexcept {
			// We initialize with std::integer_sequence<bool> as the default type for empty sequences.
			return (seqs + ... + std::integer_sequence<bool>{});
		}
	}
	template <typename ... Sequence>
	using concat_integer_sequence = decltype(concat_integer_sequence_impl::concat_integer_sequence(Sequence{}...));
	template <typename ... Sequence>
	using concat_index_sequence = concat_integer_sequence<std::index_sequence<>, Sequence...>;

	namespace repeat_integer_sequence_impl {
		template <std::size_t Count, auto Value>
		struct repeat_one {
			template <std::size_t ... Idx>
			static auto get(std::index_sequence<Idx...>) {
				return std::integer_sequence<std::remove_const_t<decltype(Value)>, ((void)Idx, Value)...>{};
			}
			using type = decltype(get(std::make_index_sequence<Count>{}));
		};

		template <typename Counts, typename Sequence>
		struct repeat_integer_sequence;
		template <std::size_t ... Counts, typename T, T ... Values>
		struct repeat_integer_sequence<std::index_sequence<Counts...>, std::integer_sequence<T, Values...>> {
			using type = concat_integer_sequence<std::integer_sequence<T>, typename repeat_one<Counts, Values>::type...>;
		};
	}
	// repeat_integer_sequence<index_sequence<2, 1, 2>, index_sequence<1, 2, 3>> -> index_sequence<1, 1, 2, 3, 3>
	template <typename Counts, typename Sequence>
	using repeat_integer_sequence = typename repeat_integer_sequence_impl::repeat_integer_sequence<Counts, Sequence>::type;

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

	template <std::size_t N>
	constexpr void select_nth() noexcept {
		static_assert(tc::dependent_false<tc::constant<N>>::value, "index out of range");
	}
	template <std::size_t N>
	[[nodiscard]] constexpr decltype(auto) select_nth(auto&& a0, auto&&... args) noexcept {
		if constexpr (0 == N) {
			return tc_move_if_owned(a0);
		} else {
			return tc::select_nth<N - 1>(tc_move_if_owned(args)...);
		}
	}
	template <std::size_t N>
	[[nodiscard]] constexpr decltype(auto) select_nth(auto&& a0, auto&& a1, auto&&... args) noexcept {
		if constexpr (0 == N) {
			return tc_move_if_owned(a0);
		} else if constexpr (1 == N) {
			return tc_move_if_owned(a1);
		} else {
			return tc::select_nth<N - 2>(tc_move_if_owned(args)...);
		}
	}
	template <std::size_t N>
	[[nodiscard]] constexpr decltype(auto) select_nth(auto&& a0, auto&& a1, auto&& a2, auto&&... args) noexcept {
		if constexpr (0 == N) {
			return tc_move_if_owned(a0);
		} else if constexpr (1 == N) {
			return tc_move_if_owned(a1);
		} else if constexpr (2 == N) {
			return tc_move_if_owned(a2);
		} else {
			return tc::select_nth<N - 3>(tc_move_if_owned(args)...);
		}
	}
	template <std::size_t N>
	[[nodiscard]] constexpr decltype(auto) select_nth(auto&& a0, auto&& a1, auto&& a2, auto&& a3, auto&&... args) noexcept {
		if constexpr (0 == N) {
			return tc_move_if_owned(a0);
		} else if constexpr (1 == N) {
			return tc_move_if_owned(a1);
		} else if constexpr (2 == N) {
			return tc_move_if_owned(a2);
		} else if constexpr (3 == N) {
			return tc_move_if_owned(a3);
		} else {
			return tc::select_nth<N - 4>(tc_move_if_owned(args)...);
		}
	}

	//////////////////////////////////////////////////////////////////////////
	// forward_like

	template<typename Cvref, typename T>
	[[nodiscard]] constexpr decltype(auto) forward_like(T& t) noexcept {
		return static_cast<tc::apply_cvref_t<T, Cvref>&&>(t);
	}

	//////////////////////////////////////////////////////////////////////////
	// tuple_like

	template<typename Tuple>
	concept tuple_like = tc::actual_integer<decltype(std::tuple_size<tc::remove_ref_temporary_t<Tuple>>::value)>;
}

//////////////////////////////////////////////////////////////////////////
// get

namespace tc_get_impl_adl { // Outside tc namespace to avoid finding tc::get leading to infinite recursion.
#ifdef __GNUC__
	// TODO: GCC 13.1 still crashes without the workaround, 13.2 compiles
	template<typename T> void get() = delete;
	template<std::size_t I> void get() = delete;
#endif
	template<typename T>
	constexpr decltype(auto) get_impl(auto&& tpl) noexcept {
		return /*adl*/get<T>(tc_move_if_owned(tpl));
	}

	template<std::size_t I>
	constexpr decltype(auto) get_impl(auto&& tpl) noexcept {
		return /*adl*/get<I>(tc_move_if_owned(tpl));
	}
}

namespace tc_get_impl {
	template <typename T>
	[[nodiscard]] constexpr decltype(auto) get(auto&& tpl) noexcept {
		return tc_get_impl_adl::get_impl<T>(tc_move_if_owned(tpl));
	}

	template <std::size_t I>
	[[nodiscard]] constexpr decltype(auto) get(auto&& tpl) noexcept {
		return tc_get_impl_adl::get_impl<I>(tc_move_if_owned(tpl));
	}

	template<typename T, typename Src> requires std::same_as<T, std::remove_cvref_t<Src>>
	[[nodiscard]] constexpr decltype(auto) get(Src&& src) noexcept {
		return tc_move_if_owned(src);
	}
}

namespace tc {
	// Introduces a tc::get in a way that is invisible to ADL.
	using namespace tc_get_impl;

	namespace temporary_adl {
		template <typename T, typename Tuple, unsigned Lifetime>
		constexpr decltype(auto) get(temporary<Tuple, Lifetime> tpl) noexcept {
			if constexpr (std::is_reference<T>::value) {
				return tc::get<T>(tc_unwrap_temporary(tc_move(tpl)));
			} else {
				return tc_rewrap_temporary(TC_FWD(tc::temporary<Tuple, Lifetime>), tc::get<T>(tc_unwrap_temporary(tc_move(tpl))));
			}
		}

		template <std::size_t I, typename Tuple, unsigned Lifetime>
		constexpr decltype(auto) get(temporary<Tuple, Lifetime> tpl) noexcept {
			if constexpr (std::is_reference<std::tuple_element_t<I, Tuple>>::value) {
				return tc::get<I>(tc_unwrap_temporary(tc_move(tpl)));
			} else {
				return tc_rewrap_temporary(TC_FWD(tc::temporary<Tuple&&, Lifetime>), tc::get<I>(tc_unwrap_temporary(tc_move(tpl))));
			}
		}
	}
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
	using std::swap;

	template<typename T>
	constexpr void swap_impl(T&& a, T&& b) return_MAYTHROW(
		swap(tc_move_if_owned(a), tc_move_if_owned(b))
	)

	namespace named_swap
	{
		// Note: Using two template arguments here ensures that if this function and std::swap are both visible (via "using namespace") then
		// std::swap is chosen because it is more specialized.
		// T1 and T2 still have to be the same type because swap_impl takes two parameters with the same type.
		template<typename T1, typename T2>
		constexpr void swap(T1&& a, T2&& b) return_MAYTHROW(
			swap_impl(tc_move_if_owned(a), tc_move_if_owned(b))
		)
	}
}

namespace tc
{
	// Introduces a tc::swap name in a way that is invisible to ADL.
	using tc_swap_impl::named_swap::swap;
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

#ifdef MSVC_WORKAROUND
namespace tc::static_auto_constexpr_no_adl {
	template<typename Func>
	struct SValueHolder final {
		static constexpr auto value=Func{};
	};

	struct assigner_t final: tc::nonmovable {
		template<typename Func>
		constexpr auto const& operator=(Func) && noexcept {
			return SValueHolder<Func>::value;
		}
	};
}

// Workaround MSVC code generation bug since 19.34: https://developercommunity.visualstudio.com/t/code-generation-bug-on-static-variable-i/10541326
// We use static constexpr local variables for optimization purpose. They are initialized at compile time and will not be copied onto stack during runtime.
// But they may or may not have multiple instantiations.
#define tc_static_auto_constexpr(var, ...) constexpr auto const& var = tc_as_constexpr(__VA_ARGS__)
#define tc_static_auto_constexpr_lambda(f) constexpr auto const& f = tc::static_auto_constexpr_no_adl::assigner_t{} // We can step into lambdas during debugging
// A lambda expression can use a variable without capturing it if the variable is a reference that has been initialized with a constant expression.
// MSVC sometimes cannot use constexpr variable/reference in lambdas at all even with explicit capture.
// In such cases we have to use const reference and capture (which is not needed by standard).
#define tc_static_auto_constexpr_capture(var, ...) auto const& var = tc_as_constexpr(__VA_ARGS__)
#define tc_static_auto_constexpr_lambda_capture(f) auto const& f = tc::static_auto_constexpr_no_adl::assigner_t{} // We can step into lambdas during debugging
#define IF_MSVC_STATIC_WORKAROUND(...) __VA_ARGS__ // unnecessary capture of const reference of static constexpr values needed by MSVC
#define tc_static_auto_constexpr_litstr(var, ...) constexpr decltype(auto) var = __VA_ARGS__;
#define tc_static_auto_constexpr_litstr_capture(var, ...) decltype(auto) var = __VA_ARGS__;
#else
#define tc_static_auto_constexpr(var, ...) static auto constexpr var = __VA_ARGS__
#define tc_static_auto_constexpr_lambda(f) static auto constexpr f
#define tc_static_auto_constexpr_capture(var, ...) tc_static_auto_constexpr(TC_FWD(var), TC_FWD(__VA_ARGS__))
#define tc_static_auto_constexpr_lambda_capture(f) tc_static_auto_constexpr_lambda(TC_FWD(f))
#define IF_MSVC_STATIC_WORKAROUND(...)
#define tc_static_auto_constexpr_litstr(var, ...) static constexpr decltype(auto) var = __VA_ARGS__;
#define tc_static_auto_constexpr_litstr_capture(var, ...) tc_static_auto_constexpr_litstr(TC_FWD(var), TC_FWD(__VA_ARGS__))
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
	namespace make_lazy_adl {
		template<typename Func>
		struct make_lazy /*not final*/ : Func {
			static_assert(tc::decayed<Func>);

			constexpr make_lazy() noexcept = default;
			constexpr explicit make_lazy(Func func) noexcept : Func(tc_move(func)) {}

			constexpr operator decltype(std::declval<Func const&>()())() const&& MAYTHROW {
				return (*this)();
			}
		};
	}
	using make_lazy_adl::make_lazy;
}

#ifndef __clang__ // TODO: try MSVC after 19.31
	// lazy rvalues are returned by value - avoid decltype on __VA_ARGS__, because expression usually contains lambdas
	#define tc_lazy( ... ) tc::make_lazy([&](auto&&...) MAYTHROW -> decltype(auto) { return tc::lvalue_or_decay(__VA_ARGS__); })
	// tc_lazy_prvalue allows copy elision
	#define tc_lazy_prvalue( ... ) tc::make_lazy([&](auto&&...) MAYTHROW { return __VA_ARGS__; })
#else
	#define tc_lazy( ... ) tc::make_lazy([&](auto&&...) MAYTHROW -> decltype(auto) { \
		if constexpr (std::is_reference<decltype((__VA_ARGS__))>::value) { \
			return tc::lvalue_or_decay(__VA_ARGS__); \
		} else { \
			return __VA_ARGS__; \
		} \
	})
	#define tc_lazy_prvalue(...) tc_lazy(__VA_ARGS__)
#endif

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
