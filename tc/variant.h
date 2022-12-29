
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "base/assert_defs.h"
#include "base/type_traits.h"
#include "base/functors.h"
#include "base/casts.h"
#include "base/explicit_cast.h"
#include "base/invoke_with_constant.h"
#include "optional.h"
#include <variant>

/*
 Extensions to make std::variant easier to use


 std::get_if always takes a pointer to the variant.
 tc::get_if allows passing a reference or a pointer.
```
	std::variant<int, std::string> var = "Hello World";
	_ASSERT(tc::get_if<0>(var)==nullptr);
	_ASSERT(tc::get_if<0>(std::addressof(var))==nullptr);
	_ASSERT(tc::equal(tc::get_if<1>(var), "Hello World"))
	_ASSERT(tc::equal(tc::get_if<1>(std::addressof(var)), "Hello World"));
```
*/

namespace tc {
	template<std::size_t I, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant& v) noexcept -> tc::optional<std::variant_alternative_t<I, Variant>&> {
		if(auto pval=std::get_if<I>(std::addressof(v))) {
			return *pval;
		} else {
			return std::nullopt;
		}
	}

	template<std::size_t I, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant&& v) noexcept -> tc::optional<std::variant_alternative_t<I, Variant>&&> {
		if(auto pval=std::get_if<I>(std::addressof(v))) {
			return std::move(*pval);
		} else {
			return std::nullopt;
		}
	}

	template<typename T, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant& v) noexcept -> tc::optional<std::conditional_t<std::is_const<Variant>::value, T const, T>&> {
		if(auto pval=std::get_if<T>(std::addressof(v))) {
			return *pval;
		} else {
			return std::nullopt;
		}
	}

	template<typename T, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant&& v) noexcept -> tc::optional<std::conditional_t<std::is_const<Variant>::value, T const, T>&&> {
		if(auto pval=std::get_if<T>(std::addressof(v))) {
			return std::move(*pval);
		} else {
			return std::nullopt;
		}
	}
}

/*
 tc::get overloads for std::variant so we can use tc::get<I> with tuples and variants.
 
```
	std::variant<int, std::string> var = "Hello World";
	_ASSERT(tc::equal(tc::get<1>(var), "Hello World"));

	static_assert(std::is_same_v<decltype(tc::get<1>(tc_move(var))), std::string&&>);
	auto str(tc::get<1>(tc_move(var))); // will call move constructor

```
*/
namespace tc_get_impl {
	template<std::size_t I, typename Variant>
	[[nodiscard]] constexpr decltype(auto) get(Variant&& v) noexcept requires(tc::is_instance_or_derived<std::variant, Variant>::value) {
		return tc::forward_like<Variant>(*VERIFYNORETURN(tc::get_if<I>(v)));
	}

	template<typename T, typename Variant>
	[[nodiscard]] constexpr decltype(auto) get(Variant&& v) noexcept requires(tc::is_instance_or_derived<std::variant, Variant>::value) {
		return tc::forward_like<Variant>(*VERIFYNORETURN(tc::get_if<T>(v)));
	}
}

namespace tc {
	namespace detail {
		namespace no_adl {
			template<typename Overload>
			struct overload_result_type final {
				template<typename... Args>
				using with_args = decltype(tc::invoke(std::declval<Overload const&>(), std::declval<Args>()...));

				template<typename ArgList>
				using with_arglist = tc::type::apply_t<with_args, ArgList>;
			};
		}

		template<typename Overload, typename... Variants>
		using visit_result_t = tc::type::apply_t<
			tc::common_reference_prvalue_as_val_t,
			tc::type::transform_t<
				tc::type::cartesian_product_t<
					tc::type::transform_t<
						typename tc::is_instance_or_derived<std::variant, Variants>::arguments,
						tc::type::rcurry<tc::same_cvref_t, Variants>::template type
					>...
				>,
				no_adl::overload_result_type<Overload>::template with_arglist
			>
		>;


		template<typename Result, typename Overload>
		decltype(auto) projected_result(Overload&& overload) noexcept {
			return [overload=std::forward<Overload>(overload)](auto&&... args) MAYTHROW -> Result {
				return tc::invoke(overload, tc_move_if_owned(args)...);
			};
		}
	}

/*
	tc::fn_visit is a more flexible alternative to std::visit.
	
	tc::fn_visit(f...) returns a function that unwraps variants and passes the active alternative to the best match in the overload set f... using normal C++ rules.
	The result of fn_visit(f...)(v...) is the result of the invocation g(a...), where a... are the active alternatives of v... and g the selected overload from the set f...,
	converted to the common reference of decltype(f(a...))... for all combinations of alternatives in v...
	(i.e. tc::common_reference_prvalue_as_val, see base/type_traits_fwd.h and type_traits.t.cpp)
```
	struct A {} a;
	struct B : A{} b;
	
	std::variant<int, std::string> var = "Hello World";
	decltype(auto) a2 = tc::fn_visit(
		[&](int) -> A const& { return a; },
		[&](const std::string& arg) -> B& { return b; }
	)(var);
	static_assert(std::is_same_v<decltype(a2), A const&>);
```

	The common type is specialized for our range library. The common type of a vector<T>& and
	a subrange<vector<T>&> is again a subrange<vector<T>&>:

```
	tc::vector<int> vecn;
	auto rngn = tc::take(vecn, tc::end(vecn));
	decltype(auto) a2 = tc::fn_visit(
		[&](int) -> tc::vector<int>& { return vecn; },
		[&](const std::string&) { return rngn; }
	)(var);
	static_assert(std::is_same_v<decltype(a2), tc::subrange<tc::vector<int>&>>);
```
	
	Currying allows concise use with our range library avoiding additional indirections when they are not needed.
```
	tc::vector<std::variant<int, double>> vecvarnf;
	tc::for_each(vecvarnf, tc::fn_visit(
		[](int& n) noexcept {
			// handle int alternative
		},
		[](double& f) noexcept {
			// handle double alternative
		}
	));
```
*/
	namespace no_adl {
		template<typename Overload>
		struct [[nodiscard]] fn_visit_impl : private Overload {
			using Overload::Overload;
			template<typename... Variant>
			detail::visit_result_t<Overload, Variant...> operator()(Variant&&... v) const& MAYTHROW {
				([&]() noexcept { _ASSERTNORETURN( !v.valueless_by_exception() ); }(), ...);
				return
#ifdef TC_MAC
					std::__variant_detail::__visitation::__variant::__visit_value // does not throw std::bad_variant_access, which is not supported until macOS 10.14
#else
					std::visit
#endif
					(
						detail::projected_result<detail::visit_result_t<Overload, Variant...>>(tc::base_cast<Overload>(*this)),
						tc::base_cast<typename tc::is_instance_or_derived<std::variant, Variant>::base_instance>(std::forward<Variant>(v))...
					);
			}
		};
	}
	template<typename... F>
	constexpr auto fn_visit(F&&... f) noexcept {
		return no_adl::fn_visit_impl<tc::overload<F...>>(std::forward<F>(f)...);
	}

	namespace no_adl {
		template<typename Var, typename T>
		struct variant_type_index;

		template<typename... Ts, typename T> requires (std::is_same<Ts, T>::value || ...)
		struct variant_type_index<std::variant<Ts...>, T> final:
			tc::constant<tc::type::find_unique<tc::type::list<Ts...>, T>::index>
		{};
	}
	using no_adl::variant_type_index;

	namespace explicit_convert_adl {
		template<typename TVariant, std::size_t I, typename... Args, typename Alternative = std::variant_alternative_t<I, TVariant>>
			requires tc::is_explicit_castable<Alternative, Args...>::value
		constexpr TVariant explicit_convert_impl(adl_tag_t, tc::type::identity<TVariant>, std::in_place_index_t<I> tag, Args&&... args) noexcept(
			noexcept(tc::explicit_cast<Alternative>(std::forward<Args>(args)...))
		) {
			return tc::with_lazy_explicit_cast<Alternative>(
				[](auto&&... args) return_ctor_MAYTHROW(TVariant, (std::in_place_index<I>, tc_move_if_owned(args)...)),
				std::forward<Args>(args)...
			);
		}

		template<typename TVariant, typename T, typename... Args>
		constexpr auto explicit_convert_impl(adl_tag_t, tc::type::identity<TVariant>, std::in_place_type_t<T>, Args&&... args) return_decltype_MAYTHROW(
			tc::explicit_cast<TVariant>(std::in_place_index_t<tc::variant_type_index<TVariant, T>::value>(), std::forward<Args>(args)...)
		)

		template<typename... TT, typename... TS> requires (sizeof...(TT) == sizeof...(TS))
		std::variant<TT...> explicit_convert_impl(adl_tag_t, tc::type::identity<std::variant<TT...>>, std::variant<TS...> const& src) MAYTHROW {
			using TTarget = std::variant<TT...>;
			return tc::invoke_with_constant<std::make_index_sequence<sizeof...(TT)>>(
				[&](auto nconstIndex) MAYTHROW -> TTarget {
					RETURN_CAST(std::in_place_index_t<nconstIndex()>(), tc::lazy_explicit_cast<std::variant_alternative_t<nconstIndex(), TTarget>>(tc::get<nconstIndex()>(src)));
				},
				src.index()
			); // MAYTHROW
		}

		template<typename... TT, typename... TS> requires (sizeof...(TT) == sizeof...(TS))
		std::variant<TT...> explicit_convert_impl(adl_tag_t, tc::type::identity<std::variant<TT...>>, std::variant<TS...>&& src) MAYTHROW {
			using TTarget = std::variant<TT...>;
			return tc::invoke_with_constant<std::make_index_sequence<sizeof...(TT)>>(
				[&](auto nconstIndex) MAYTHROW -> TTarget {
					RETURN_CAST(std::in_place_index_t<nconstIndex()>(), tc::lazy_explicit_cast<std::variant_alternative_t<nconstIndex(), TTarget>>(tc::get<nconstIndex()>(tc_move(src))));
				},
				src.index()
			); // MAYTHROW
		}
	}

	template<std::size_t I, typename TVariant, typename... Args>
	std::variant_alternative_t<I, TVariant>& emplace(TVariant& var, Args&&... args) MAYTHROW {
		if constexpr (tc::is_safely_constructible<TVariant, Args&&...>::value) {
			return var.template emplace<I>(std::forward<Args>(args)...);
		} else {
			return var.template emplace<I>(tc::lazy_explicit_cast<std::variant_alternative_t<I, TVariant>>(std::forward<Args>(args)...));
		}
	}

	template<typename T, typename TVariant, typename... Args>
	auto emplace (TVariant& var, Args&&... args) return_decltype_MAYTHROW (
		tc::emplace<tc::variant_type_index<TVariant, T>::value>(var, std::forward<Args>(args)...)
	)
}

namespace tc {
	namespace no_adl {
		template<typename Lhs, typename Rhs>
		struct is_equality_comparable_with final: tc::constant<false> {};

		template<typename Lhs, typename Rhs> requires
			tc::is_safely_convertible<decltype(std::declval<Lhs>()==std::declval<Rhs>()), bool>::value ||
			tc::is_safely_convertible<decltype(std::declval<Rhs>()==std::declval<Lhs>()), bool>::value
		struct is_equality_comparable_with<Lhs, Rhs> final: tc::constant<true> {
			STATICASSERTSAME(
				decltype(std::declval<Lhs>()==std::declval<Rhs>()),
				decltype(std::declval<Rhs>()==std::declval<Lhs>())				
			);
		};
	}
	using no_adl::is_equality_comparable_with;

	namespace no_adl {
		template<typename TVariant, typename TValue>
		struct is_variant_equality_comparable_to_value final: tc::constant<false> {};

		template<typename... Ts, typename TValue> requires
			(!tc::is_base_of<std::variant<Ts...>, TValue>::value) &&
			(!tc::is_base_of<std::optional<std::variant<Ts...>>, TValue>::value)
		struct is_variant_equality_comparable_to_value<std::variant<Ts...>, TValue> final: tc::constant<
			tc::type::find_unique_if<tc::type::list<Ts const&...>, tc::type::curry<tc::is_equality_comparable_with, TValue const&>::template type>::found
		> {};
	}
	using no_adl::is_variant_equality_comparable_to_value;
}

namespace tc::variant_detail {
	template<typename... Ts, typename TRhs> requires tc::is_variant_equality_comparable_to_value<std::variant<Ts...>, TRhs>::value
	[[nodiscard]] bool equal_to_impl(std::variant<Ts...> const& lhs, TRhs const& rhs) noexcept {
	#if 0 // TODO: subrange.h cannot be included here -> move and_then into separate header
		return tc::and_then(
			tc::get_if<tc::type::find_unique_if<tc::type::list<Ts const&...>, tc::type::curry<tc::is_equality_comparable_with, TRhs const&>::template type>::index>(lhs),
			[&](auto const& t) noexcept { return t==rhs; }
		);
	#endif
		if (auto o = tc::get_if<tc::type::find_unique_if<tc::type::list<Ts const&...>, tc::type::curry<tc::is_equality_comparable_with, TRhs const&>::template type>::index>(lhs)) {
			return *o==rhs;
		} else {
			return false;
		}
	}
}

namespace tc::equal_to_adl {
	template<typename... Ts, typename TRhs>
	auto equal_to_impl(adl_tag_t, std::variant<Ts...> const& lhs, TRhs const& rhs) return_decltype_noexcept(
		tc::variant_detail::equal_to_impl(lhs, rhs)
	)

	template<typename TLhs, typename... Ts>
	auto equal_to_impl(adl_tag_t, TLhs const& lhs, std::variant<Ts...> const& rhs) return_decltype_noexcept(
		tc::variant_detail::equal_to_impl(rhs, lhs)
	)
}

static_assert( std::is_move_constructible< std::variant<int, double, tc::string<char>> >::value );
static_assert( std::is_move_assignable< std::variant<int, double, tc::string<char>> >::value );
static_assert( std::is_nothrow_move_constructible< std::variant<int, double, tc::string<char>> >::value );

#define tc_if_holds_else_value(var, val, type, ...) ([&](auto o) MAYTHROW -> decltype(auto) { \
	auto const f=[&](auto& _) MAYTHROW -> decltype(auto) { return (__VA_ARGS__); }; \
	static_assert( !std::is_rvalue_reference<decltype(f(*o))>::value ); \
	static_assert( !std::is_rvalue_reference<decltype((val))>::value ); \
	return CONDITIONAL_PRVALUE_AS_VAL(o, f(*o), TC_FWD(val)); \
}(tc::get_if<type>(var)))
