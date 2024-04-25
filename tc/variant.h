
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
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

 std::get_if takes a pointer to the variant and returns a pointer.
 tc::get_if takes a reference to the variant and returns tc::optional<TRef>.
*/

namespace tc {
	template<std::size_t I, typename Variant> requires (I < boost::mp11::mp_size<typename tc::is_instance_or_derived<std::remove_reference_t<Variant>, std::variant>::arguments>::value)
	[[nodiscard]] constexpr auto get_if(Variant&& v) noexcept -> tc::optional<tc::apply_cvref_t<std::variant_alternative_t<I, typename tc::is_instance_or_derived<std::remove_reference_t<Variant>, std::variant>::base_instance>, Variant&&>> {
		if(auto pval=std::get_if<I>(std::addressof(v))) {
			return tc::forward_like<Variant>(*pval);
		} else {
			return std::nullopt;
		}
	}

	template<typename T, typename Variant> requires boost::mp11::mp_set_contains<typename tc::is_instance_or_derived<std::remove_reference_t<Variant>, std::variant>::arguments, T>::value
	[[nodiscard]] constexpr auto get_if(Variant&& v) noexcept -> tc::optional<tc::apply_cvref_t<T, Variant&&>> {
		if(auto pval=std::get_if<T>(std::addressof(v))) {
			return tc::forward_like<Variant>(*pval);
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
	template<std::size_t I, typename Variant> requires (I < boost::mp11::mp_size<typename tc::is_instance_or_derived<std::remove_reference_t<Variant>, std::variant>::arguments>::value)
	[[nodiscard]] constexpr decltype(auto) get(Variant&& v) noexcept {
		return tc::forward_like<Variant>(*VERIFYNORETURN(tc::get_if<I>(v)));
	}

	template<typename T, typename Variant> requires boost::mp11::mp_set_contains<typename tc::is_instance_or_derived<std::remove_reference_t<Variant>, std::variant>::arguments, T>::value
	[[nodiscard]] constexpr decltype(auto) get(Variant&& v) noexcept {
		return tc::forward_like<Variant>(*VERIFYNORETURN(tc::get_if<T>(v)));
	}
}

namespace tc {
	namespace detail {
		namespace no_adl {
			template<typename Overload>
			struct overload_result_type final {
				template<typename... Args>
				using with_args = decltype(tc_invoke_pack(std::declval<Overload const&>(), std::declval<Args>()));

				template<typename ArgList>
				using with_arglist = boost::mp11::mp_apply<with_args, ArgList>;
			};
		}

		template<typename Overload, typename... Variants>
		using visit_result_t = boost::mp11::mp_apply<
			tc::common_reference_t,
			tc::mp_transform<
				no_adl::overload_result_type<Overload>::template with_arglist,
				boost::mp11::mp_product<
					boost::mp11::mp_list,
					tc::mp_transform<
						boost::mp11::mp_bind_back<tc::same_cvref_t, Variants>::template fn,
						typename tc::is_instance_or_derived<std::remove_reference_t<Variants>, std::variant>::arguments
					>...
				>
			>
		>;

		namespace no_adl {
			template<typename Result, typename Overload>
			struct projected_result {
				Overload const& m_overload;
				constexpr Result operator()(auto&&... args) MAYTHROW {
					return tc_invoke_pack(m_overload, tc_move_if_owned(args));
				}
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
	static_assert(std::is_same_v<decltype(a2), tc::slice_t<tc::vector<int>&>>);
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
			template<typename... Variant> requires (... && tc::instance_or_derived<std::remove_reference_t<Variant>, std::variant>)
			constexpr detail::visit_result_t<Overload, Variant...> operator()(Variant&&... v) const& MAYTHROW {
				([&]() noexcept { _ASSERTNORETURN( !v.valueless_by_exception() ); tc::discard(v); }(), ...);
				return
#ifdef TC_MAC
					std::__variant_detail::__visitation::__variant::__visit_value // does not throw std::bad_variant_access, which is not supported until macOS 10.14
#else
					std::visit
#endif
					(
						detail::no_adl::projected_result<detail::visit_result_t<Overload, Variant...>, Overload>{*this},
						tc_move_if_owned(v)...
					);
			}

			template<typename... T> requires (... && (!tc::instance_or_derived<std::remove_reference_t<T>, std::variant>))
			constexpr decltype(auto) operator()(T&&... t) const& MAYTHROW {
				return tc_invoke_pack(tc::base_cast<Overload>(*this), tc_move_if_owned(t));
			}
		};
	}
	template<typename... F>
	constexpr auto fn_visit(F&&... f) noexcept {
		return no_adl::fn_visit_impl<tc::overload<F...>>(tc_move_if_owned(f)...);
	}

	namespace no_adl {
		template<typename Var, typename T>
		struct variant_type_index;

		template<typename... Ts, typename T> requires tc::mp_find_unique<std::variant<Ts...>, T>::found
		struct variant_type_index<std::variant<Ts...>, T> final:
			tc::constant<tc::mp_find_unique<std::variant<Ts...>, T>::index>
		{};
	}
	using no_adl::variant_type_index;

	namespace explicit_convert_detail {
		template<typename VarTarget, typename VarSourceNocvref, typename VarSource>
		constexpr bool explicit_castable_between_variants_impl = false;

		template<typename... TT, typename... TS, typename VarSource> requires (... && tc::explicit_castable_from<TT, tc::apply_cvref_t<TS, VarSource>>)
		constexpr bool explicit_castable_between_variants_impl<std::variant<TT...>, std::variant<TS...>, VarSource> = true;

		template<typename VarTarget, typename VarSource>
		concept explicit_castable_between_variants=explicit_castable_between_variants_impl<VarTarget, typename tc::is_instance_or_derived<std::remove_reference_t<VarSource>, std::variant>::base_instance, VarSource>;
	}

	namespace explicit_convert_adl {
		template<typename TVariant, std::size_t I, typename... Args, tc::explicit_castable_from<Args...> Alternative = std::variant_alternative_t<I, TVariant>>
		constexpr TVariant explicit_convert_impl(adl_tag_t, std::type_identity<TVariant>, std::in_place_index_t<I> tag, Args&&... args) noexcept(
			noexcept(tc::explicit_cast<Alternative>(tc_move_if_owned(args)...))
		) {
			return tc::with_lazy_explicit_cast<Alternative>(
				[](auto&&... args) return_ctor_MAYTHROW(TVariant, (std::in_place_index<I>, tc_move_if_owned(args)...)),
				tc_move_if_owned(args)...
			);
		}

		template<typename TVariant, typename T, typename... Args>
		constexpr auto explicit_convert_impl(adl_tag_t, std::type_identity<TVariant>, std::in_place_type_t<T>, Args&&... args) return_decltype_MAYTHROW(
			tc::explicit_cast<TVariant>(std::in_place_index_t<tc::variant_type_index<TVariant, T>::value>(), tc_move_if_owned(args)...)
		)

		template<typename... TT, typename Variant> requires	tc::explicit_convert_detail::explicit_castable_between_variants<std::variant<TT...>, Variant>
		std::variant<TT...> explicit_convert_impl(adl_tag_t, std::type_identity<std::variant<TT...>>, Variant&& var) MAYTHROW {
			using TTarget = std::variant<TT...>;
			return tc::invoke_with_constant<std::make_index_sequence<sizeof...(TT)>>(
				[&](auto const nconstIndex) MAYTHROW -> TTarget {
					return TTarget(std::in_place_index_t<nconstIndex()>(), tc::lazy_explicit_cast<std::variant_alternative_t<nconstIndex(), TTarget>>(tc::get<nconstIndex()>(tc_move_if_owned(var))));
				},
				var.index()
			); // MAYTHROW
		}
	}

	template<std::size_t I, typename TVariant, typename... Args>
	std::variant_alternative_t<I, TVariant>& emplace(TVariant& var, Args&&... args) MAYTHROW {
		if constexpr (tc::safely_constructible_from<std::variant_alternative_t<I, TVariant>, Args&&...>) {
			return var.template emplace<I>(tc_move_if_owned(args)...);
		} else {
			return var.template emplace<I>(tc::lazy_explicit_cast<std::variant_alternative_t<I, TVariant>>(tc_move_if_owned(args)...));
		}
	}

	template<typename T, typename TVariant, typename... Args>
	auto emplace (TVariant& var, Args&&... args) return_decltype_MAYTHROW (
		tc::emplace<tc::variant_type_index<TVariant, T>::value>(var, tc_move_if_owned(args)...)
	)
}

namespace tc {
	namespace no_adl {
		template<typename Lhs, typename Rhs>
		struct is_equality_comparable_with final: tc::constant<false> {};

		template<typename Lhs, typename Rhs> requires
			tc::safely_convertible_to<decltype(std::declval<Lhs>()==std::declval<Rhs>()), bool> ||
			tc::safely_convertible_to<decltype(std::declval<Rhs>()==std::declval<Lhs>()), bool>
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
			(!tc::derived_from<TValue, std::variant<Ts...>>) &&
			(!tc::derived_from<TValue, std::optional<std::variant<Ts...>>>)
		struct is_variant_equality_comparable_to_value<std::variant<Ts...>, TValue> final: 
			boost::mp11::mp_any_of<boost::mp11::mp_list<Ts const&...>, boost::mp11::mp_bind_front<tc::is_equality_comparable_with, TValue const&>::template fn>
		{};
	}
	using no_adl::is_variant_equality_comparable_to_value;
}

namespace tc::variant_detail {
	template<typename... Ts, typename TRhs> requires tc::is_variant_equality_comparable_to_value<std::variant<Ts...>, TRhs>::value
	[[nodiscard]] bool equal_to_impl(std::variant<Ts...> const& lhs, TRhs const& rhs) noexcept {
	#if 0 // TODO: subrange.h cannot be included here -> move and_then into separate header
		return tc::and_then(
			tc::get_if<tc::mp_find_unique_if<boost::mp11::mp_list<Ts const&...>, boost::mp11::mp_bind_front<tc::is_equality_comparable_with, TRhs const&>::template fn>::index>(lhs),
			[&](auto const& t) noexcept { return t==rhs; }
		);
	#endif
		if (auto o = tc::get_if<tc::mp_find_unique_if<boost::mp11::mp_list<Ts const&...>, boost::mp11::mp_bind_front<tc::is_equality_comparable_with, TRhs const&>::template fn>::index>(lhs)) {
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
	return tc_conditional_prvalue_as_val(o, f(*o), TC_FWD(val)); \
}(tc::get_if<type>(var)))
