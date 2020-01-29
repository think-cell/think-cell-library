
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "type_traits.h"
#include "functors.h"
#include "casts.h"
#include <variant>

namespace tc {
	template<std::size_t I, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant* v) return_decltype_noexcept(
		std::get_if<I>(v)
	)

	template<std::size_t I, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant& v) return_decltype_noexcept(
		std::get_if<I>(std::addressof(v))
	)

	template<typename T, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant* v) return_decltype_noexcept(
		std::get_if<T>(v)
	)

	template<typename T, typename Variant>
	[[nodiscard]] constexpr auto get_if(Variant& v) return_decltype_noexcept(
		std::get_if<T>(std::addressof(v))
	)

	template<std::size_t I, typename... Types>
	[[nodiscard]] constexpr std::variant_alternative_t<I, std::variant<Types...>>& get(std::variant<Types...>& v) noexcept {
		return *VERIFY(tc::get_if<I>(v));
	}

	template<std::size_t I, typename... Types>
	[[nodiscard]] constexpr std::variant_alternative_t<I, std::variant<Types...>>&& get(std::variant<Types...>&& v) noexcept {
		return tc_move_always(*VERIFY(tc::get_if<I>(v)));
	}

	template<std::size_t I, typename... Types>
	[[nodiscard]] constexpr std::variant_alternative_t<I, std::variant<Types...>> const& get(std::variant<Types...> const& v) noexcept {
		return *VERIFY(tc::get_if<I>(v));
	}

	template<std::size_t I, typename... Types>
	[[nodiscard]] constexpr decltype(auto) get(std::variant<Types...> const&& v) noexcept {
		return static_cast<std::variant_alternative_t<I, std::variant<Types...>> const&&>(*VERIFY(tc::get_if<I>(v)));
	}

	template<typename T, typename... Types>
	[[nodiscard]] constexpr T& get(std::variant<Types...>& v) noexcept {
		return *VERIFY(tc::get_if<T>(v));
	}

	template<typename T, typename... Types>
	[[nodiscard]] constexpr T&& get(std::variant<Types...>&& v) noexcept {
		return tc_move_always(*VERIFY(tc::get_if<T>(v)));
	}

	template<typename T, typename... Types>
	[[nodiscard]] constexpr T const& get(std::variant<Types...> const& v) noexcept {
		return *VERIFY(tc::get_if<T>(v));
	}

	template<typename T, typename... Types>
	[[nodiscard]] constexpr decltype(auto) get(std::variant<Types...> const&& v) noexcept {
		return static_cast<T const&&>(*VERIFY(tc::get_if<T>(v)));
	}

	namespace detail {
		template<typename Variant, std::enable_if_t<tc::is_instance<std::variant, std::remove_reference_t<Variant>>::value>* = nullptr>
		[[nodiscard]] decltype(auto) get_variant(Variant&& var) noexcept {
			return std::forward<Variant>(var);
		}

		template<typename Variant, std::enable_if_t<tc::is_instance<std::variant, typename std::remove_reference_t<Variant>::TVariant>::value>* = nullptr>
		[[nodiscard]] decltype(auto) get_variant(Variant&& var) noexcept {
			return tc::base_cast<typename std::remove_reference_t<Variant>::TVariant>(std::forward<Variant>(var));
		}
	}

	template<typename Result=tc::use_default_result_t, typename Variant, typename... Funcs>
	constexpr decltype(auto) visit(Variant&& var, Funcs&& ... funcs) MAYTHROW {
		_ASSERTE( !var.valueless_by_exception() );
		return
#ifdef TC_MAC
			std::__variant_detail::__visitation::__variant::__visit_value // does not throw std::bad_variant_access, which is not supported until macOS 10.14
#else
			std::visit
#endif
			(
				tc::make_overload<Result>(std::forward<Funcs>(funcs)...),
				detail::get_variant(std::forward<Variant>(var))
			);
	}

	template<typename Result=tc::use_default_result_t, typename Variant0, typename Variant1, typename... Funcs>
	constexpr decltype(auto) visit2(Variant0&& var0, Variant1&& var1, Funcs&& ... funcs) MAYTHROW {
		_ASSERTE( !var0.valueless_by_exception() && !var1.valueless_by_exception() );
		return
#ifdef TC_MAC
			std::__variant_detail::__visitation::__variant::__visit_value // does not throw std::bad_variant_access, which is not supported until macOS 10.14
#else
			std::visit
#endif
			(
				tc::make_overload<Result>(std::forward<Funcs>(funcs)...),
				detail::get_variant(std::forward<Variant0>(var0)),
				detail::get_variant(std::forward<Variant1>(var1))
			);
	}

	namespace no_adl {
		template<typename Var, typename T, typename Enable=void>
		struct variant_type_index;

		template<typename... Ts, typename T>
		struct variant_type_index<std::variant<Ts...>, T, std::enable_if_t<std::disjunction<std::is_same<Ts, T>...>::value>> final:
			std::integral_constant<std::size_t, tc::type::find_unique<tc::type::list<Ts...>, T>::index>
		{};
	}
	using no_adl::variant_type_index;
}

namespace tc {
	namespace no_adl {
		template<typename Lhs, typename Rhs, typename Enable=void>
		struct is_equality_comparable final: std::false_type {};

		template<typename Lhs, typename Rhs>
		struct is_equality_comparable<Lhs, Rhs, std::enable_if_t<
			tc::is_safely_convertible<decltype(std::declval<Lhs>()==std::declval<Rhs>()), bool>::value ||
			tc::is_safely_convertible<decltype(std::declval<Rhs>()==std::declval<Lhs>()), bool>::value
		>> final: std::true_type {
			static_assert(
				std::is_same<
					decltype(std::declval<Lhs>()==std::declval<Rhs>()),
					decltype(std::declval<Rhs>()==std::declval<Lhs>())
				>::value
			);
		};
	}
	using no_adl::is_equality_comparable;

	namespace no_adl {
		template<typename TVariant, typename TValue, typename Enable=void>
		struct is_variant_equality_comparable_to_value final: std::false_type {};

		template<typename... Ts, typename TValue>
		struct is_variant_equality_comparable_to_value<std::variant<Ts...>, TValue, std::enable_if_t<!tc::is_base_of<std::variant<Ts...>, TValue>::value>> final: std::integral_constant<bool,
			tc::type::find_unique_if<tc::type::list<Ts const&...>, tc::type::curry<tc::is_equality_comparable, TValue const&>::template type>::found
		> {};
	}
	using no_adl::is_variant_equality_comparable_to_value;
}

template<typename... Ts, typename TRhs, std::enable_if_t<tc::is_variant_equality_comparable_to_value<std::variant<Ts...>, TRhs>::value>* = nullptr>
[[nodiscard]] bool operator==(std::variant<Ts...> const& lhs, TRhs const& rhs) noexcept {
	if (auto o = tc::get_if<tc::type::find_unique_if<tc::type::list<Ts const&...>, tc::type::curry<tc::is_equality_comparable, TRhs const&>::template type>::index>(lhs)) {
		return *o==rhs;
	} else {
		return false;
	}
}

template<typename... Ts, typename TRhs, std::enable_if_t<tc::is_variant_equality_comparable_to_value<std::variant<Ts...>, TRhs>::value>* = nullptr>
[[nodiscard]] bool operator!=(std::variant<Ts...> const& lhs, TRhs const& rhs) noexcept {
	return !(lhs==rhs);
}

template<typename TLhs, typename... Ts, std::enable_if_t<tc::is_variant_equality_comparable_to_value<std::variant<Ts...>, TLhs>::value>* = nullptr>
[[nodiscard]] bool operator==(TLhs const& lhs, std::variant<Ts...> const& rhs) noexcept {
	return rhs==lhs;
}

template<typename TLhs, typename... Ts, std::enable_if_t<tc::is_variant_equality_comparable_to_value<std::variant<Ts...>, TLhs>::value>* = nullptr>
[[nodiscard]] bool operator!=(TLhs const& lhs, std::variant<Ts...> const& rhs) noexcept {
	return !(lhs==rhs);
}

static_assert( std::is_move_constructible< std::variant<int, double, std::string> >::value );
static_assert( std::is_move_assignable< std::variant<int, double, std::string> >::value );

#ifdef __clang__
static_assert( std::is_nothrow_move_constructible< std::variant<int, double, std::string> >::value );
#endif

#define tc_if_holds_else_value(var, val, type, ...) ([&](auto* p) MAYTHROW -> decltype(auto) { \
	auto const f=[&](auto& _) MAYTHROW -> decltype(auto) { return (__VA_ARGS__); }; \
	static_assert( !std::is_rvalue_reference<decltype(f(*p))>::value ); \
	static_assert( !std::is_rvalue_reference<decltype((val))>::value ); \
	return CONDITIONAL_PRVALUE_AS_VAL(p, f(*p), TC_FWD(val)); \
}(tc::get_if<type>(var)))
