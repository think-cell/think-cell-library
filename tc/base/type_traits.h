
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits_fwd.h"
#include "../container/string.h"

#include <boost/logic/tribool.hpp>

#include <optional>
#include <vector>
#ifndef __EMSCRIPTEN__
#include <atomic>
#endif

namespace tc::no_adl {
	//////////////////////////
	// decay

	// std::vector<bool>::const_reference is bool (C++ standard)
	template<bool bPreventSlicing>
	struct decay<std::vector<bool>::reference, bPreventSlicing> {
		using type = bool;
	};

#ifndef __EMSCRIPTEN__
	template<typename T, bool bPreventSlicing>
	struct decay<std::atomic<T>, bPreventSlicing> {
		using type = typename decay<T, bPreventSlicing>::type; // recursive
	};
#endif

	//////////////////////////
	// is_class_safely_constructible

	template <typename TTarget, typename... Args>
	struct is_class_safely_constructible final : tc::constant<true> {};

	// allow only copy construction from other classes, everything else we do ourselves
	template <typename T, typename A, typename Arg0, typename... Args>
	struct is_class_safely_constructible<std::vector<T,A>, Arg0, Args...> final
		: tc::constant<
			0==sizeof...(Args) &&
			std::is_class<std::remove_reference_t<Arg0>>::value
		>
	{
	};

	// allow only copy construction from other classes, everything else we do ourselves
	template <typename C, typename T, typename A, typename Arg0, typename... Args>
	struct is_class_safely_constructible<std::basic_string<C,T,A>, Arg0, Args...> final
		: tc::constant<
			0==sizeof...(Args) &&
			( std::is_class<std::remove_reference_t<Arg0>>::value || std::convertible_to<Arg0, C const*> )
		>
	{
	};

	// Restrict optional constructors

	// We consider the constructor optional<TTarget>::optional(TSource&&) dubious and
	// prefer optional<TTarget>::optional(std::in_place, TSource&&).
	// However, it is consistent with allowing optional<TTarget>::operator=(TSource&&),
	// which may be more efficient than optional::emplace and is used in constructs like
	// tc::change(otarget, source).
	template <typename TTarget, typename TSource, typename TSourceNocvref = std::remove_cvref_t<TSource>>
	struct is_optional_safely_constructible : tc::constant<
		tc::safely_constructible_from<TTarget, TSource> ||
		( // class type might have a convert operator to std::optional
			std::is_class<TSourceNocvref>::value &&
			!std::is_constructible<TTarget, TSource>::value // in this case TTarget must not be constructible from the class type to avoid ambiguity
		)
	> {};

	template <typename TTarget, typename Nullopt>
	struct is_optional_safely_constructible<TTarget, /*TSource*/Nullopt, /*TSourceNocvref*/std::nullopt_t> : tc::constant<true> {};

	template <typename TTarget, typename Optional, typename T>
	struct is_optional_safely_constructible<TTarget, /*TSource*/Optional, /*TSourceNocvref*/std::optional<T>>
		: tc::constant<tc::safely_constructible_from<TTarget, tc::same_cvref_t<T, Optional>>>
	{};

	template <typename T, typename Arg0, typename... Args>
	struct is_class_safely_constructible<std::optional<T>, Arg0, Args...> final : std::conditional_t<
		std::is_same<std::remove_cvref_t<Arg0>, std::in_place_t>::value,
#ifdef __GNUC__ // workaround gcc12 internal compiler error
		std::integral_constant<bool, tc::safely_constructible_from<T, Args...>>,	
#else
		tc::constant<tc::safely_constructible_from<T, Args...>>,
#endif
		std::conjunction<tc::constant<0 == sizeof...(Args)>, is_optional_safely_constructible<T, Arg0>>
	> {};

	template<typename TFirst, typename TSecond, typename ArgFirst, typename ArgSecond>
	struct is_class_safely_constructible<std::pair<TFirst, TSecond>, ArgFirst, ArgSecond> final: tc::constant<
		tc::safely_constructible_from<TFirst, ArgFirst> &&
		tc::safely_constructible_from<TSecond, ArgSecond>
	> {};

	template<typename TFirst, typename TSecond, typename TPair> requires tc::instance<std::remove_reference_t<TPair>, std::pair>
	struct is_class_safely_constructible<std::pair<TFirst, TSecond>, TPair> final: tc::constant<
		tc::safely_constructible_from<TFirst, decltype(std::get<0>(std::declval<TPair>()))> &&
		tc::safely_constructible_from<TSecond, decltype(std::get<1>(std::declval<TPair>()))>
	> {};


	//////////////////////////
	// common_type_decayed

	template<>
	struct common_type_decayed<bool, tc::decay_t<decltype(boost::indeterminate)>> {
		using type = boost::tribool;
	};

	template<>
	struct common_type_decayed<tc::decay_t<decltype(boost::indeterminate)>, bool> {
		using type = boost::tribool;
	};

	template<typename T>
	struct common_type_decayed<T, std::nullopt_t> : std::conditional<tc::instance<T, std::optional>, T, std::optional<T>> {};

	template<typename T>
	struct common_type_decayed<std::nullopt_t, T> : common_type_decayed<T, std::nullopt_t> {};

	template<>
	struct common_type_decayed<std::nullopt_t, std::nullopt_t> : common_type_decayed_base<std::nullopt_t, std::nullopt_t> {};
} // namespace tc::no_adl
