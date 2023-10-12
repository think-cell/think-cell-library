
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "tag_type.h"
#include "type_list.h"

// define a template function with possible customization overloads
// 1. overloads for our classes: highest priority; customization name_impl(...) is defined in ADL namespace (prefer friend function)
// 2. overloads for external types: second highest priority; customization name_impl(adl_tag_t, ...) is defined in name_adl namespace with adl_tag_t as 1st parameter
// 3. default implementation(s): lowest priority; name_impl(...) is implemented in name_default namespace before this macro
#define DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(name) \
	namespace name ## _default { \
		void name ## _impl(tc::define_fn_dummy_t) noexcept; \
	} \
	namespace name ## _adl { \
		DEFINE_ADL_TAG_TYPE(adl_tag) \
	} \
	namespace name ## _detail { \
		template<typename... Args> \
		concept has_adl_ ## name ## _impl = requires { name ## _impl(std::declval<Args>()...); }; \
		template<typename... Args> \
		concept has_adl_tag_ ## name ## _impl = requires { name ## _impl(name ## _adl::adl_tag, std::declval<Args>()...); }; \
		template<typename... Args> \
		concept has_default_ ## name ## _impl = requires { name ## _default:: name ## _impl(std::declval<Args>()...); }; \
	} \
	template<typename... Args> requires \
		name ## _detail::has_adl_ ## name ## _impl<Args...> \
	constexpr decltype(auto) name(Args&& ... args) noexcept(noexcept(name ## _impl(tc_move_if_owned(args)...))) { \
		return name ## _impl(tc_move_if_owned(args)...); \
	} \
	template<typename... Args> requires \
		(!name ## _detail::has_adl_ ## name ## _impl<Args...>) && \
		name ## _detail::has_adl_tag_ ## name ## _impl<Args...> \
	constexpr decltype(auto) name(Args&& ... args) noexcept(noexcept(name ## _impl(name ## _adl::adl_tag, tc_move_if_owned(args)...))) { \
		return name ## _impl(name ## _adl::adl_tag, tc_move_if_owned(args)...); \
	} \
	template<typename... Args> requires \
		(!name ## _detail::has_adl_ ## name ## _impl<Args...>) && \
		(!name ## _detail::has_adl_tag_ ## name ## _impl<Args...>) && \
		name ## _detail::has_default_ ## name ## _impl<Args...> \
	constexpr decltype(auto) name(Args&& ... args) noexcept(noexcept(name ## _default:: name ## _impl(tc_move_if_owned(args)...))) { \
		return name ## _default:: name ## _impl(tc_move_if_owned(args)...); \
	}
