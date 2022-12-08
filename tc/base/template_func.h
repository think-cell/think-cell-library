
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
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
		namespace no_adl { \
			template<typename... Args> \
			struct has_adl_ ## name ## _impl final { \
			private: \
				template<typename...> \
				static tc::constant<false> check(...); /*unevaluated-only*/ \
				template<typename... T> \
				static tc::constant<true> check(tc::void_t<decltype(name ## _impl(std::declval<T>()...))>*); /*unevaluated-only*/ \
			public: \
				static constexpr bool value = decltype(check<Args...>(nullptr))::value; \
			}; \
			template<typename... Args> \
			struct has_adl_tag_ ## name ## _impl final { \
			private: \
				template<typename...> \
				static tc::constant<false> check(...); /*unevaluated-only*/ \
				template<typename... T> \
				static tc::constant<true> check(tc::void_t<decltype(name ## _impl(name ## _adl::adl_tag, std::declval<T>()...))>*); /*unevaluated-only*/ \
			public: \
				static constexpr bool value = decltype(check<Args...>(nullptr))::value; \
			}; \
			template<typename... Args> \
			struct has_default_ ## name ## _impl final { \
			private: \
				template<typename...> \
				static tc::constant<false> check(...); /*unevaluated-only*/ \
				template<typename... T> \
				static tc::constant<true> check(tc::void_t<decltype(name ## _default:: name ## _impl(std::declval<T>()...))>*); /*unevaluated-only*/ \
			public: \
				static constexpr bool value = decltype(check<Args...>(nullptr))::value; \
			}; \
		} \
		using no_adl::has_adl_ ## name ## _impl; \
		using no_adl::has_adl_tag_ ## name ## _impl; \
		using no_adl::has_default_ ## name ## _impl; \
	} \
	template<typename... Args> requires \
		name ## _detail::has_adl_ ## name ## _impl<Args...>::value \
	constexpr decltype(auto) name(Args&& ... args) noexcept(noexcept(name ## _impl(std::forward<Args>(args)...))) { \
		return name ## _impl(std::forward<Args>(args)...); \
	} \
	template<typename... Args> requires \
		(!name ## _detail::has_adl_ ## name ## _impl<Args...>::value) && \
		name ## _detail::has_adl_tag_ ## name ## _impl<Args...>::value \
	constexpr decltype(auto) name(Args&& ... args) noexcept(noexcept(name ## _impl(name ## _adl::adl_tag, std::forward<Args>(args)...))) { \
		return name ## _impl(name ## _adl::adl_tag, std::forward<Args>(args)...); \
	} \
	template<typename... Args> requires \
		(!name ## _detail::has_adl_ ## name ## _impl<Args...>::value) && \
		(!name ## _detail::has_adl_tag_ ## name ## _impl<Args...>::value) && \
		name ## _detail::has_default_ ## name ## _impl<Args...>::value \
	constexpr decltype(auto) name(Args&& ... args) noexcept(noexcept(name ## _default:: name ## _impl(std::forward<Args>(args)...))) { \
		return name ## _default:: name ## _impl(std::forward<Args>(args)...); \
	}
