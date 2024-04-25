
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "tag_type.h"
#include "type_list.h"

#define DECLARE_TMPL_FUNC_WITH_CUSTOMIZATIONS(name) \
	namespace name ## _adl { \
		struct adl_tag_t; \
	} \
	namespace no_adl { \
		struct fn_ ## name; \
	} \
	extern inline no_adl::fn_ ## name const name;

// define a template function with possible customization overloads
// 1. overloads for external types: highest priority; customization name_impl(adl_tag_prio_t, ...) is defined in name_adl namespace with adl_tag_prio_t as 1st parameter
// 2. overloads for our classes: second highest priority; customization name_impl(...) is defined in ADL namespace (prefer friend function)
// 3. overloads for external types: third highest priority; customization name_impl(adl_tag_t, ...) is defined in name_adl namespace with adl_tag_t as 1st parameter
// 4. default implementation(s): lowest priority; name_impl(...) is implemented in name_default namespace before this macro
#define DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(name) \
	namespace name ## _default { \
		void name ## _impl(tc::define_fn_dummy_t) noexcept; \
	} \
	namespace name ## _adl { \
		DEFINE_ADL_TAG_TYPE(adl_tag) \
		DEFINE_ADL_TAG_TYPE(adl_tag_prio) \
	} \
	namespace name ## _detail { \
		template<typename... Args> \
		concept has_adl_tag_prio_ ## name ## _impl = requires { name ## _impl(name ## _adl::adl_tag_prio, std::declval<Args>()...); }; \
		template<typename... Args> \
		concept has_adl_ ## name ## _impl = requires { name ## _impl(std::declval<Args>()...); }; \
		template<typename... Args> \
		concept has_adl_tag_ ## name ## _impl = requires { name ## _impl(name ## _adl::adl_tag, std::declval<Args>()...); }; \
		template<typename... Args> \
		concept has_default_ ## name ## _impl = requires { name ## _default:: name ## _impl(std::declval<Args>()...); }; \
	} \
	\
	namespace no_adl { \
		struct [[nodiscard]] TC_EMPTY_BASES fn_ ## name { \
		private: \
			template<typename... Args> \
			static constexpr bool NoExcept() noexcept { \
				if constexpr(name ## _detail::has_adl_tag_prio_ ## name ## _impl<Args...>) { \
					return noexcept(name ## _impl(name ## _adl::adl_tag_prio, std::declval<Args>()...)); \
				} else if constexpr (name ## _detail::has_adl_ ## name ## _impl<Args...>) { \
					return noexcept(name ## _impl(std::declval<Args>()...)); \
				} else if constexpr(name ## _detail::has_adl_tag_ ## name ## _impl<Args...>) { \
					return noexcept(name ## _impl(name ## _adl::adl_tag, std::declval<Args>()...)); \
				} else { static_assert(name ## _detail::has_default_ ## name ## _impl<Args...>); \
					return noexcept(name ## _default:: name ## _impl(std::declval<Args>()...)); \
				} \
			} \
		public: \
			using is_transparent=void; \
			template<typename... Args> requires \
				name ## _detail::has_adl_tag_prio_ ## name ## _impl<Args...> || \
				name ## _detail::has_adl_ ## name ## _impl<Args...> || \
				name ## _detail::has_adl_tag_ ## name ## _impl<Args...> || \
				name ## _detail::has_default_ ## name ## _impl<Args...> \
			constexpr decltype(auto) operator()(Args&& ... args) const& noexcept( NoExcept<Args...>() ) { \
				if constexpr(name ## _detail::has_adl_tag_prio_ ## name ## _impl<Args...>) { \
					return name ## _impl(name ## _adl::adl_tag_prio, tc_move_if_owned(args)...); \
				} else if constexpr (name ## _detail::has_adl_ ## name ## _impl<Args...>) { \
					return name ## _impl(tc_move_if_owned(args)...); \
				} else if constexpr(name ## _detail::has_adl_tag_ ## name ## _impl<Args...>) { \
					return name ## _impl(name ## _adl::adl_tag, tc_move_if_owned(args)...); \
				} else { static_assert(name ## _detail::has_default_ ## name ## _impl<Args...>); \
					return name ## _default:: name ## _impl(tc_move_if_owned(args)...); \
				} \
			} \
		}; \
	} \
	inline no_adl::fn_ ## name constexpr name;

