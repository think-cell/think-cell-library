
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/generic_macros.h"
#include "../base/tc_move.h"
#include "../base/invoke.h"

namespace tc {
	namespace no_adl {
		template<typename Func, typename Src, typename Enable = void>
		struct transform_value final {};

		template<typename Func, typename Src>
		struct transform_value<Func, Src, tc::void_t<tc::decay_t<decltype(tc::invoke(std::declval<tc::decay_t<Func> const&>(), std::declval<Src>()))>>> final {
			using type = tc::decay_t<decltype(tc::invoke(std::declval<tc::decay_t<Func> const&>(), std::declval<Src>()))>;
		};

		template<typename Func, typename Src, typename Enable = void>
		struct transform_output final {};

		template<typename Func, typename Src>
		struct transform_output<Func, Src, tc::void_t<decltype(tc::invoke(std::declval<tc::decay_t<Func> const&>(), std::declval<Src>()))>> final {
			using type = tc::remove_rvalue_reference_t<decltype(tc::invoke(std::declval<tc::decay_t<Func> const&>(), std::declval<Src>()))>;
		};
	}

	template<typename Func, typename Src>
	using transform_value_t = typename no_adl::transform_value<Func, Src>::type;

	template<typename Func, typename Src>
	using transform_output_t = typename no_adl::transform_output<Func, Src>::type;

	DEFINE_TAG_TYPE(transform_tag)
}

#define DEFINE_MEMBER_TRANSFORM_3(class_template, value_template, T) TC_FWD( \
	private: \
		template<typename, typename /*=void gives warning on MSVC*/> \
		struct transform_result; \
		template<typename U> \
		struct transform_result<value_template<U>, void> : tc::type::identity<class_template<U>> {}; \
	public: \
		template<std::size_t nDepth=0, typename Func, std::enable_if_t<0==nDepth>* = nullptr> \
		[[nodiscard]] constexpr auto transform(Func&& func) const& return_decltype_MAYTHROW( \
			typename transform_result<tc::transform_value_t<Func, value_template<T> const&>, void>::type(tc::transform_tag, *this, std::forward<Func>(func)) \
		) \
		template<std::size_t nDepth=0, typename Func, std::enable_if_t<0==nDepth>* = nullptr> \
		[[nodiscard]] constexpr auto transform(Func&& func) && return_decltype_MAYTHROW( \
			typename transform_result<tc::transform_value_t<Func, value_template<T>>, void>::type(tc::transform_tag, tc_move_always(*this), std::forward<Func>(func)) \
		) \
		template<std::size_t nDepth, typename Func, std::enable_if_t<0<nDepth>* = nullptr> \
		[[nodiscard]] constexpr auto transform(Func&& func) const& MAYTHROW { \
			return transform([&](auto const& val) MAYTHROW { return val.template transform<nDepth-1>(std::forward<Func>(func)); }); \
		} \
		template<std::size_t nDepth, typename Func, std::enable_if_t<0<nDepth>* = nullptr> \
		[[nodiscard]] constexpr auto transform(Func&& func) && MAYTHROW { \
			return transform([&](auto&& val) MAYTHROW { return tc_move_if_owned(val).template transform<nDepth-1>(std::forward<Func>(func)); }); \
		} \
	)

#define DEFINE_MEMBER_TRANSFORM_2(class_template, value_template) \
	DEFINE_MEMBER_TRANSFORM_3(class_template, value_template, T)

#define DEFINE_MEMBER_TRANSFORM_1(class_template) \
	DEFINE_MEMBER_TRANSFORM_2(class_template, tc::type::deducible_identity_t)

#define DEFINE_MEMBER_TRANSFORM(...) \
	TC_EXPAND(BOOST_PP_OVERLOAD(DEFINE_MEMBER_TRANSFORM_, __VA_ARGS__)(__VA_ARGS__))
