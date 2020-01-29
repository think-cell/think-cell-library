
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "generic_macros.h"
#include "range_defines.h"
#include "range_fwd.h"

#include "range_adaptor.h"
#include "meta.h"
#include "minmax.h"

#include "tc_move.h"
#include "invoke.h"

namespace tc {
	namespace no_adl {
		template<typename Func, typename Src, typename Enable = void>
		struct transform_value final {};

		template<typename Func, typename Src>
		struct transform_value<Func, Src, tc::void_t<tc::decay_t<decltype(tc::invoke(std::declval<tc::decay_t<Func> const&>(), std::declval<Src>()))>>> final {
			using type = tc::decay_t<decltype(tc::invoke(std::declval<tc::decay_t<Func> const&>(), std::declval<Src>()))>;
		};
	}

	template<typename Func, typename Src>
	using transform_value_t = typename no_adl::transform_value<Func, Src>::type;

	template<typename Rng, typename Func>
	[[nodiscard]] auto transform(Rng&& rng, Func&& func)
		return_ctor_MAYTHROW( transform_adaptor<tc::decay_t<Func> BOOST_PP_COMMA() Rng >, (std::forward<Rng>(rng),std::forward<Func>(func)) )

	DEFINE_TAG_TYPE(transform_tag)
}

#define DEFINE_MEMBER_TRANSFORM_3(class_template, value_template, T) \
	private: \
		template<typename, typename /*=void gives warning on MSVC*/> \
		struct transform_result; \
		template<typename U> \
		struct transform_result<value_template<U>, void> : tc::type::identity<class_template<U>> {}; \
	public: \
		template<typename Func> \
		[[nodiscard]] typename transform_result<tc::transform_value_t<Func, value_template<T> const&>, void>::type transform(Func&& func) const& MAYTHROW { \
			return {tc::transform_tag, *this, std::forward<Func>(func)}; \
		} \
		template<typename Func> \
		[[nodiscard]] typename transform_result<tc::transform_value_t<Func, value_template<T>>, void>::type transform(Func&& func) && MAYTHROW { \
			return {tc::transform_tag, tc_move_always(*this), std::forward<Func>(func)}; \
		}

#define DEFINE_MEMBER_TRANSFORM_2(class_template, value_template) \
	DEFINE_MEMBER_TRANSFORM_3(class_template, value_template, T)

#define DEFINE_MEMBER_TRANSFORM_1(class_template) \
	DEFINE_MEMBER_TRANSFORM_2(class_template, tc::type::deducible_identity_t)

#define DEFINE_MEMBER_TRANSFORM(...) \
	EXPAND(BOOST_PP_OVERLOAD(DEFINE_MEMBER_TRANSFORM_, __VA_ARGS__)(__VA_ARGS__))
