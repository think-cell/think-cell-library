
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#define DEFINE_TAG_TYPE_BASE(tag_name) \
	struct tag_name##_t final { constexpr explicit tag_name##_t() noexcept = default; };

#define DEFINE_TAG_TYPE_VAR(tag_name, specifier) \
	specifier constexpr auto tag_name = tag_name##_t();

#define DEFINE_TAG_TYPE(tag_name) \
	namespace no_adl { \
		DEFINE_TAG_TYPE_BASE(tag_name) \
	} \
	using no_adl::tag_name##_t; \
	DEFINE_TAG_TYPE_VAR(tag_name, inline)

#define DEFINE_NESTED_TAG_TYPE(tag_name) \
	DEFINE_TAG_TYPE_BASE(tag_name) \
	DEFINE_TAG_TYPE_VAR(tag_name, static)

#define DEFINE_ADL_TAG_TYPE(tag_name) \
	DEFINE_TAG_TYPE_BASE(tag_name) \
	DEFINE_TAG_TYPE_VAR(tag_name, inline)

#define DEFINE_TEMPLATE_TAG_TYPE(tag_name) \
	namespace no_adl { \
		template<typename T> \
		DEFINE_TAG_TYPE_BASE(tag_name) \
	} \
	using no_adl::tag_name##_t; \
	template<typename T> \
	inline constexpr auto tag_name = tag_name##_t<T>();

namespace tc {
	DEFINE_TAG_TYPE(aggregate_tag) // tag to distinguish constructors that aggregate their single argument from templated copy constructors
}