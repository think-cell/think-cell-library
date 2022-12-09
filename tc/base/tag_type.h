
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include <boost/preprocessor/control/expr_if.hpp>
#include <boost/preprocessor/facilities/is_empty.hpp>
#include <boost/preprocessor/logical/not.hpp>

#include <type_traits>

#define DERIVE_FROM_BASE_TAG(...) : __VA_ARGS__##_t

#define DEFINE_TAG_TYPE_STRUCT(tag_name, base_tag_name, derivable) \
	struct tag_name##_t derivable BOOST_PP_EXPR_IF(BOOST_PP_NOT(BOOST_PP_IS_EMPTY(base_tag_name)), DERIVE_FROM_BASE_TAG(base_tag_name)) { \
		constexpr explicit tag_name##_t() noexcept = default; /* to avoid implicit initialization from {} */ \
		using is_tag = void; \
	};

#define DEFINE_TAG_TYPE_VAR(tag_name, specifier) \
	specifier constexpr auto tag_name = tag_name##_t();

#define DEFINE_TAG_TYPE_BASE(tag_name, base_tag_name, derivable) \
	namespace no_adl { \
		DEFINE_TAG_TYPE_STRUCT(tag_name, base_tag_name, derivable) \
	} \
	using no_adl::tag_name##_t; \
	DEFINE_TAG_TYPE_VAR(tag_name, inline)

#define DEFINE_TAG_TYPE(tag_name) \
	DEFINE_TAG_TYPE_BASE(tag_name, , final)

#define DEFINE_NESTED_TAG_TYPE(tag_name) \
	DEFINE_TAG_TYPE_STRUCT(tag_name, , final) \
	DEFINE_TAG_TYPE_VAR(tag_name, static)

#define DEFINE_ADL_TAG_TYPE(tag_name) \
	DEFINE_TAG_TYPE_STRUCT(tag_name, , final) \
	DEFINE_TAG_TYPE_VAR(tag_name, inline)

#define DEFINE_TEMPLATE_TAG_TYPE(tag_name) \
	namespace no_adl { \
		template<typename T> \
		DEFINE_TAG_TYPE_STRUCT(tag_name, , final) \
	} \
	using no_adl::tag_name##_t; \
	template<typename T> \
	inline constexpr auto tag_name = tag_name##_t<T>();

namespace tc {
	DEFINE_TAG_TYPE(aggregate_tag) // tag to distinguish constructors that aggregate their single argument from templated copy constructors
	DEFINE_TAG_TYPE(func_tag)

	// DEFINE_FN(func) always defines a function void func(define_fn_dummy_t)
	// If that function did not exist, -> decltype( func(...) ) would not be
	// a valid statement and clang complains about that.
	DEFINE_TAG_TYPE(define_fn_dummy)

	template<typename T>
	concept tag = requires { typename std::remove_cv_t<T>::is_tag; };
}
