
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits.h"

namespace tc {
	namespace no_adl {
		template<typename T, typename Enable=void>
		struct accessor_return_type final {
			static_assert(std::is_array<T>::value || tc::is_decayed<T>::value);
			using const_ref_type = T const&;
			using ref_ref_type = T&&;
			using const_ref_ref_type = T const&&;
		};
	
		template<typename T>
		struct accessor_return_type<T, std::enable_if_t<
			!std::is_union<std::remove_reference_t<T>>::value &&
			!std::is_class<std::remove_reference_t<T>>::value &&
			!std::is_array<std::remove_reference_t<T>>::value
		>> final {
			static_assert(tc::is_decayed<T>::value);
			using const_ref_type = T;
			using ref_ref_type = T;
			using const_ref_ref_type = T;
		};
	}

	namespace no_adl {
		template<typename T, typename Enable=void>
		struct has_ref_ref_accessor final: std::false_type {};

		template<typename T>
		struct has_ref_ref_accessor<T, tc::void_t<typename accessor_return_type<T>::ref_ref_type>> final: std::true_type {};

		template<typename T, typename Enable=void>
		struct has_const_ref_ref_accessor final: std::false_type {};

		template<typename T>
		struct has_const_ref_ref_accessor<T, tc::void_t<typename accessor_return_type<T>::const_ref_ref_type>> final: std::true_type {};
	}

	using no_adl::has_ref_ref_accessor;
	using no_adl::has_const_ref_ref_accessor;
}

#define EXPAND(...) __VA_ARGS__

#define DEFINE_MEMBER_WITHOUT_INIT(type, name) \
		type name;
#define DEFINE_MEMBER_WITH_INIT(type, name, ...) \
		type name = __VA_ARGS__;

#define DEFINE_MEMBER_BASE(type, ...) \
	EXPAND(EXPAND(BOOST_PP_IF(BOOST_PP_GREATER(BOOST_PP_VARIADIC_SIZE(__VA_ARGS__), 1), DEFINE_MEMBER_WITH_INIT, DEFINE_MEMBER_WITHOUT_INIT))(EXPAND(type), __VA_ARGS__))

#define DEFINE_ACCESSORS_BASE(type, funcname, name) \
	public: \
		constexpr typename tc::no_adl::accessor_return_type<type>::const_ref_type funcname() const& noexcept { return name; } \
		\
		template<ENABLE_SFINAE, std::enable_if_t<tc::has_ref_ref_accessor<SFINAE_TYPE(EXPAND(type))>::value>* = nullptr> \
		constexpr typename tc::no_adl::accessor_return_type<SFINAE_TYPE(EXPAND(type))>::ref_ref_type funcname() && noexcept { return tc_move(name); } \
		\
		template<ENABLE_SFINAE, std::enable_if_t<!tc::has_ref_ref_accessor<SFINAE_TYPE(EXPAND(type))>::value>* = nullptr> \
		constexpr type&& funcname() && noexcept = delete; /* Visual Studio gives improper error message if it returns a dummy type */ \
		\
		template<ENABLE_SFINAE, std::enable_if_t<tc::has_const_ref_ref_accessor<SFINAE_TYPE(EXPAND(type))>::value>* = nullptr> \
		constexpr typename tc::no_adl::accessor_return_type<SFINAE_TYPE(EXPAND(type))>::const_ref_ref_type funcname() const&& noexcept { return std::move(name); } \
		\
		template<ENABLE_SFINAE, std::enable_if_t<!tc::has_const_ref_ref_accessor<SFINAE_TYPE(EXPAND(type))>::value>* = nullptr> \
		constexpr type const&& funcname() const&& noexcept = delete; /* Visual Studio gives improper error message if it returns a dummy type */

#define DEFINE_MEMBER_AND_ACCESSORS(type, funcname, ...) /* type, funcname, name(, value) */ \
	DEFINE_MEMBER_BASE(EXPAND(type), __VA_ARGS__) \
	DEFINE_ACCESSORS_BASE(EXPAND(type), funcname, BOOST_PP_VARIADIC_ELEM(0, __VA_ARGS__)) \
	private:

#define DEFINE_PROTECTED_MEMBER_AND_ACCESSORS(type, funcname, ...) /* type, funcname, name(, value) */ \
	DEFINE_MEMBER_BASE(EXPAND(type), __VA_ARGS__) \
	DEFINE_ACCESSORS_BASE(EXPAND(type), funcname, BOOST_PP_VARIADIC_ELEM(0, __VA_ARGS__)) \
	protected:
