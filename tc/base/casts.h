
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "functors.h"
#include "move.h"

#include "return_decltype.h"
#include "type_traits_fwd.h"
#include "template_func.h"
#include "explicit_cast_fwd.h"

#include <boost/integer.hpp>
#ifndef __EMSCRIPTEN__
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations" // sprintf is deprecated in Xcode14.1 RC
#endif
#include <boost/filesystem/path.hpp>
#ifdef __clang__
#pragma clang diagnostic pop
#endif
#endif

#include <type_traits>

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {
	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// base_cast
	// (cannot be implemented like derived_cast because when deriving protected, derived to base cast is often publicly inaccessible)

	#pragma push_macro("BASE_CAST_IMPL")
	#define BASE_CAST_IMPL(cvref) \
	template<typename Dst> requires std::is_class<Dst>::value \
	[[nodiscard]] constexpr Dst cvref base_cast(typename tc::type::identity<Dst>::type cvref t) noexcept { \
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst); \
		return static_cast<Dst cvref>(t); \
	}
	BASE_CAST_IMPL(&)
	BASE_CAST_IMPL(&&)
	BASE_CAST_IMPL(*)
	BASE_CAST_IMPL(const&)
	BASE_CAST_IMPL(const&&)
	BASE_CAST_IMPL(const*)
	BASE_CAST_IMPL(volatile&)
	BASE_CAST_IMPL(volatile&&)
	BASE_CAST_IMPL(volatile*)
	BASE_CAST_IMPL(volatile const&)
	BASE_CAST_IMPL(volatile const&&)
	BASE_CAST_IMPL(volatile const*)
	#pragma pop_macro("BASE_CAST_IMPL")

	/////////////////////////////////////////////
	// derived_cast
	
	namespace derived_cast_detail {
		template<typename To, typename From>
		constexpr void check_derived_cast(From const& obj) noexcept {
			if constexpr (!std::is_same<To, From>::value && std::is_polymorphic<From>::value) {
				_ASSERT(dynamic_cast<To const*>(std::addressof(obj)));
			}
		}

		namespace derived_cast_internal_default {
			template<typename To, typename From, bool bChecked>
			[[nodiscard]] constexpr same_cvref_t< To, From&&> derived_cast_internal_impl(tc::type::identity<To>, From&& t, std::bool_constant<bChecked>) noexcept {
				static_assert( tc::derived_from<To, std::remove_reference_t<From>>, "derived_cast is for downcasts only.");
				if constexpr (bChecked) {
					check_derived_cast<To>(t);
				}
				return static_cast< apply_cvref_t< To, From&&> >(t);
			}

			template<typename To, typename From, bool bChecked>
			[[nodiscard]] constexpr same_cvref_t< To, From>* derived_cast_internal_impl(tc::type::identity<To>, From* pt, std::bool_constant<bChecked>) noexcept {
				static_assert( tc::derived_from<To, std::remove_pointer_t<From>>, "derived_cast is for downcasts only.");
				if constexpr (bChecked) {
					if (nullptr != pt) check_derived_cast<To>(*pt);
				}
				return static_cast< apply_cvref_t< To, From>* >(pt);
			}
		}

		DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(derived_cast_internal)
	}

	template<typename To, typename From>
	[[nodiscard]] constexpr decltype(auto) derived_cast(From&& t) noexcept {
		STATICASSERTSAME(std::remove_reference_t<To>, To);
		return tc::derived_cast_detail::derived_cast_internal(tc::type::identity<To>(), tc_move_if_owned(t), /*bChecked*/tc::constant<true>());
	}

	template<typename To, typename From>
	[[nodiscard]] constexpr decltype(auto) unchecked_derived_cast(From&& t) noexcept {
		STATICASSERTSAME(std::remove_reference_t<To>, To);
		return tc::derived_cast_detail::derived_cast_internal(tc::type::identity<To>(), tc_move_if_owned(t), /*bChecked*/tc::constant<false>());
	}

	/////////////////////////////////////////////
	// to_underlying

	namespace to_underlying_default {
		template< tc::char_type T >
		[[nodiscard]] constexpr auto to_underlying_impl(T t)
			return_decltype_noexcept( static_cast<typename boost::uint_t<sizeof(T)*8>::exact>(t) )

		template< tc::enum_type T >
		[[nodiscard]] constexpr auto to_underlying_impl( T e ) noexcept {
			return static_cast<std::underlying_type_t<T>>(e);
		}

		// No implicit conversions to bool
		template< std::same_as<bool> T >
		[[nodiscard]] constexpr auto to_underlying_impl(T b) noexcept {
			STATICASSERTEQUAL(sizeof(bool), sizeof(unsigned char));
			return static_cast<unsigned char>(b);
		}
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(to_underlying)

	template<typename T>
	using underlying_type_t = decltype(tc::to_underlying(std::declval<T>()));

	namespace from_underlying_detail {
		namespace from_underlying_default {
			template< tc::char_type T >
			[[nodiscard]] constexpr auto from_underlying_impl(tc::type::identity<T>, underlying_type_t<T> t) noexcept {
				// We don't need to do any checks, the underlying type has the same size.
				return static_cast<T>(t);
			}

			template< tc::enum_type T >
			[[nodiscard]] constexpr auto from_underlying_impl(tc::type::identity<T>, underlying_type_t<T> e) noexcept {
				// We cannot do checks on arbitrary enums; there's a specialization for contiguous enums.
				return static_cast<T>(e);
			}

			[[nodiscard]] constexpr auto from_underlying_impl(tc::type::identity<bool>, underlying_type_t<bool> b) noexcept {
				_ASSERTANYOF(b, (0)(1));
				return static_cast<bool>(b);
			}
		}

		DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(from_underlying)
	}

	template<typename T, typename U>
		requires tc::explicit_castable_from<tc::underlying_type_t<T>, U>
	[[nodiscard]] constexpr T from_underlying(const U& value) return_MAYTHROW(
		from_underlying_detail::from_underlying(tc::type::identity<T>{}, tc::explicit_cast<tc::underlying_type_t<T>>(value))
	)

	/////////////////////////////////////////////
	// as_unsigned/signed

	template< typename T >
	[[nodiscard]] constexpr auto as_unsigned(T t) noexcept code_return_decltype(
		static_assert( tc::actual_integer<T> );
		_ASSERTE( 0<=t );,
		static_cast<std::make_unsigned_t<T>>(t)
	)

	template< typename T >
	[[nodiscard]] constexpr std::make_signed_t<T> as_signed(T t) noexcept {
		static_assert( tc::actual_integer<T> );
		if constexpr (!std::is_signed<T>::value) {
			_ASSERTE(t <= tc::as_unsigned(std::numeric_limits<std::make_signed_t<T>>::max()));
		}
		return static_cast<std::make_signed_t<T>>(t);
	}

	/////////////////////////////////////////////
	// const casts

	MODIFY_WARNINGS_BEGIN(((disable)(4180))) // qualifier applied to function type has no meaning; ignored

		template< typename T >
		[[nodiscard]] constexpr T const& as_const(T& t) noexcept { // intention is to avoid side-effects
			return static_cast<T const&>(t);
		}
		template <typename T>
		[[nodiscard]] constexpr T&& as_const(T&& t) noexcept { // needed in generic code when both values and references can occur
			static_assert(!std::is_lvalue_reference<T&&>::value);
			return static_cast<T&&>(t);
		}

		template< typename T >
		[[nodiscard]] constexpr std::remove_const_t<T>& as_mutable(T& t) noexcept {
			return const_cast<std::remove_const_t<T>&>(t);
		}

	MODIFY_WARNINGS_END

	template< typename T >
	[[nodiscard]] constexpr T const* as_const_ptr( T const* pt ) noexcept {
		return pt;
	}

	template< typename T >
	[[nodiscard]] constexpr T* as_mutable_ptr( T const* pt ) noexcept {
		return const_cast<T*>(pt);
	}

	/////////////////////////////////////////////
	// void_cast

	template<typename Dst, typename Src>
	[[nodiscard]] Dst* void_cast(Src* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(!std::is_reference<Dst>::value);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst*>(p);
	}

	template<typename Dst, typename Src>
	[[nodiscard]] Dst const* void_cast(Src const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(!std::is_reference<Dst>::value);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst const*>(p);
	}

	template<typename Dst, typename Src>
	[[nodiscard]] Dst volatile* void_cast(Src volatile* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(!std::is_reference<Dst>::value);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst volatile*>(p);
	}

	template<typename Dst, typename Src>
	[[nodiscard]] Dst volatile const* void_cast(Src volatile const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(!std::is_reference<Dst>::value);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst volatile const*>(p);
	}

	/////////////////////////////////////////////
	// implicit_cast

	template<typename TTarget, typename TSource> requires (!tc::actual_integer<std::remove_reference_t<TSource>>) && tc::safely_convertible_to<TSource&&, TTarget>
	[[nodiscard]] constexpr TTarget implicit_cast(TSource&& src) noexcept {
		return tc_move_if_owned(src);
	}

	// bit filed cannot bind to universal reference
MODIFY_WARNINGS_BEGIN(((disable)(4244))) // disable warning C4244: conversion from 'int' to 'float', possible loss of data
	template<typename TTarget, typename TSource> requires tc::actual_integer<TSource> && tc::safely_convertible_to<TSource&&, TTarget>
	[[nodiscard]] constexpr TTarget implicit_cast(TSource src) noexcept {
		return src;
	}
MODIFY_WARNINGS_END

	/////////////////////////////////////////////
	// reluctant_implicit_cast
	// Returns a reference to its argument whenever possible, otherwise performs an implicit conversion.

	template<typename TTarget, typename TSource>
	[[nodiscard]] std::conditional_t<
		tc::decayed_derived_from<TSource, TTarget>,
		TSource&&,
		TTarget
	> reluctant_implicit_cast(TSource&& src) noexcept {
		STATICASSERTSAME(std::remove_cvref_t<TTarget>, TTarget);
		return tc_move_if_owned(src);
	}

	/////////////////////////////////////////////
	// as_c_str
	namespace as_c_str_default {
		template< typename Char, typename Traits, typename Alloc >
		[[nodiscard]] Char const* as_c_str_impl(std::basic_string< Char, Traits, Alloc > const& str) noexcept
		{
			return str.data(); // since C++ 11, performs same function as c_str(). cannot use tc::ptr_begin to avoid circular dependency
		}

		template< typename Char, typename Traits, typename Alloc >
		[[nodiscard]] Char* as_c_str_impl(std::basic_string< Char, Traits, Alloc >& str) noexcept
		{
			return str.data(); // since C++ 11, performs same function as c_str(). cannot use tc::ptr_begin to avoid circular dependency
		}

		template< typename Char, typename Traits, typename Alloc >
		[[nodiscard]] Char* as_c_str_impl(std::basic_string< Char, Traits, Alloc >&& str) noexcept
		{
			return str.data(); // since C++ 11, performs same function as c_str(). cannot use tc::ptr_begin to avoid circular dependency
		}

		template<tc::char_type Char>
		[[nodiscard]] constexpr Char const* as_c_str_impl(Char const* psz) noexcept {
			return psz;
		}

		template<tc::char_type Char>
		[[nodiscard]] constexpr Char* as_c_str_impl(Char* psz) noexcept {
			return psz;
		}

	#ifdef TC_PRIVATE
		// Prevents implicit conversion from std::vector<wchar_t> to boost::filesystem::path on Windows, which causes a dangling pointer to be returned
		template<typename T> requires std::is_same<T, boost::filesystem::path>::value
		[[nodiscard]] inline boost::filesystem::path::value_type const* as_c_str_impl(T const& fspath) noexcept {
			return fspath.c_str();
		}
	#endif
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(as_c_str)
}

