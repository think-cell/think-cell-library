
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "functors.h"
#include "tc_move.h"

#include "return_decltype.h"
#include "type_traits_fwd.h"
#include "template_func.h"

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
		namespace derived_cast_internal_default {
			template<typename To, typename From>
			[[nodiscard]] constexpr same_cvref_t< To, From&&> derived_cast_internal_impl( tc::type::identity<To>, From&& t ) noexcept {
				static_assert( std::is_base_of<std::remove_reference_t<From>, To>::value, "derived_cast is for downcasts only.");
				return static_cast< same_cvref_t< To, From&&> >(t);
			}

			template<typename To, typename From>
			[[nodiscard]] constexpr same_cvref_t< To, From>* derived_cast_internal_impl( tc::type::identity<To>, From* pt ) noexcept {
				static_assert( std::is_base_of<std::remove_pointer_t<From>, To>::value, "derived_cast is for downcasts only.");
				return static_cast< same_cvref_t< To, From>* >(pt);
			}
		}

		DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(derived_cast_internal)
	}

	template<typename To, typename From>
	[[nodiscard]] constexpr decltype(auto) derived_cast(From&& t) noexcept {
		return tc::derived_cast_detail::derived_cast_internal(tc::type::identity<To>(), std::forward<From>(t));
	}

	//-------------------------------------------------------------------------------------------------------------------------

	namespace underlying_cast_default {
		template< typename T > requires tc::is_char<T>::value
		[[nodiscard]] constexpr auto underlying_cast_impl(T t)
			return_decltype_noexcept( static_cast<typename boost::uint_t<sizeof(T)*8>::exact>(t) )

		template< typename Enum > requires std::is_enum<Enum>::value
		[[nodiscard]] constexpr std::underlying_type_t<Enum> underlying_cast_impl( Enum e ) noexcept {
			return static_cast<std::underlying_type_t<Enum>>(e);
		}

		// No implicit conversions to bool
		template< typename T > requires std::is_same<T, bool>::value
		[[nodiscard]] constexpr unsigned char underlying_cast_impl(T b) noexcept {
			STATICASSERTEQUAL(sizeof(bool), sizeof(unsigned char));
			return static_cast<unsigned char>(b);
		}
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(underlying_cast)

	template< typename T >
	[[nodiscard]] constexpr auto unsigned_cast(T t) noexcept code_return_decltype(
		static_assert( tc::is_actual_integer<T>::value );
		_ASSERTE( 0<=t );,
		static_cast<std::make_unsigned_t<T>>(t)
	)

	template< typename T >
	[[nodiscard]] constexpr std::make_signed_t<T> signed_cast(T t) noexcept {
		static_assert( tc::is_actual_integer<T>::value );
		if constexpr (!std::is_signed<T>::value) {
			_ASSERTE(t <= tc::unsigned_cast(std::numeric_limits<std::make_signed_t<T>>::max()));
		}
		return static_cast<std::make_signed_t<T>>(t);
	}

	MODIFY_WARNINGS_BEGIN(((disable)(4180))) // qualifier applied to function type has no meaning; ignored

		template< typename T >
		[[nodiscard]] constexpr T const& as_const(T& t) noexcept { // intention is to avoid side-effects
			return static_cast<T const&>(t);
		}
		template <typename T>
		[[nodiscard]] constexpr T&& as_const(T&& t) noexcept { // needed in generic code when both values and references can occur
			static_assert(!std::is_lvalue_reference<T&&>::value);
			return std::forward<T&&>(t);
		}

		template< typename T >
		[[nodiscard]] constexpr std::remove_const_t<T>& as_mutable(T& t) noexcept {
			return const_cast<std::remove_const_t<T>&>(t);
		}

	MODIFY_WARNINGS_END

	template< typename T >
	[[nodiscard]] constexpr T const* make_const_ptr( T const* pt ) noexcept {
		return pt;
	}

	template< typename T >
	[[nodiscard]] constexpr T* make_mutable_ptr( T const* pt ) noexcept {
		return const_cast<T*>(pt);
	}

	/////////////////////////////////////////////
	// void_cast

	template<typename Dst, typename Src>
	[[nodiscard]] Dst* void_cast(Src* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst*>(p);
	}

	template<typename Dst, typename Src>
	[[nodiscard]] Dst const* void_cast(Src const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst const*>(p);
	}

	template<typename Dst, typename Src>
	[[nodiscard]] Dst volatile* void_cast(Src volatile* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst volatile*>(p);
	}

	template<typename Dst, typename Src>
	[[nodiscard]] Dst volatile const* void_cast(Src volatile const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		// static_assert(!std::is_void<Dst>::value); // practical for generic code to allow it
		return static_cast<Dst volatile const*>(p);
	}

	/////////////////////////////////////////////
	// implicit_cast

	template<typename TTarget, typename TSource> requires (!tc::is_actual_integer<std::remove_reference_t<TSource>>::value) && tc::is_safely_convertible<TSource&&, TTarget>::value
	[[nodiscard]] constexpr TTarget implicit_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	// bit filed cannot bind to universal reference
MODIFY_WARNINGS_BEGIN(((disable)(4244))) // disable warning C4244: conversion from 'int' to 'float', possible loss of data
	template<typename TTarget, typename TSource> requires tc::is_actual_integer<TSource>::value && tc::is_safely_convertible<TSource&&, TTarget>::value
	[[nodiscard]] constexpr TTarget implicit_cast(TSource src) noexcept {
		return src;
	}
MODIFY_WARNINGS_END

	/////////////////////////////////////////////
	// reluctant_implicit_cast
	// Returns a reference to its argument whenever possible, otherwise performs an implicit conversion.

	template<typename TTarget, typename TSource>
	[[nodiscard]] std::conditional_t<
		tc::is_base_of_decayed<TTarget, TSource>::value,
		TSource&&,
		TTarget
	> reluctant_implicit_cast(TSource&& src) noexcept {
		STATICASSERTSAME(std::remove_cvref_t<TTarget>, TTarget);
		return std::forward<TSource>(src);
	}

	/////////////////////////////////////////////
	// reluctant_static_cast
	// Returns a reference to its argument whenever possible, otherwise performs an explicit conversion.

	template<typename TTarget, typename TSource> requires tc::is_base_of_decayed<TTarget, TSource>::value
	[[nodiscard]] TSource&& reluctant_static_cast(TSource&& src) noexcept {
		STATICASSERTSAME(std::remove_cvref_t<TTarget>, TTarget);
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource>
	[[nodiscard]] TTarget reluctant_static_cast(TSource&& src) noexcept {
		STATICASSERTSAME(std::remove_cvref_t<TTarget>, TTarget);
		return static_cast<TTarget>(std::forward<TSource>(src));
	}

	/////////////////////////////////////////////
	// as_c_str

	template< typename Char, typename Traits, typename Alloc >
	[[nodiscard]] Char const* as_c_str(std::basic_string< Char, Traits, Alloc > const& str) noexcept
	{
		return str.data(); // since C++ 11, performs same function as c_str()
	}

	template< typename Char, typename Traits, typename Alloc >
	[[nodiscard]] Char* as_c_str(std::basic_string< Char, Traits, Alloc >& str) noexcept
	{
		return str.data(); // since C++ 11, performs same function as c_str()
	}

	template< typename Char, typename Traits, typename Alloc >
	[[nodiscard]] Char* as_c_str(std::basic_string< Char, Traits, Alloc >&& str) noexcept
	{
		return str.data(); // since C++ 11, performs same function as c_str()
	}

	template<typename Char> requires tc::is_char<Char>::value
	[[nodiscard]] constexpr Char const* as_c_str(Char const* psz) noexcept {
		return psz;
	}

	template<typename Char> requires tc::is_char<Char>::value
	[[nodiscard]] constexpr Char* as_c_str(Char* psz) noexcept {
		return psz;
	}

#ifdef TC_PRIVATE
	// Prevents implicit conversion from std::vector<wchar_t> to boost::filesystem::path on Windows, which causes a dangling pointer to be returned
	template<typename T> requires std::is_same<T, boost::filesystem::path>::value
	[[nodiscard]] inline boost::filesystem::path::value_type const* as_c_str(T const& fspath) noexcept {
		return fspath.c_str();
	}
#endif
}

