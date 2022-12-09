
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "type_traits.h"
#include "template_func.h"
#include "return_decltype.h"
#include <array>

//////////////////////////////////////////////////////////////////
// generic initialization and assignment between different types
//
// Copy/move/assignment cannot be overloaded for third party types
// or builtins.
//
// If tc::explicit_cast<T, S> is used to convert a builtin numeric type S into
// another builtin numeric type T, rounding and range checks are
// applied.
//
// For other types, the default behavior of explicit_cast is
//   A a(tc::explicit_cast<A>(b))                       is equivalent to   A(a) ,
//   a=tc::explicit_cast<remove_reference<A>::type>(b)  is equivalent to   a=b ,
// i.e., the argument b is forwarded to the copy/move/assignment operators
// of A.
//
// Specialize explicit_convert if additional conversions for specific target
// types are required.

namespace tc {
	DEFINE_TAG_TYPE(fill_tag)
	DEFINE_TAG_TYPE(range_tag)

	namespace no_adl {
		template<typename T>
		struct char_limits;

		template<>
		struct char_limits<char> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 4;

			[[nodiscard]] static constexpr bool interval_in_range(unsigned int nFirst, unsigned int nLast) noexcept {
				return nLast <= 0x7f;
			}
		};

		template<>
		struct char_limits<char16_t> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 2;

			[[nodiscard]] static constexpr bool interval_in_range(unsigned int nFirst, unsigned int nLast) noexcept {
				return nLast <= 0xd7ff || (0xe000 <= nFirst && nLast <= 0xffff);
			}
		};

		template<>
		struct char_limits<char32_t> {
			static constexpr std::size_t c_nMaxCodeUnitsPerCodePoint = 1;

			[[nodiscard]] static constexpr bool interval_in_range(unsigned int nFirst, unsigned int nLast) noexcept {
				return nLast <= 0xd7ff || (0xe000 <= nFirst && nLast <= 0x10ffff);
			}
		};

		struct char_limits_undefined_dummy {};

		template<>
		struct char_limits<wchar_t> :
			std::conditional_t<
				2 == sizeof(wchar_t),
				char_limits<char16_t>,
				std::conditional_t<
					4 == sizeof(wchar_t),
					char_limits<char32_t>,
					char_limits_undefined_dummy
				>
			>
		{};

	}
	using no_adl::char_limits;

	template <typename T>
	[[nodiscard]] constexpr bool char_in_range(unsigned int const n) noexcept {
		return char_limits<T>::interval_in_range(n, n);
	}

	namespace explicit_convert_default {
		// This is the default explicit_convert, not a specialization, because aggregate initialization must have lower priority than other specializations which could also be aggregate types
		// Aggregate types are tc::is_safely_constructible from same type (copy/move). This is handled in the default explicit_cast below.
		template<typename TTarget, typename... Args> requires std::is_class<TTarget>::value && std::is_aggregate<TTarget>::value
		constexpr auto explicit_convert_impl(tc::type::identity<TTarget>, Args&&... args)
			return_decltype_MAYTHROW( TTarget{std::forward<Args>(args)...} )

		template<typename T, std::size_t N, typename... Args>
		std::array<T, N> explicit_convert_impl(tc::type::identity<std::array<T, N>>, Args&&... args) = delete; // explicitly delete direct list construction of std::array
	}

	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(explicit_convert)

	template<typename TTarget, typename... Args, std::enable_if_t<!tc::is_safely_constructible<std::remove_cv_t<TTarget>, Args&&...>::value>* = nullptr>
	[[nodiscard]] constexpr auto explicit_cast(Args&&... args)
		return_decltype_MAYTHROW(tc::explicit_convert(tc::type::identity<std::remove_cv_t<TTarget>>(), std::forward<Args>(args)...))

	template<typename TTarget, typename... Args, std::enable_if_t<tc::is_safely_constructible<std::remove_cv_t<TTarget>, Args&&...>::value>* = nullptr>
	[[nodiscard]] constexpr auto explicit_cast(Args&&... args)
		return_ctor_MAYTHROW(std::remove_cv_t<TTarget>, (std::forward<Args>(args)...))

	namespace is_explicit_castable_detail {
		template<typename TTarget, typename ArgList, typename Enable=void>
		struct is_explicit_castable : tc::constant<false> {};

		template<typename TTarget, typename... Args>
		struct is_explicit_castable<TTarget, tc::type::list<Args...>, tc::void_t<decltype(
			tc::explicit_cast<TTarget>(std::declval<Args>()...)
		)>> : tc::constant<true> {};
	}
	template<typename TTarget, typename... Args>
	using is_explicit_castable=is_explicit_castable_detail::is_explicit_castable<TTarget,tc::type::list<Args...>>;

	// Using decltype(this->member) ensures casting to the correct type even if member is shadowed (but disallows base classes).
	#define MEMBER_INIT_CAST(member, ...) member(tc::explicit_cast<decltype(this->member)>(__VA_ARGS__))

	namespace no_adl {
		struct bool_context final {
			template< typename T >
			constexpr bool_context(T const& t) noexcept
				: MEMBER_INIT_CAST(m_b, t)
			{}
			constexpr operator bool() const& noexcept { return m_b; }
		private:
			bool m_b;
		};
	}
	using no_adl::bool_context;
}
