
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "type_traits_fwd.h"

namespace tc {
	template<typename T, typename Fn>
	[[nodiscard]] constexpr decltype(auto) modified_impl(T&& t, Fn fn) MAYTHROW {
		if constexpr(std::is_lvalue_reference<T>::value || std::is_const<std::remove_reference_t<T> >::value) {
			auto tCopy=tc::decay_copy(t);
			fn(tCopy);
			return tCopy;
		} else {
			fn(t);
			return tc_move(t);
		}
	}
}

#define modified(obj, ...) tc::modified_impl(obj, [&](auto& _) MAYTHROW -> void { __VA_ARGS__; })

namespace tc {
	namespace no_adl {
		template< typename T >
		struct auto_cref_impl final {
			STATICASSERTSAME(std::remove_cv_t<T>,tc::decay_t<T>);
			using type=std::remove_cv_t<T> const;
		};

		template< typename T >
		struct auto_cref_impl< T& > final {
			using type=T const&;
		};

		template< typename T >
		struct auto_cref_impl< T&& > final {
			STATICASSERTSAME(std::remove_cv_t<T>,tc::decay_t<T>);
			using type=std::remove_cv_t<T> const;
		};

		template< typename T >
		struct auto_cref_return_impl final {
			STATICASSERTSAME(std::remove_cv_t<T>,tc::decay_t<T>);
			using type=std::remove_cv_t<T>;
		};

		template< typename T >
		struct auto_cref_return_impl< T& > final {
			using type=T const&;
		};

		template< typename T >
		struct auto_cref_return_impl< T&& > final {
			STATICASSERTSAME(std::remove_cv_t<T>,tc::decay_t<T>);
			using type=std::remove_cv_t<T>;
		};		
	}
};

#define auto_cref( var, ... ) \
	typename tc::no_adl::auto_cref_impl<decltype((__VA_ARGS__))>::type var = __VA_ARGS__
#define auto_cref_return( var, ... ) \
	typename tc::no_adl::auto_cref_return_impl<decltype((__VA_ARGS__))>::type var = __VA_ARGS__
