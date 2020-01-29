// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <type_traits>

namespace tc
{
	template<typename FuncTry, typename FuncFinally>
	decltype(auto) try_cleanup_impl(FuncTry& funcTry, FuncFinally& funcFinally, std::bool_constant<false>) {
		try {
			return funcTry();
		} catch (...) {
			funcFinally();
			throw;
		}
	}

	template<typename FuncTry, typename FuncFinally>
	constexpr decltype(auto) try_cleanup_impl(FuncTry& funcTry, FuncFinally&, std::bool_constant<true>) noexcept {
		return funcTry();
	}

	template<typename FuncTry, typename FuncFinally>
	static constexpr decltype(auto) try_finally(FuncTry funcTry, FuncFinally funcFinally) noexcept(noexcept(funcTry()) && noexcept(funcFinally())) {
		if constexpr (std::is_same<decltype(funcTry()), void>::value) {
			try_cleanup_impl(funcTry, funcFinally, std::bool_constant<noexcept(funcTry())>());
			funcFinally();
		} else {
			decltype(auto) result = try_cleanup_impl(funcTry, funcFinally, std::bool_constant<noexcept(funcTry())>());
			funcFinally();
			return static_cast<decltype(result)>(result);
		}
	}

	template<typename FuncTry, typename FuncCleanup>
	static constexpr decltype(auto) try_cleanup(FuncTry funcTry, FuncCleanup funcCleanup) noexcept(noexcept(funcTry())) {
		return try_cleanup_impl(funcTry, funcCleanup, std::bool_constant<noexcept(funcTry())>());
	}
}
