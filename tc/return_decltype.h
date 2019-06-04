
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
		template<typename T>
		struct return_decltype_assert_no_xvalue final {
			static_assert(!std::is_rvalue_reference<T>::value, "choose between return_decltype_xvalue_by_ref and return_decltype_xvalue_by_val");
			using type = T;
		};
	}
	template<typename T>
	using return_decltype_t = typename no_adl::return_decltype_assert_no_xvalue<T>::type;

#define return_decltype(...) -> tc::return_decltype_t<decltype((__VA_ARGS__))> { \
	return (__VA_ARGS__); \
}

#define code_return_decltype(code, ...) -> tc::return_decltype_t<decltype((__VA_ARGS__))> { \
	code \
	return (__VA_ARGS__); \
}

#define return_by_val(...) -> tc::decay_t<decltype((__VA_ARGS__))> { \
	return (__VA_ARGS__); \
}

#define return_decltype_xvalue_by_ref(...) -> decltype(auto) { \
	return (__VA_ARGS__); \
}

	template<typename T>
	using xvalue_decay_t = std::conditional_t<std::is_rvalue_reference<T>::value,
		tc::decay_t<T>,
		T
	>;

#define return_decltype_xvalue_by_val(...) -> tc::xvalue_decay_t<decltype((__VA_ARGS__))> { \
	return (__VA_ARGS__); \
}

	// helper function to decay rvalues when decltype cannot be used because of lambdas in the expression
	// note that contrary to return_decltype_xvalue_by_val, this helper function also decays prvalues
	template<typename T>
	xvalue_decay_t<T&&> lvalue_or_decay(T&& t) noexcept {
		return std::forward<T>(t);
	}
}

#define return_ctor(T, ...) -> T { return T __VA_ARGS__ ; }
#define code_return_ctor(code, T, ...) -> T { \
	code \
	return T __VA_ARGS__ ; \
}

#include "range_defines.h"
namespace decltype_return_test {
	struct A {
		int a;
		void access_a() & noexcept {
			STATICASSERTSAME(decltype(a), int);
			STATICASSERTSAME(decltype((a)), int&);
		}
		int& b;
		void access_b() & noexcept {
			STATICASSERTSAME(decltype(b), int&);
			STATICASSERTSAME(decltype((b)), int&);
		}
		int&& c;
		void access_c() & noexcept {
			STATICASSERTSAME(decltype(c), int&&);
			STATICASSERTSAME(decltype((c)), int&);
		}
	};
}

