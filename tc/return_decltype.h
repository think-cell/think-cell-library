
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
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

#define return_decltype_MAYTHROW(...) noexcept(noexcept((__VA_ARGS__))) -> tc::return_decltype_t<decltype((__VA_ARGS__))> { \
	return (__VA_ARGS__); \
}

#define return_decltype_noexcept(...) noexcept -> tc::return_decltype_t<decltype((__VA_ARGS__))> { \
	static_assert(noexcept((__VA_ARGS__)), "expression is not noexcept"); \
	return (__VA_ARGS__); \
}

// TODO C++20: When lambdas are allowed in unevaluated contexts, they will be allowed inside noexcept() expressions, and
// any usage of return_decltype_NOEXCEPT(XYZ) should be replaced with return_decltype_noexcept(NOEXCEPT(XYZ)), and
// we can remove NOEXCEPT_NO_LAMBDA
#define return_decltype_NOEXCEPT(...) noexcept -> tc::return_decltype_t<decltype((__VA_ARGS__))> { \
	return NOEXCEPT_NO_LAMBDA(__VA_ARGS__); \
}

#define return_MAYTHROW(...) noexcept(noexcept((__VA_ARGS__))) { \
	return (__VA_ARGS__); \
}

#define code_return_decltype(code, ...) -> tc::return_decltype_t<decltype((__VA_ARGS__))> { \
	code \
	return (__VA_ARGS__); \
}

#define return_by_val(...) -> tc::decay_t<decltype((__VA_ARGS__))> { \
	return (__VA_ARGS__); \
}

#define return_decltype_xvalue_by_ref_MAYTHROW(...) noexcept(noexcept((__VA_ARGS__))) -> decltype((__VA_ARGS__)) { \
	return (__VA_ARGS__); \
}

#define return_decltype_xvalue_by_ref_noexcept(...) noexcept -> decltype((__VA_ARGS__)) { \
	static_assert(noexcept((__VA_ARGS__)), "expression is not noexcept"); \
	return (__VA_ARGS__); \
}
// We cannot have return_decltype_xvalue_by_ref_NOEXCEPT since xvalues cannot be passed through NOEXCEPT.

	template<typename T>
	using xvalue_decay_t = std::conditional_t<std::is_rvalue_reference<T>::value,
		tc::decay_t<T>,
		T
	>;

#define return_decltype_xvalue_by_val_MAYTHROW(...) noexcept(noexcept((__VA_ARGS__))) -> tc::xvalue_decay_t<decltype((__VA_ARGS__))> { \
	return (__VA_ARGS__); \
}

#define return_decltype_xvalue_by_val_noexcept(...) noexcept -> tc::xvalue_decay_t<decltype((__VA_ARGS__))> { \
	static_assert(noexcept((__VA_ARGS__)), "expression is not noexcept"); \
	return (__VA_ARGS__); \
}
// We cannot have return_decltype_xvalue_by_val_NOEXCEPT since xvalues cannot be passed through NOEXCEPT.

	// helper function to decay rvalues when decltype cannot be used because of lambdas in the expression
	// note that contrary to return_decltype_xvalue_by_val, this helper function also decays prvalues
	template<typename T>
	xvalue_decay_t<T&&> lvalue_or_decay(T&& t) noexcept {
		return std::forward<T>(t);
	}
}

#define return_ctor_MAYTHROW(T, ...) noexcept(noexcept(T __VA_ARGS__)) -> T { \
	return T __VA_ARGS__ ; \
}
#define return_ctor_noexcept(T, ...) noexcept -> T { \
	static_assert((noexcept(T __VA_ARGS__)), "expression is not noexcept"); \
	return T __VA_ARGS__ ; \
}
#define return_ctor_NOEXCEPT(T, ...) noexcept -> T { \
	return NOEXCEPT_NO_LAMBDA(T __VA_ARGS__) ; \
}
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

