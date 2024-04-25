
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "temporary.h"

namespace tc {
	#define return_MAYTHROW(...) noexcept(noexcept((__VA_ARGS__))) { \
		return (__VA_ARGS__); \
	}

	template<typename T>
	using xvalue_decay_t = typename std::conditional_t<std::is_rvalue_reference<T>::value || tc::instance_tn<T, tc::temporary>,
		tc::decay<T>,
		std::type_identity<T>
	>::type;

	// helper function to decay rvalues when decltype cannot be used because of lambdas in the expression
	template<typename T>
	constexpr xvalue_decay_t<T&&> lvalue_or_decay(T&& t) noexcept {
		return tc_move_if_owned(t);
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// return_decltype: does not allow returning xvalues, safely handles tc::temporary
	namespace no_adl {
		template<typename T>
		struct return_decltype final {
			static_assert(!std::is_rvalue_reference<T>::value, "use return_decltype_allow_xvalue if you really want to return xvalue references, but be prepared to handle tc::temporary arguments");
			using type = tc::return_temporary_t<T>;
		};
	}
	template<typename T>
	using return_decltype_t = typename no_adl::return_decltype<T>::type;

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

	#define code_return_decltype(code, ...) -> tc::return_decltype_t<decltype((__VA_ARGS__))> { \
		code \
		return (__VA_ARGS__); \
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// return_decltype_allow_xvalue: allows returning xvalues but does not allow dangling temporaries
	#define return_decltype_allow_xvalue_MAYTHROW(...) noexcept(noexcept((__VA_ARGS__))) -> decltype((__VA_ARGS__)) { \
		static_assert(!tc::dangling_temporary<decltype((__VA_ARGS__))>, "you need to use return_decltype_allow_xvalue_slow_MAYTHROW"); \
		return (__VA_ARGS__); \
	}

	#define return_decltype_allow_xvalue_noexcept(...) noexcept -> decltype((__VA_ARGS__)) { \
		static_assert(!tc::dangling_temporary<decltype((__VA_ARGS__))>, "you need to use return_decltype_allow_xvalue_slow_noexcept"); \
		static_assert(noexcept((__VA_ARGS__)), "expression is not noexcept"); \
		return (__VA_ARGS__); \
	}

	// We cannot have return_decltype_allow_xvalue_NOEXCEPT since xvalues cannot be passed through NOEXCEPT.

	#define code_return_decltype_allow_xvalue(code, ...) -> decltype((__VA_ARGS__)) { \
		static_assert(!tc::dangling_temporary<decltype((__VA_ARGS__))>, "you need to use code_return_decltype_allow_xvalue_slow"); \
		code \
		return (__VA_ARGS__); \
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// return_decltype_allow_xvalue_slow: allows returning xvalues and safely handles dangling temporaries.
	// Only use it, when the static_assert tells you to.
	// (This cannot be folded into return_decltype_allow_xvalue due to a drastic compile-time increase.)
	#define return_decltype_allow_xvalue_slow_MAYTHROW(...) noexcept(noexcept((__VA_ARGS__))) -> tc::return_temporary_t<decltype((__VA_ARGS__))> { \
		return (__VA_ARGS__); \
	}

	#define return_decltype_allow_xvalue_slow_noexcept(...) noexcept -> tc::return_temporary_t<decltype((__VA_ARGS__))> { \
		static_assert(noexcept((__VA_ARGS__)), "expression is not noexcept"); \
		return (__VA_ARGS__); \
	}

	// We cannot have return_decltype_allow_xvalue_slow_NOEXCEPT since xvalues cannot be passed through NOEXCEPT.

	#define code_return_decltype_allow_xvalue_slow(code, ...) -> tc::return_temporary_t<decltype((__VA_ARGS__))> { \
		code \
		return (__VA_ARGS__); \
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// return_ctor: always returns T as prvalue
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

	//-------------------------------------------------------------------------------------------------------------------------
	// tc_auto_cref
	namespace no_adl {
		template <typename T>
		struct auto_cref_impl final {
			STATICASSERTSAME(std::remove_cv_t<T>, tc::decay_t<T>);
			using type = std::remove_cv_t<T> const;
		};

		template <typename T>
		struct auto_cref_impl<T&&> final {
			STATICASSERTSAME(std::remove_cv_t<T>, tc::decay_t<T>);
			using type = std::remove_cv_t<T> const;
		};

		template <typename T, unsigned Lifetime>
		struct auto_cref_impl<tc::temporary<T, Lifetime>> final {
			STATICASSERTSAME(std::remove_cv_t<T>, tc::decay_t<T>);
			using type = std::conditional_t<0 == Lifetime, std::remove_cv_t<T> const, tc::temporary<T const, Lifetime> const>; // if 0 < Lifetime, the underlying temporary object lives longer than the current function
		};

		template <typename T>
		struct auto_cref_impl<T&> final {
			using type = T const&;
		};
	}
	template <typename T>
	using auto_cref_t = typename no_adl::auto_cref_impl<tc::canonicalize_temporary_t<T>>::type;
	template <typename T>
	using auto_cref_return_t = std::remove_const_t<auto_cref_t<T>>;

	#define tc_auto_cref( var, ... ) \
		tc::auto_cref_t<decltype((__VA_ARGS__))> var = __VA_ARGS__
	#define tc_auto_cref_return( var, ... ) \
		tc::auto_cref_return_t<decltype((__VA_ARGS__))> var = __VA_ARGS__
}
