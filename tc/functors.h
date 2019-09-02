
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"

////////////////////////////////
// functor equivalents for operators, free functions and member functions

#include "tc_move.h"
#include "return_decltype.h"
#include "tag_type.h"

#include <boost/type_traits/has_dereference.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/control/if.hpp>

#include <cmath>
#include <cstdlib>
#include <algorithm>

// DEFINE_FN(func) always defines a function void func(define_fn_dummy)
// If that function did not exist, -> decltype( func(...) ) would not be
// a valid statement and clang complains about that.
struct define_fn_dummy final {};

#define DEFINE_FN2( func, name ) \
	struct name { \
		template< typename A0, typename ...Args >     /*require at least one argument*/ \
		auto operator()( A0&& a0, Args&& ... args) const \
			return_decltype_xvalue_by_ref( func(std::forward<A0>(a0), std::forward<Args>(args)...) ) \
	};

#define DEFINE_MEM_FN_BODY_( ... ) \
	template< typename O, typename ...__A > \
	auto operator()( O&& o, __A&& ... __a ) const \
		return_decltype_xvalue_by_ref( std::forward<O>(o) __VA_ARGS__ ( std::forward<__A>(__a)... ) )

// std::mem_fn (C++11 standard 20.8.2) knows the type it can apply its member function pointer to and
// dereferences via operator* until it reaches that something of that type. We cannot do that because
// we have no type. Merely checking for the presence of the member function name is dangerous because
// changes in otherwise unrelated types (the iterator and its pointee) would influence the lookup. 
// Instead we distinguish between shallow member access, implemented in dot_mem_fn, and deep 
// member access via operator->, with the small addition that if operator-> does not exist, we use the
// the dot operator, in the spirit of dereferencing as much as possible.

#define DEFINE_MEM_FN_AUTO_BODY_( ... ) \
	template< typename O, typename ...__A, std::enable_if_t<boost::has_dereference<O>::value>* = nullptr > \
	auto operator()( O&& o, __A&& ... __a ) const \
		return_decltype_xvalue_by_ref( std::forward<O>(o)-> __VA_ARGS__ ( std::forward<__A>(__a)... ) ) \
	template< typename O, typename ...__A, std::enable_if_t<!boost::has_dereference<O>::value>* = nullptr > \
	auto operator()( O&& o, __A&& ... __a ) const \
		return_decltype_xvalue_by_ref( std::forward<O>(o). __VA_ARGS__ ( std::forward<__A>(__a)... ) )

// When a functor must be declared in class-scope, e.g., to access a protected member function,
// you should use DEFINE_MEM_FN instead of DEFINE_FN. 
// DEFINE_FN always has to define a function named "void func(define_fn_dummy)" which may
// shadow inherited members named func.
#define DEFINE_MEM_FN( func ) \
	struct dot_member_ ## func { \
		template< typename O > auto operator()( O & o ) const return_decltype( o.func ) \
		template< typename O > auto operator()( O const& o ) const return_decltype( o.func ) \
		template< typename O, std::enable_if_t<!std::is_reference<O>::value>* = nullptr > \
		auto operator()( O&& o ) const return_decltype_xvalue_by_ref( tc_move(o).func ) \
	}; \
	std::true_type returns_reference_to_argument(dot_member_ ## func); /*mark as returning reference to argument*/ \
	struct dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .func ) \
	}; \
	struct mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( func ) \
	};

#define DEFINE_FN( func ) \
	[[maybe_unused]] static void func(define_fn_dummy) noexcept; \
	DEFINE_FN2( func, fn_ ## func ) \
	DEFINE_MEM_FN( func )

#define MEM_FN_TEMPLATE_DECLARATION(r, data, i, elem) BOOST_PP_IF(i,BOOST_PP_COMMA,BOOST_PP_EMPTY)() elem T##i
#define MEM_FN_TEMPLATE_INVOCATION(r, data, i, elem) BOOST_PP_IF(i,BOOST_PP_COMMA,BOOST_PP_EMPTY)() T##i

#define DEFINE_FN2_TMPL( func, tmpl ) \
    template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
    DEFINE_FN2( func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) >, fn_ ## func )

// See comments above for DEFINE_MEM_FN
#define DEFINE_MEM_FN_TMPL( func, tmpl ) \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct dot_mem_fn_ ## func { \
		DEFINE_MEM_FN_BODY_( .template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	}; \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	struct mem_fn_ ## func { \
		DEFINE_MEM_FN_AUTO_BODY_( template func< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_INVOCATION, _, tmpl ) > ) \
	};

#define DEFINE_FN_TMPL( func, tmpl ) \
	template< BOOST_PP_SEQ_FOR_EACH_I( MEM_FN_TEMPLATE_DECLARATION, _, tmpl ) > \
	[[maybe_unused]] static void func(define_fn_dummy) noexcept; \
	DEFINE_FN2_TMPL( func, tmpl ) \
	DEFINE_MEM_FN_TMPL( func, tmpl )

DEFINE_FN2( std::abs, fn_std_abs );
DEFINE_FN2( operator delete, fn_operator_delete )

DEFINE_FN( first );
DEFINE_FN( second );

template<int N> DEFINE_FN2( std::get<N>, fn_std_get );

// Cannot use DEFINE_FN2_TMPL here, since casts are build in and the compiler
//(both clang and MSVC) does not accept passing a parameter pack to *_cast
#pragma push_macro("DEFINE_CAST_")
#define DEFINE_CAST_(name) \
	template <typename To> \
	struct fn_ ## name { \
		template<typename From> auto operator()(From&& from) const \
			return_decltype( name <To>(std::forward<From>(from))) \
	};

DEFINE_CAST_(static_cast)
DEFINE_CAST_(reinterpret_cast)
DEFINE_CAST_(const_cast)

#pragma pop_macro("DEFINE_CAST_")

// We can use TC_MEM_FN instead of std::mem_fn when the member function has multiple overloads, for example accessors.
// Note: if you want to capture something in the parameter list, you should not use this macro but write your own lambda and make sure it's safe.
#ifndef __clang__ // MSVC 15.8.0 sometimes has problem identifying lambda parameter in noexcept(expression). TODO: This is fixed in VS2019.
#define TC_MEM_FN(...) \
	[](auto&& _) MAYTHROW -> decltype(auto) { \
		return std::forward<decltype(_)>(_)__VA_ARGS__; \
	}
#else
#define TC_MEM_FN(...) \
	[](auto&& _) noexcept(noexcept(std::forward<decltype(_)>(_)__VA_ARGS__)) -> decltype(auto) { \
		return std::forward<decltype(_)>(_)__VA_ARGS__; \
	}
#endif

namespace tc {
	struct fn_subscript final {
		template<typename Lhs, typename Rhs>
		auto operator()( Lhs&& lhs, Rhs&& rhs ) const& noexcept
			return_decltype_xvalue_by_ref( std::forward<Lhs>(lhs)[std::forward<Rhs>(rhs)] )
	};

	/* indirection operator, aka dereference operator */
	struct fn_indirection final {
		template<typename Lhs>
		auto operator()( Lhs&& lhs ) const& noexcept
			return_decltype_xvalue_by_ref( *std::forward<Lhs>(lhs))
	};
	std::true_type returns_reference_to_argument(fn_indirection) noexcept; // mark as returning reference to argument

	struct fn_logical_not final {
		template<typename Lhs>
		auto operator()( Lhs&& lhs ) const
			return_decltype_xvalue_by_ref( !std::forward<Lhs>(lhs))
	};

	namespace no_adl {
		template<typename F1, typename... Fs>
		struct overload : tc::decay_t<F1>, overload<Fs...>
		{
			using tc::decay_t<F1>::operator();
			using overload<Fs...>::operator();

			constexpr overload(F1&& f1, Fs&& ... fs) noexcept : tc::decay_t<F1>(std::forward<F1>(f1)), overload<Fs...>(std::forward<Fs>(fs)...) {}
		};

		template<typename F1>
		struct overload<F1> : tc::decay_t<F1>
		{
			using tc::decay_t<F1>::operator();

			constexpr overload(F1&& f1) noexcept : tc::decay_t<F1>(std::forward<F1>(f1)) {}
		};

		template<typename Result, typename... Fs>
		struct result_wrapper {
		private:
			overload<Fs...> m_overload;
		public:
			constexpr result_wrapper(Fs&&... fs) noexcept: m_overload(std::forward<Fs>(fs)...) {}

			template<typename... T, std::enable_if_t<std::is_same<Result, void>::value || tc::is_safely_convertible<decltype(m_overload(std::declval<T>()...)), Result>::value>* = nullptr>
			constexpr Result operator()(T&&... t) const& MAYTHROW {
				if constexpr (std::is_same<Result, void>::value) {
					m_overload(std::forward<T>(t)...);
				} else {
					return m_overload(std::forward<T>(t)...);
				}
			}
		};
	}

	// C++17 deduction guide needed for class template argument deduction as replacement for make_overload. Note: ctor overload(F1&& f1) is not a universal reference by default.
	// template<typename Func>
	// overload(Func&&) noexcept -> overload<Func>;

	DEFINE_TAG_TYPE(use_default_result)

	template <typename Result, typename... F>
	constexpr auto make_overload(F&& ... f) noexcept {
		if constexpr (std::is_same<Result, tc::use_default_result_t>::value) {
			return no_adl::overload<F...>(std::forward<F>(f)...);
		} else {
			return no_adl::result_wrapper<Result, F...>(std::forward<F>(f)...);
		}
	}
}

#pragma push_macro("INFIX_FN_")
#define INFIX_FN_( name, op ) \
	struct fn_ ## name { \
		template<typename Lhs, typename Rhs> \
		auto operator()( Lhs&& lhs, Rhs&& rhs ) const \
			return_decltype_xvalue_by_ref(std::forward<Lhs>(lhs) op std::forward<Rhs>(rhs)) \
	};

INFIX_FN_( bit_or, | )
INFIX_FN_( bit_and, & )
INFIX_FN_( assign_bit_or, |= )
INFIX_FN_( assign_bit_and, &= )
INFIX_FN_( plus, + )
INFIX_FN_( minus, - )
INFIX_FN_( mul, * )
INFIX_FN_( assign_plus, += )
INFIX_FN_( assign_minus, -= )
INFIX_FN_( assign_mul, *= )
INFIX_FN_( assign, = )

#pragma pop_macro("INFIX_FN_")

#define ALLOW_NOEXCEPT( ... ) \
	decltype(static_cast<__VA_ARGS__>(nullptr))

namespace tc {
	namespace no_adl {
		template<typename Func>
		struct expand_tuple {
		private:
			tc::decay_t<Func> m_func;
		public:
			explicit expand_tuple(Func&& func) noexcept
				: m_func(std::forward<Func>(func))
			{}

			template<typename Tuple>
			auto operator()(Tuple&& tuple) const& MAYTHROW {
				return std::apply(m_func, std::forward<Tuple>(tuple));
			}
		};

		template<typename Func>
		expand_tuple(Func&& func) -> expand_tuple<Func>;
	}

	using no_adl::expand_tuple;
}
