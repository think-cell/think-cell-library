
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "type_traits.h"
#include "noncopyable.h"
#include "../container/insert.h"
#include "../range/concat_adaptor.h"
#include "../algorithm/algorithm.h"
#include "../unittest.h"


STATICASSERTSAME(tc::remove_rvalue_reference_t<int>, int);
STATICASSERTSAME(tc::remove_rvalue_reference_t<int const>, int const);
STATICASSERTSAME(tc::remove_rvalue_reference_t<int&>, int&);
STATICASSERTSAME(tc::remove_rvalue_reference_t<int const&>, int const&);
STATICASSERTSAME(tc::remove_rvalue_reference_t<int&&>, int);
STATICASSERTSAME(tc::remove_rvalue_reference_t<int const&&>, int const);
STATICASSERTSAME(tc::remove_rvalue_reference_t<tc::vector<int>>, tc::vector<int>);
STATICASSERTSAME(tc::remove_rvalue_reference_t<tc::vector<int> const>, tc::vector<int> const);
STATICASSERTSAME(tc::remove_rvalue_reference_t<tc::vector<int>&>, tc::vector<int>&);
STATICASSERTSAME(tc::remove_rvalue_reference_t<tc::vector<int> const&>, tc::vector<int> const&);
STATICASSERTSAME(tc::remove_rvalue_reference_t<tc::vector<int>&&>, tc::vector<int>);
STATICASSERTSAME(tc::remove_rvalue_reference_t<tc::vector<int> const&&>, tc::vector<int> const);

namespace void_t_test {
	template< typename T, typename=void >
	struct Foo {
		static constexpr int const value = 0;
	};
	template< typename T >
	struct Foo<T, tc::void_t<typename T::type1>> {
		static constexpr int const value = 1;
	};
	template< typename T >
	struct Foo<T, tc::void_t<typename T::type2>> {
		static constexpr int const value = 2;
	};

	struct Bar0 { };
	struct Bar1 {
		using type1 = int;
	};
	struct Bar2 {
		using type2 = int;
	};

	STATICASSERTEQUAL(0, Foo<Bar0>::value);
	STATICASSERTEQUAL(1, Foo<Bar1>::value);
	STATICASSERTEQUAL(2, Foo<Bar2>::value);

	struct WithFunction {
		void func();
	};
	struct WithoutFunction { };

	TC_HAS_EXPR(func, (T), std::declval<T&>().func());

	static_assert(has_func<WithFunction>);
	static_assert(!has_func<WithoutFunction>);

	tc::constant<false> check_has_func1(...);
	template< typename T > requires has_func<T>
	tc::constant<true> check_has_func1(T&& t);

	static_assert(decltype(check_has_func1(std::declval<WithFunction>()))::value);
	static_assert(!decltype(check_has_func1(std::declval<WithoutFunction>()))::value);

	template< typename T>
	tc::constant<false> check_has_func2(T&&);
	template< typename T> requires has_func<T>
	tc::constant<true> check_has_func2(T&& t);

	static_assert(decltype(check_has_func2(std::declval<WithFunction>()))::value);
	static_assert(!decltype(check_has_func2(std::declval<WithoutFunction>()))::value);
}

static_assert( tc::safely_convertible_to<int, double> );

static_assert( std::convertible_to<int, float> );
static_assert( sizeof(int)!=sizeof(float) || !tc::safely_convertible_to<int, float> );

static_assert( std::convertible_to<long long, double> );
static_assert( sizeof(long long)!=sizeof(double) || !tc::safely_convertible_to<long long, double> );

static_assert( std::convertible_to<double, int> );
static_assert( !tc::safely_convertible_to<double, int> );

static_assert( std::convertible_to<float, int> );
static_assert( !tc::safely_convertible_to<float, int> );

static_assert( std::convertible_to<int, unsigned int> );
static_assert( !tc::safely_convertible_to<int, unsigned int> );

static_assert( std::convertible_to<unsigned int, int> );
static_assert( !tc::safely_convertible_to<unsigned int, int> );

static_assert(std::convertible_to<int*, bool>);
static_assert(std::is_constructible<bool, int*>::value);
static_assert(!std::convertible_to<std::nullptr_t, bool>);
static_assert(std::is_constructible<bool, std::nullptr_t>::value);
static_assert(tc::safely_constructible_from<bool, int*>);
static_assert(tc::safely_constructible_from<bool, std::nullptr_t>);
static_assert(!tc::safely_convertible_to<int*, bool>);
static_assert(!tc::safely_convertible_to<std::nullptr_t, bool>);
static_assert(tc::safely_constructible_from<bool, bool>);
static_assert(tc::safely_convertible_to<bool, bool>);

// scoped enum (enum class)
enum class TEnumClass { a, b, c };
enum TEnum { x, y, z };
static_assert( !tc::safely_convertible_to<int, TEnumClass> );
static_assert( !tc::safely_convertible_to<TEnumClass, int> );
static_assert( !tc::has_common_reference_prvalue_as_val<TEnumClass, int>);

// unscoped enum (primitive enum)
enum TPrimitiveEnum { a, b, c };
static_assert( !tc::safely_convertible_to<int, TPrimitiveEnum> );
static_assert( std::convertible_to<TPrimitiveEnum, std::underlying_type_t<TPrimitiveEnum> > );
static_assert( !tc::safely_convertible_to<TPrimitiveEnum, std::underlying_type_t<TPrimitiveEnum>> );
static_assert( !tc::has_common_reference_prvalue_as_val<TPrimitiveEnum, int>);

enum TPrimitiveEnum2 { l, m };
static_assert( !tc::safely_convertible_to<TPrimitiveEnum, TPrimitiveEnum2> );
static_assert( !tc::has_common_reference_prvalue_as_val<TPrimitiveEnum, TPrimitiveEnum2>);

struct SBase {};
struct SDerived final : SBase {};
static_assert(std::convertible_to<SDerived, SBase>);
static_assert(!tc::safely_convertible_to<SDerived, SBase>);

static_assert(tc::safely_convertible_to<SDerived&, SDerived>);
static_assert(!tc::safely_convertible_to<SDerived&, SBase>);
static_assert(tc::safely_convertible_to<SDerived&, SBase&>);
static_assert(tc::safely_convertible_to<SDerived&, SBase const&>);
static_assert(!tc::safely_convertible_to<SDerived&, SBase&&>);
static_assert(tc::safely_convertible_to<SDerived&, SBase const&&>);

static_assert(tc::safely_convertible_to<SDerived const&, SDerived>);
static_assert(!tc::safely_convertible_to<SDerived const&, SBase>);
static_assert(!tc::safely_convertible_to<SDerived const&, SBase&>);
static_assert(tc::safely_convertible_to<SDerived const&, SBase const&>);
static_assert(!tc::safely_convertible_to<SDerived const&, SBase&&>);
static_assert(tc::safely_convertible_to<SDerived const&, SBase const&&>);

static_assert(tc::safely_convertible_to<SDerived, SDerived>);
static_assert(!tc::safely_convertible_to<SDerived, SBase>);
static_assert(!tc::safely_convertible_to<SDerived, SBase const&>);
static_assert(!tc::safely_convertible_to<SDerived, SBase&>);
static_assert(!tc::safely_convertible_to<SDerived, SBase&&>);
static_assert(!tc::safely_convertible_to<SDerived, SBase const&&>);

static_assert(tc::safely_convertible_to<SDerived&&, SDerived>);
static_assert(!tc::safely_convertible_to<SDerived&&, SBase>);
static_assert(!tc::safely_convertible_to<SDerived&&, SBase const&>);
static_assert(!tc::safely_convertible_to<SDerived&&, SBase&>);
static_assert(tc::safely_convertible_to<SDerived&&, SBase&&>);
static_assert(tc::safely_convertible_to<SDerived&&, SBase const&&>);

static_assert(tc::safely_convertible_to<SDerived const&&, SDerived>);
static_assert(!tc::safely_convertible_to<SDerived const&&, SBase>);
static_assert(!tc::safely_convertible_to<SDerived const&&, SBase const&>);
static_assert(!tc::safely_convertible_to<SDerived const&&, SBase&>);
static_assert(!tc::safely_convertible_to<SDerived const&&, SBase&&>);
static_assert(tc::safely_convertible_to<SDerived const&&, SBase const&&>);

struct SToInt final {
	operator int() const& noexcept;
};

static_assert(!std::convertible_to<SBase, int>);
static_assert(std::convertible_to<SToInt, int>);
static_assert(tc::safely_convertible_to<SToInt, int>);

static_assert(std::convertible_to<SToInt, int const&>);
static_assert(!tc::safely_convertible_to<SToInt, int const&>);

static_assert(!tc::safely_convertible_to<SToInt, int&&>);
static_assert(!tc::safely_convertible_to<SToInt, int const&&>);
static_assert(!tc::safely_convertible_to<SToInt&, int&&>);
static_assert(!tc::safely_convertible_to<SToInt&, int const&&>);
static_assert(!tc::safely_convertible_to<SToInt&&, int&&>);
static_assert(!tc::safely_convertible_to<SToInt&&, int const&&>);

static_assert(tc::safely_convertible_to<tc::string<char>&, tc::span<char>>);
static_assert(tc::safely_convertible_to<tc::string<char>&, tc::span<char const>>);
static_assert(!tc::safely_convertible_to<tc::string<char>, tc::span<char const>>);
static_assert(!tc::safely_convertible_to<tc::string<char> const, tc::span<char const>>);
static_assert(!tc::safely_convertible_to<tc::string<char>&&, tc::span<char const>>);
static_assert(!tc::safely_convertible_to<tc::string<char> const&&, tc::span<char const>>);
static_assert(tc::safely_convertible_to<tc::string<char> const&, tc::span<char const>>);
static_assert(!tc::safely_convertible_to<char const*, tc::span<char>>);
static_assert(tc::safely_convertible_to<char const*, tc::span<char const>>);
static_assert(tc::safely_convertible_to<char const* &, tc::span<char const>>);
static_assert(tc::safely_convertible_to<char const* &&, tc::span<char const>>);
static_assert(tc::safely_convertible_to<int(&)[3], tc::span<int const>>);
static_assert(tc::safely_convertible_to<int(&)[3], tc::span<int>>);
static_assert(tc::safely_convertible_to<tc::span<int>, tc::span<int>>);
static_assert(tc::safely_convertible_to<tc::span<int>, tc::span<int const>>);
static_assert(tc::safely_convertible_to<tc::span<int>&&, tc::span<int const>>);
static_assert(tc::safely_convertible_to<tc::span<int>&, tc::span<int const>>);
static_assert(tc::safely_convertible_to<tc::span<int> const&, tc::span<int const>>);
static_assert(!tc::safely_convertible_to<tc::span<char const>, decltype(tc::concat("abc", "def"))>);
static_assert(!tc::safely_convertible_to<tc::span<char const>, tc::vector<int>>);

static_assert(!tc::safely_convertible_to<tc::string<char>&, tc::span<char>&>);
static_assert(!tc::safely_convertible_to<tc::string<char>&, tc::span<char> const&>);

#ifdef TC_MAC
static_assert(tc::safely_constructible_from<void (^)(), void (^)()>);
static_assert(!tc::safely_constructible_from<void (^)(), int (^)()>);
static_assert(!tc::safely_constructible_from<void (^)(), void (^)(int)>);
static_assert(tc::safely_constructible_from<void (^)(), nullptr_t>);
static_assert(!tc::safely_constructible_from<void (^)(), int*>);
static_assert(!tc::safely_constructible_from<void (^)(), void*>);

namespace {
	using TVoidFunction = void (*)();
}
static_assert(!tc::safely_constructible_from<void (^)(), TVoidFunction>);

static_assert(tc::no_adl::is_objc_block<void (^)()>::value);
static_assert(tc::no_adl::is_objc_block<void (^)(int, char, bool)>::value);
static_assert(!tc::no_adl::is_objc_block<void (*)(int, char, bool)>::value);
#endif

struct A {};
struct B : A {
	operator int(){return 0;}
};

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<A&&, A&&>,
		A&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<A&, A&&>,
		A const&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<A&, B&>,
		A&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<A&&, B&&>,
		A&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<A&, B&&>,
		A const&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<A&, A&, A const&>,
		A const&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<A&&, A&&, B&&>,
		A&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<char const>&, tc::span<char const> const&>,
		tc::span<char const> const&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<char>, tc::span<char const> const&>,
		tc::span<char const>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<char const>, tc::span<char const> const&>,
		tc::span<char const>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<char const>, tc::string<char>&>,
		tc::span<char const>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<char>, tc::string<char> const&>,
		tc::span<char const>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<char>, tc::span<char>>,
		tc::span<char>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<char>, tc::span<char const>>,
		tc::span<char const>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::vector<char>&, tc::vector<char> const&>,
		tc::vector<char> const&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<int(&)[17], int(&)[17]>,
		int(&)[17]
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<int const(&)[17], int(&)[17]>,
		int const(&)[17]
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<int(&)[17], int(&)[18]>,
		tc::span<int>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<int const(&)[17], int(&)[18]>,
		tc::span<int const>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<int>, int(&)[19]>,
		tc::span<int>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<int> const, int(&)[19]>,
		tc::span<int>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<int(&)[17], int(&)[18], int(&)[19]>,
		tc::span<int>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<int&, short&>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::vector<char>&, tc::string<char>&>,
		std::conditional_t<
			std::same_as<tc::iterator_t<tc::vector<char>>, tc::iterator_t<tc::string<char>>>,
			tc::iterator_range<tc::iterator_t<tc::vector<char>>>,
			tc::span<char>
		>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::vector<char>&, tc::string<char> const&>,
		std::conditional_t<
			std::same_as<tc::iterator_t<tc::vector<char>>, tc::iterator_t<tc::string<char>>>,
			tc::iterator_range<tc::iterator_t<tc::vector<char> const>>,
			tc::span<char const>
		>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<const wchar_t (&)[6], tc::span<wchar_t>&&>,
		tc::span<wchar_t const>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::vector<char>&&, tc::vector<char>&>,
		tc::vector<char> const&&
	>::value
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<tc::span<char>, tc::string<char>&&>
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<tc::vector<char>&, tc::string<char>&&>
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<char const*, tc::span<char>, tc::string<char> const&&>
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<tc::vector<char>&&, tc::string<char>&>
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<A, A&&>
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<tc::span<char const>, tc::string<char>>
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<A, A>
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A&, A>,
		A
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A&&, A>,
		A
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A, A>,
		A
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A&&, A&&>,
		A&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A&, A&>,
		A&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A&, A&&>,
		A const&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A const&&, A&&>,
		A const&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<A const&, A&>,
		A const&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<B const&, A volatile&>,
		A const volatile&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<B&, A volatile&&>,
		A const volatile&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<B const&&, A volatile&>,
		A const volatile&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<B const&&, B volatile&>,
		B const volatile &&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<tc::vector<char>&, tc::vector<char> const&>,
		tc::vector<char> const&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<int(&)[17], int(&)[17]>,
		int(&)[17]
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<int const(&)[17], int(&)[17]>,
		int const(&)[17]
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<int&, short&>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<tc::vector<char>&&, tc::vector<char>&>,
		tc::vector<char> const&&
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<tc::span<char const>>,
		tc::span<char const>
	>::value
);

static_assert(
	!tc::has_common_reference_prvalue_as_val<A, B>
);

static_assert(
	!tc::has_common_reference_prvalue_as_val<tc::vector<char>&&, tc::string<char>&>
);

static_assert(
	!tc::has_common_reference_prvalue_as_val<tc::vector<char>&&, tc::string<char>&, tc::span<char const>>
);

static_assert(
	!tc::has_common_reference_prvalue_as_val<tc::vector<char>, tc::string<char>&>
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::static_vector<int, 3>&, tc::subrange<tc::static_vector<int, 3>&>>,
		tc::subrange<tc::static_vector<int, 3>&>
	>::value
);

static_assert(
	std::is_same <
		tc::common_reference_xvalue_as_ref_t<tc::static_vector<int, 3>&, tc::subrange<tc::static_vector<int, 3>>&>,
		tc::subrange<tc::static_vector<int, 3>&>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::subrange<tc::static_vector<int, 3>&>, tc::subrange<tc::static_vector<int, 3>>&>,
		tc::subrange<tc::static_vector<int, 3>&>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<int>, tc::subrange<tc::static_vector<int, 3>&>>,
		tc::span<int>
	>::value
);

STATICASSERTSAME(
	tc::span_t<tc::subrange<tc::vector<int>>&>,
	tc::span<int>
);

static_assert(
	std::is_same<
		tc::common_reference_xvalue_as_ref_t<tc::span<int>, tc::subrange<tc::vector<int>>&>,
		tc::span<int>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<tc::static_vector<int, 3>&, tc::subrange<tc::static_vector<int, 3>&>>,
		tc::subrange<tc::static_vector<int, 3>&>
	>::value
);

static_assert(
	std::is_same <
		tc::common_reference_prvalue_as_val_t<tc::static_vector<int, 3>&, tc::subrange<tc::static_vector<int, 3>>&>,
		tc::subrange<tc::static_vector<int, 3>&>
	>::value
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<tc::subrange<tc::static_vector<int, 3>&>, tc::subrange<tc::static_vector<int, 3>>&>,
		tc::subrange<tc::static_vector<int, 3>&>
	>::value
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<tc::static_vector<int, 3>&, tc::subrange<tc::static_vector<int, 3>>>
);

static_assert(
	!tc::has_common_reference_xvalue_as_ref<tc::static_vector<int, 3>, tc::subrange<tc::static_vector<int, 3>&>>
);

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<tc::vector<char>&, tc::string<char>&>,
		std::conditional_t<
			std::same_as<tc::iterator_t<tc::vector<char>>, tc::iterator_t<tc::string<char>>>,
			tc::iterator_range<tc::iterator_t<tc::vector<char>>>,
			tc::span<char>
		>
	>::value
);

namespace
{
	struct my_vector
	{
		tc::vector<int> impl;

		auto begin() & { return tc::begin(impl); }
		auto end() & { return tc::begin(impl); }
	};
}

static_assert(
	std::is_same<
		tc::common_reference_prvalue_as_val_t<tc::vector<int>&, my_vector&>,
		tc::iterator_range<tc::iterator_t<tc::vector<int>>>
	>::value
);

static_assert(
	!tc::has_common_reference_prvalue_as_val<tc::vector<int>&, my_vector>
);

namespace {
struct S;
tc::unordered_set<S const*> g_sets;

struct S{
	S() {
		tc::cont_must_emplace(g_sets, this);
	}

	S(S const& other) {
		tc::cont_must_emplace(g_sets, this);
	}

	S(S&& other) {
		tc::cont_must_emplace(g_sets, this);
	}

	~S() {
		tc::cont_must_erase(g_sets, this);
	}

	void foo() & {
		_ASSERT(tc::end(g_sets) != g_sets.find(this));
	}

	void foo() && {
		_ASSERT(tc::end(g_sets) != g_sets.find(this));
	}

	void foo() const& {
		_ASSERT(tc::end(g_sets) != g_sets.find(this));
	}

	void foo() const&& {
		_ASSERT(tc::end(g_sets) != g_sets.find(this));
	}

	friend bool operator<(S const& lhs, S const& rhs) {
		_ASSERT(tc::end(g_sets) != g_sets.find(std::addressof(lhs)));
		_ASSERT(tc::end(g_sets) != g_sets.find(std::addressof(rhs)));
		return true;
	}
};

S createS(int) {
	return S{};
}
}

#include "../algorithm/minmax.h"

static_assert(
	std::is_same<
		tc::common_type_t<int, short>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::common_type_t<tc::size_proxy<int>, short>,
		short
	>::value
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>&>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>&&>,
		int
	>::value
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>, tc::size_proxy<long>>,
		int
	>::value
);

/*
// must not compile
static_assert(
	std::is_same<
		tc::common_type_t<int, unsigned int>,
		unsigned int
	>::value
);
*/

/*
// must not compile
static_assert(
	std::is_same<
		tc::common_type_t<char, char16_t>,
		int
	>::value
);
*/

/*
	must not compile (slicing)
static_assert(
	std::is_same<
		tc::common_type_t<B,A>,
		A
	>::value
);
*/

static_assert(
	std::is_same<
		decltype(tc::min(std::declval<tc::size_proxy<long>>(), std::declval<short>())),
		short
	>::value
);

UNITTESTDEF(minTest) {
	tc::vector<int> vecn;

	static_assert(
		std::is_same<
			tc::common_reference_prvalue_as_val_t<decltype(tc::size(vecn)),short>,
			short
		>::value
	);

	void(tc::implicit_cast<tc::common_reference_prvalue_as_val_t<decltype(tc::size(vecn)),int>>(tc::size(vecn)));

	void(tc::min(tc::size(vecn),2));

	int a = 3;
	int b = 4;
	_ASSERTEQUAL(tc::min(a,b), 3);
	tc::min(a,b) = 7;
	_ASSERTEQUAL(a, 7);

	_ASSERTEQUAL(tc::min(a,2), 2);
	_ASSERTEQUAL(tc::min(5,b), 4);

	_ASSERTEQUAL(tc::min(1,a,b), 1);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<std::int16_t>(), std::declval<std::int32_t>()))
			, std::int32_t
		>::value
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<std::uint16_t>(), std::declval<std::int32_t>()))
			, std::int32_t
		>::value
	);

	{
		S s2[2];

		tc::for_each(
			tc::make_range_of_iterators(
				tc::transform(
					tc::transform(
						tc::iota(0,1),
						[&](int const n) noexcept -> S&& {
							return std::move(s2[n]);
						}
					),
					tc::fn_min()
				)
			),
			[&](auto it) noexcept {
				tc_auto_cref( elem, *it );
				tc::discard(elem);
			}
		);
	}

	{
		S s;
		tc::min(s,S{}).foo();
	}


	tc::projected(tc::fn_min(), tc_fn(createS))(0,1).foo();


	{
		S s2[2];
		tc::projected(
			tc::fn_min(),
			[&](int const n) noexcept -> S&& {
				return std::move(s2[n]);
			}
		)(0,1).foo();
	}

	{
		tc::projected(
			tc::fn_min(),
			tc::fn_static_cast<S&&>()
		)(createS(1), createS(2));
	}

	_ASSERT(tc::empty(g_sets));

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<int>(),std::declval<long>())),
			tc::common_type_t<int,long>
		>::value
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<long>(),std::declval<int>())),
			tc::common_type_t<long,int>
		>::value
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<unsigned long>(),std::declval<unsigned int>())),
			unsigned long
		>::value
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<unsigned int>(),std::declval<unsigned long>())),
			unsigned long
		>::value
	);
}

static_assert(!tc::safely_constructible_from<tc::string<wchar_t>, wchar_t const* const&, wchar_t const* const&>);

namespace is_instance_test {
	template<typename, typename, typename> struct CTemplate1 : tc::nonmovable {};
	template<typename, typename, typename> struct CTemplate2 : tc::nonmovable {};

	using CInstantiation1 = CTemplate1<int, bool, void>;
	using CInstantiation2 = CTemplate2<bool, void, int>;

	static_assert(tc::instance<CInstantiation1, CTemplate1>);
	STATICASSERTSAME((tc::type::list<int, bool, void>), (typename tc::is_instance<CInstantiation1, CTemplate1>::arguments));

	static_assert(!tc::instance<CInstantiation2, CTemplate1>);

	static_assert(!tc::instance<CInstantiation1, CTemplate2>);

	static_assert(tc::instance<CInstantiation2, CTemplate2>);
	STATICASSERTSAME((tc::type::list<bool, void, int>), (typename tc::is_instance<CInstantiation2, CTemplate2>::arguments));
}

namespace is_instance2_test {
	template<typename, typename, bool> struct CTemplate1 : tc::nonmovable {};
	template<typename, typename, bool> struct CTemplate2 : tc::nonmovable {};

	using CInstantiation1 = CTemplate1<int, bool, true>;
	using CInstantiation2 = CTemplate2<bool, void, false>;

	static_assert(tc::instance2<CInstantiation1, CTemplate1>);
	STATICASSERTSAME(int, (typename tc::is_instance2<CInstantiation1, CTemplate1>::first_argument));
	STATICASSERTSAME(bool, (typename tc::is_instance2<CInstantiation1, CTemplate1>::second_argument));
	STATICASSERTEQUAL(true, (tc::is_instance2<CInstantiation1, CTemplate1>::third_argument));

	static_assert(!tc::instance2<CInstantiation2, CTemplate1>);

	static_assert(!tc::instance2<CInstantiation1, CTemplate2>);

	static_assert(tc::instance2<CInstantiation2, CTemplate2>);
	STATICASSERTSAME(bool, (typename tc::is_instance2<CInstantiation2, CTemplate2>::first_argument));
	STATICASSERTSAME(void, (typename tc::is_instance2<CInstantiation2, CTemplate2>::second_argument));
	STATICASSERTEQUAL(false, (tc::is_instance2<CInstantiation2, CTemplate2>::third_argument));
}

namespace is_instance_or_derived_test {
	template<typename, typename> struct CTemplate1 : tc::nonmovable {};
	template<typename T> struct CTemplate1Int : CTemplate1<int, T> {};
	template<typename, typename> struct CTemplate2 : tc::nonmovable {
		// Catch-all constructor for implicit conversion, should be ignored.
		template<typename T> CTemplate2(T&&);
	};

	struct CInstantiation1 : CTemplate1Int<bool> {
		// Implicit conversions to unrelated type, should be ignored.
		operator CTemplate2<void, void>();
		operator CTemplate2<void, void>&();
		operator CTemplate2<void, void>*();
	};
	using CInstantiation2 = CTemplate2<bool, void>;

	static_assert(tc::instance_or_derived<CInstantiation1, CTemplate1>);
	static_assert(!tc::instance_or_derived<CInstantiation1&, CTemplate1>);
	static_assert(!tc::is_instance_or_derived<CInstantiation1&, CTemplate1>::value);
	STATICASSERTSAME((CTemplate1<int, bool>), (typename tc::is_instance_or_derived<CInstantiation1, CTemplate1>::base_instance));
	STATICASSERTSAME((tc::type::list<int, bool>), (typename tc::is_instance_or_derived<CInstantiation1, CTemplate1>::arguments));

#if defined(_MSC_VER) && !defined(__clang__)
	static_assert(tc::instance_or_derived<CInstantiation1, CTemplate1 const volatile>);
	STATICASSERTSAME((CTemplate1<int, bool>), (typename tc::is_instance_or_derived<CInstantiation1, CTemplate1 const volatile>::base_instance));
	STATICASSERTSAME((tc::type::list<int, bool>), (typename tc::is_instance_or_derived<CInstantiation1, CTemplate1 const volatile>::arguments));
#endif

	static_assert(tc::instance_or_derived<CInstantiation1 const volatile, CTemplate1>);
	STATICASSERTSAME((CTemplate1<int, bool>), (typename tc::is_instance_or_derived<CInstantiation1 const volatile, CTemplate1>::base_instance));
	STATICASSERTSAME((tc::type::list<int, bool>), (typename tc::is_instance_or_derived<CInstantiation1 const volatile, CTemplate1>::arguments));

	static_assert(tc::instance_or_derived<CInstantiation1, CTemplate1Int>);
	STATICASSERTSAME((CTemplate1Int<bool>), (typename tc::is_instance_or_derived<CInstantiation1, CTemplate1Int>::base_instance));
	STATICASSERTSAME((tc::type::list<bool>), (typename tc::is_instance_or_derived<CInstantiation1, CTemplate1Int>::arguments));

	static_assert(!tc::instance_or_derived<CInstantiation2, CTemplate1>);

	static_assert(!tc::instance_or_derived<CInstantiation1, CTemplate2>);

	static_assert(tc::instance_or_derived<CInstantiation2, CTemplate2>);
	STATICASSERTSAME((CTemplate2<bool, void>), (typename tc::is_instance_or_derived<CInstantiation2, CTemplate2>::base_instance));
	STATICASSERTSAME((tc::type::list<bool, void>), (typename tc::is_instance_or_derived<CInstantiation2, CTemplate2>::arguments));

	struct CPrivateInstantiation1 : private CTemplate1<int, int> {
		operator CTemplate1<void, void>();
		operator CTemplate1<void, void>&();
		operator CTemplate1<void, void>*();
	};

	// static_assert(!tc::instance_or_derived<CPrivateInstantiation1, CTemplate1>);  // Does not compile.
	static_assert(!tc::instance_or_derived<CPrivateInstantiation1, CTemplate1Int>);
	static_assert(!tc::instance_or_derived<CPrivateInstantiation1, CTemplate2>);
}

namespace is_instance_or_derived2_test {
	IS_INSTANCE_OR_DERIVED_TRAIT(2, ((typename)(T1))((typename)(T2))((bool)(b)), using first_argument = T1; using second_argument = T2; static constexpr auto third_argument = b;)

	template<typename, typename, bool> struct CTemplate1 : tc::nonmovable {};
	template<typename, typename, bool> struct CTemplate2 : tc::nonmovable {};

	using CInstantiation1 = CTemplate1<int, bool, true>;
	using CInstantiation2 = CTemplate2<bool, void, false>;

	static_assert(instance_or_derived2<CInstantiation1, CTemplate1>);
	STATICASSERTSAME(int, (is_instance_or_derived2<CInstantiation1, CTemplate1>::first_argument));
	STATICASSERTSAME(bool, (is_instance_or_derived2<CInstantiation1, CTemplate1>::second_argument));
	STATICASSERTEQUAL(true, (is_instance_or_derived2<CInstantiation1, CTemplate1>::third_argument));

	static_assert(!instance_or_derived2<CInstantiation2, CTemplate1>);

	static_assert(!instance_or_derived2<CInstantiation1, CTemplate2>);

	static_assert(instance_or_derived2<CInstantiation2, CTemplate2>);
	STATICASSERTSAME(bool, (is_instance_or_derived2<CInstantiation2, CTemplate2>::first_argument));
	STATICASSERTSAME(void, (is_instance_or_derived2<CInstantiation2, CTemplate2>::second_argument));
	STATICASSERTEQUAL(false, (is_instance_or_derived2<CInstantiation2, CTemplate2>::third_argument));

	template<typename T, bool b> struct CTemplate1Int : CTemplate1<int, T, b> {};

	struct CInstantiation3: CTemplate1Int<double, false> {
		operator CTemplate2<void, void, true>() const&;
		operator CTemplate2<void, void, true>&() const&;
		operator CTemplate2<void, void, true>*() const&;
	};

	static_assert(instance_or_derived2<CInstantiation3, CTemplate1>);
	static_assert(!instance_or_derived2<CInstantiation3, CTemplate2>);

	STATICASSERTSAME(int, (is_instance_or_derived2<CInstantiation3, CTemplate1>::first_argument));
	STATICASSERTSAME(double, (is_instance_or_derived2<CInstantiation3, CTemplate1>::second_argument));
	STATICASSERTEQUAL(false, (is_instance_or_derived2<CInstantiation3, CTemplate1>::third_argument));
}

namespace noncopyable_test {
	struct NonCopyable final : tc::noncopyable {};
	static_assert(!std::is_copy_constructible<NonCopyable>::value);
	static_assert(!std::is_copy_assignable<NonCopyable>::value);
	static_assert(std::is_trivially_move_constructible<NonCopyable>::value);
	static_assert(std::is_trivially_move_assignable<NonCopyable>::value);

	struct NonMovable final : tc::nonmovable {};
	static_assert(!std::is_copy_constructible<NonMovable>::value);
	static_assert(!std::is_copy_assignable<NonMovable>::value);
	static_assert(!std::is_move_constructible<NonMovable>::value);
	static_assert(!std::is_move_assignable<NonMovable>::value);
}

static_assert(std::is_same<tc::common_type_t<std::optional<int> const&, std::nullopt_t>, std::optional<int>>::value);
static_assert(std::is_same<tc::common_type_t<std::nullopt_t, std::optional<int>>, std::optional<int>>::value);
static_assert(std::is_same<tc::common_type_t<std::nullopt_t, std::nullopt_t>, std::nullopt_t>::value);
static_assert(std::is_same<tc::common_type_t<int, std::nullopt_t>, std::optional<int>>::value);
static_assert(std::is_same<tc::common_type_t<int const&, std::nullopt_t>, std::optional<int>>::value);
static_assert(std::is_same<tc::common_type_t<std::nullopt_t, int const&>, std::optional<int>>::value);
