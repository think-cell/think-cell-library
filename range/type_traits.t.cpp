//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------


#include "type_traits.h"
#include "range.t.h"


static_assert(std::is_same< tc::remove_rvalue_reference_t<int>, int >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<int const>, int const >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<int&>, int& >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<int const&>, int const& >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<int&&>, int >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<int const&&>, int const >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<std::vector<int>>, std::vector<int> >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<std::vector<int> const>, std::vector<int> const >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<std::vector<int>&>, std::vector<int>& >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<std::vector<int> const&>, std::vector<int> const& >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<std::vector<int>&&>, std::vector<int> >::value, "");
static_assert(std::is_same< tc::remove_rvalue_reference_t<std::vector<int> const&&>, std::vector<int> const >::value, "");


static_assert( tc::is_safely_convertible<int, double>::value, "" );

static_assert( std::is_convertible<double, int>::value, "" );
static_assert( !tc::is_safely_convertible<double, int>::value, "" );

static_assert( std::is_convertible<float, int>::value, "" );
static_assert( !tc::is_safely_convertible<float, int>::value, "" );

static_assert( std::is_convertible<int, unsigned int>::value, "" );
static_assert( !tc::is_safely_convertible<int, unsigned int>::value, "" );

static_assert( std::is_convertible<unsigned int, int>::value, "" );
static_assert( !tc::is_safely_convertible<unsigned int, int>::value, "" );

static_assert(std::is_convertible<int*, bool>::value, "");
static_assert(!tc::is_safely_convertible<int*, bool>::value, "");

// scoped enum (enum class)
enum class TEnumClass { a, b, c };
enum TEnum { x, y, z };
static_assert( !tc::is_safely_convertible<int, TEnumClass>::value, "" );
static_assert( !tc::is_safely_convertible<TEnumClass, int>::value, "" );

// unscoped enum (primitive enum)
enum TPrimitiveEnum { a, b, c };
static_assert( !tc::is_safely_convertible<int, TPrimitiveEnum>::value, "" );
static_assert( std::is_convertible<TPrimitiveEnum, std::underlying_type_t<TPrimitiveEnum> >::value, "" );
static_assert( !tc::is_safely_convertible<TPrimitiveEnum, std::underlying_type_t<TPrimitiveEnum>>::value, "" );

enum TPrimitiveEnum2 { l, m };
static_assert( !tc::is_safely_convertible<TPrimitiveEnum, TPrimitiveEnum2>::value, "" );

struct SBase {};
struct SDerived final : SBase {};
static_assert(std::is_convertible<SDerived, SBase>::value, "");
static_assert(!tc::is_safely_convertible<SDerived, SBase>::value, "");

struct SToInt final {
	operator int() const& noexcept;
};

static_assert(!std::is_convertible<SBase, int>::value, "");
static_assert(std::is_convertible<SToInt, int>::value, "");
static_assert(tc::is_safely_convertible<SToInt, int>::value, "");

static_assert(std::is_convertible<SToInt, int const&>::value, "");
static_assert(!tc::is_safely_convertible<SToInt, int const&>::value, "");

struct A {};
struct B : A {
	operator int(){return 0;}
};

static_assert(
	std::is_same<
		tc::common_reference_t<A&&, A>,
		A
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<A&&, A&&>,
		A&&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<A&, A&>,
		A&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<A&, A&&>,
		A const&&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<A const&&, A&&>,
		A const&&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<A const&, A&>,
		A const&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<B const&, A volatile&>,
		A const volatile&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<B&, A volatile&&>,
		A const volatile&&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<B const&&, A volatile&>,
		A const volatile&&
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_reference_t<B const&&, B volatile&>,
		B const volatile &&
	>::value,
	""
);

/*
	must not compile (slicing)
static_assert(
	std::is_same<
		tc::common_reference_t<B&, A>,
		A
	>::value,
	""
);
*/
/*
static_assert(
	std::is_same<
		tc::common_reference_t<SToInt,int>,
		int
	>::value,
	""
);
*/

#if PERFORMUNITTESTS
namespace {
struct S;
std::unordered_set<S const*> g_sets;

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
		_ASSERT(boost::end(g_sets) != g_sets.find(this));
	}

	void foo() && {
		_ASSERT(boost::end(g_sets) != g_sets.find(this));
	}

	void foo() const & {
		_ASSERT(boost::end(g_sets) != g_sets.find(this));
	}

	void foo() const && {
		_ASSERT(boost::end(g_sets) != g_sets.find(this));
	}

	friend bool operator<(S const& lhs, S const& rhs) {
		_ASSERT(boost::end(g_sets) != g_sets.find(std::addressof(lhs)));
		_ASSERT(boost::end(g_sets) != g_sets.find(std::addressof(rhs)));
		return true;
	}
};

S createS(int) {
	return S{};
}
}
#endif

#include "minmax.h"

static_assert(
	std::is_same<
		tc::common_type_t<int, short>,
		int
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_type_t<tc::size_proxy<int>, short>,
		short
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>>,
		int
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>&>,
		int
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>&&>,
		int
	>::value,
	""
);

static_assert(
	std::is_same<
		tc::common_type_t<int, tc::size_proxy<short>, tc::size_proxy<long>>,
		int
	>::value,
	""
);

/*
// must not compile
static_assert(
	std::is_same<
		tc::common_type_t<int, unsigned int>,
		unsigned int
	>::value,
	""
);
*/

/*
// must not compile
static_assert(
	std::is_same<
		tc::common_type_t<char, char16_t>,
		int
	>::value,
	""
);
*/

/*
	must not compile (slicing)
static_assert(
	std::is_same<
		tc::common_type_t<B,A>,
		A
	>::value,
	""
);
*/

static_assert(
	std::is_same<
		decltype(tc::min(std::declval<tc::size_proxy<long>>(), std::declval<short>())),
		short
	>::value,
	""
);

UNITTESTDEF(minTest) {
	tc::vector<int> vecn;

	static_assert(
		std::is_same<
			tc::common_reference_t<decltype(tc::size(vecn)),short>,
			short
		>::value,
		""
	);

	tc::common_reference_t<decltype(tc::size(vecn)),int> ref = tc::size(vecn);
	ref;

	tc::min(tc::size(vecn),2);

	int a = 3;
	int b = 4;
	_ASSERT(tc::min(a,b) == 3);
	tc::min(a,b) = 7;
	_ASSERT(a==7);

	_ASSERT(tc::min(a,2) == 2);
	_ASSERT(tc::min(5,b) == 4);

	_ASSERT(tc::min(1,a,b) == 1);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<std::int16_t>(), std::declval<std::int32_t>()))
			, std::int32_t
		>::value,
		""
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<std::uint16_t>(), std::declval<std::int32_t>()))
			, std::int32_t
		>::value,
		""
	);

	{
		S s2[2];

		tc::for_each(
			tc::make_range_of_iterators(
				tc::transform(
					tc::transform(
						tc::make_counting_range(0,1),
						[&](int n) noexcept -> S&& {
							return std::move(s2[n]);
						}
					),
					tc::fn_min()
				)
			),
			[&](auto it) noexcept {
				auto const& elem = *it;
				elem;
			}
		);
	}

	{
		S s;
		tc::min(s,S{}).foo();
	}


	tc::projected(tc::fn_min(), &createS)(0,1).foo();


	{
		S s2[2];
		tc::projected(
			tc::fn_min(),
			[&](int n) noexcept -> S&& {
				return std::move(s2[n]);
			}
		)(0,1).foo();
	}

	{
		tc::projected(
			tc::fn_min(),
			[](S&& s) noexcept ->S&& {
				return static_cast<S&&>(s);
			}
		)(createS(1), createS(2));
	}

	_ASSERT(tc::empty(g_sets));

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<int>(),std::declval<long>())),
			tc::common_type_t<int,long>
		>::value,
		""
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<long>(),std::declval<int>())),
			tc::common_type_t<long,int>
		>::value,
		""
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<unsigned long>(),std::declval<unsigned int>())),
			unsigned int
		>::value,
		""
	);

	static_assert(
		std::is_same<
			decltype(tc::min(std::declval<unsigned int>(),std::declval<unsigned long>())),
			unsigned int
		>::value,
		""
	);
}
