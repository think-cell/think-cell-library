
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "meta.h"
#include "type_list.h"

STATICASSERTSAME((tc::type::list<char, int, double, void>), (tc::type::concat_t<tc::type::list<>, tc::type::list<char, int, double, void>>));
STATICASSERTSAME((tc::type::list<char, int, double, void>), (tc::type::concat_t<tc::type::list<char, int>, tc::type::list<double, void>>));
STATICASSERTSAME((tc::type::list<char, int, double, void>), (tc::type::concat_t<tc::type::list<char>, tc::type::list<>, tc::type::list<int, double, void>>));

STATICASSERTSAME(tc::type::list<>, (tc::type::filter_t<tc::type::list<char, int, double, void>, tc::is_char_range>));
STATICASSERTSAME((tc::type::list<char const*, std::string>), (tc::type::filter_t<tc::type::list<char, char const*, int, double, std::vector<int>, std::string, void>, tc::is_char_range>));
STATICASSERTSAME((tc::type::list<char const*, tc::char16 const*>), (tc::type::filter_t<tc::type::list<char const*, tc::char16 const*>, tc::is_char_range>));

STATICASSERTSAME(char const*, (tc::type::find_unique_if_t<tc::type::list<char, char const*, int, double, std::vector<int>, void>, tc::is_char_range>));
STATICASSERTEQUAL(0, (tc::type::find_unique_if<tc::type::list<char const*, char, int, double, std::vector<int>, void>, tc::is_char_range>::index));
static_assert(tc::type::has_unique_if<tc::type::list<char, char const*, int, double, std::vector<int>, void>, tc::is_char_range>::value);

static_assert(tc::type::curry<std::is_same, char>::type<char>::value);
static_assert(!tc::type::curry<std::is_same, char>::type<unsigned char>::value);

STATICASSERTEQUAL(5, (tc::type::find_unique<tc::type::list<char, int, double, char const*, std::vector<int>, void>, void>::index));
STATICASSERTSAME(tc::type::find_unique_if_result::type_not_found, (tc::type::find_unique<tc::type::list<char, int, double, char const*, std::vector<int>>, void>));
static_assert(tc::type::has_unique<tc::type::list<char, int, double, char const*, std::vector<int>, void>, void>::value);

//STATICASSERTSAME(char const*, tc::type::find_unique_if_t<tc::type::list<char, char const*, int, double, std::vector<int>, std::string, void>, tc::is_char_range>);
//using type = tc::type::find_unique_if_t<tc::type::list<char, int, double, std::vector<int>, void>, tc::is_char_range>;
static_assert(!tc::type::has_unique_if<tc::type::list<char, char const*, int, double, std::vector<int>, std::string, void>, tc::is_char_range>::value);
static_assert(!tc::type::has_unique<tc::type::list<char, char const*, int, double, std::vector<int>, std::string, int>, int>::value);

static_assert(!tc::type::has_unique_if<tc::type::list<char, int, double, std::vector<int>, void>, tc::is_char_range>::value);
static_assert(!tc::type::has_unique<tc::type::list<char, char const*, double, std::vector<int>, std::string>, int>::value);

STATICASSERTSAME((tc::type::list<int, float, char const*>), (tc::type::transform_t<tc::type::list<int&, float const&, char const*>, tc::decay_t>));

template<typename T0, typename T1>
using common_type_2_t = std::common_type_t<T0, T1>;

STATICASSERTSAME(int, (tc::type::accumulate_t<tc::type::list<int>, common_type_2_t>));
STATICASSERTSAME(float, (tc::type::accumulate_t<tc::type::list<int, char, unsigned, float/*, int**/>, common_type_2_t>));

STATICASSERTSAME((tc::type::take_first_t<tc::type::list<int, char, unsigned, float>>), (tc::type::list<int>));
STATICASSERTSAME((tc::type::take_first_t<tc::type::list<int, char, unsigned, float>, 2>), (tc::type::list<int, char>));
STATICASSERTSAME((tc::type::take_first_t<tc::type::list<int, char, unsigned, float>, 0>), (tc::type::list<>));
STATICASSERTSAME((tc::type::drop_first_t<tc::type::list<int, char, unsigned, float>>), (tc::type::list<char, unsigned, float>));
STATICASSERTSAME((tc::type::drop_first_t<tc::type::list<int, char, unsigned, float>, 2>), (tc::type::list<unsigned, float>));
STATICASSERTSAME((tc::type::drop_first_t<tc::type::list<int, char>, 2>), (tc::type::list<>));
STATICASSERTSAME((tc::type::take_last_t<tc::type::list<int, char, unsigned, float>>), (tc::type::list<float>));
STATICASSERTSAME((tc::type::take_last_t<tc::type::list<int, char, unsigned, float>, 2>), (tc::type::list<unsigned, float>));
STATICASSERTSAME((tc::type::take_last_t<tc::type::list<int, char, unsigned, float>, 0>), (tc::type::list<>));
STATICASSERTSAME((tc::type::drop_last_t<tc::type::list<int, char, unsigned, float>>), (tc::type::list<int, char, unsigned>));
STATICASSERTSAME((tc::type::drop_last_t<tc::type::list<int, char, unsigned, float>, 2>), (tc::type::list<int, char>));
STATICASSERTSAME((tc::type::drop_last_t<tc::type::list<int, char>, 2>), (tc::type::list<>));

namespace {
	template<template<typename, std::size_t> typename T, typename List, std::size_t N, typename Enable=void>
	struct type_algo_sfinae_test final: std::false_type {};

	template<template<typename, std::size_t> typename T, typename List, std::size_t N>
	struct type_algo_sfinae_test<T, List, N, tc::void_t<T<List, N>>>: std::true_type {};
}

static_assert(type_algo_sfinae_test<tc::type::take_first_t, tc::type::list<int>, 1>::value);
static_assert(type_algo_sfinae_test<tc::type::take_first_t, tc::type::list<int>, 0>::value);
static_assert(!type_algo_sfinae_test<tc::type::take_first_t, tc::type::list<int>, 2>::value);
static_assert(!type_algo_sfinae_test<tc::type::take_first_t, tc::type::list<>, 1>::value);

static_assert(type_algo_sfinae_test<tc::type::drop_first_t, tc::type::list<int>, 1>::value);
static_assert(type_algo_sfinae_test<tc::type::drop_first_t, tc::type::list<int>, 0>::value);
static_assert(!type_algo_sfinae_test<tc::type::drop_first_t, tc::type::list<int>, 2>::value);
static_assert(!type_algo_sfinae_test<tc::type::drop_first_t, tc::type::list<>, 1>::value);

static_assert(type_algo_sfinae_test<tc::type::take_last_t, tc::type::list<int>, 1>::value);
static_assert(type_algo_sfinae_test<tc::type::take_last_t, tc::type::list<int>, 0>::value);
static_assert(!type_algo_sfinae_test<tc::type::take_last_t, tc::type::list<int>, 2>::value);
static_assert(!type_algo_sfinae_test<tc::type::take_last_t, tc::type::list<>, 1>::value);

static_assert(type_algo_sfinae_test<tc::type::drop_last_t, tc::type::list<int>, 1>::value);
static_assert(type_algo_sfinae_test<tc::type::drop_last_t, tc::type::list<int>, 0>::value);
static_assert(!type_algo_sfinae_test<tc::type::drop_last_t, tc::type::list<int>, 2>::value);
static_assert(!type_algo_sfinae_test<tc::type::drop_last_t, tc::type::list<>, 1>::value);

static_assert(tc::type::all_of<tc::type::list<int, short, long long>, tc::is_actual_integer>::value);
static_assert(tc::type::all_of<tc::type::list<>, tc::is_actual_integer>::value);
static_assert(!tc::type::all_of<tc::type::list<int, short, long long, char>, tc::is_actual_integer>::value);

static_assert(tc::type::any_of<tc::type::list<int, short, char, long long>, tc::is_char>::value);
static_assert(!tc::type::any_of<tc::type::list<int, short, long long>, tc::is_char>::value);
static_assert(!tc::type::any_of<tc::type::list<>, tc::is_char>::value);
