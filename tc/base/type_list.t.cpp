// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "type_list.h"
#include "type_traits_fwd.h"

STATICASSERTSAME(TC_FWD(boost::mp11::mp_compose<std::add_const_t, std::add_pointer_t, std::add_volatile_t>::template fn<int>), int const* volatile);
STATICASSERTSAME(TC_FWD(tc::mp_chained<std::add_const_t, std::add_pointer_t, std::add_volatile_t>::template fn<int>), int volatile* const);

STATICASSERTSAME(
	TC_FWD(tc::mp_enumerate<boost::mp11::mp_list<int, float, char>, std::tuple<int*, float*, char*>>),
	TC_FWD(boost::mp11::mp_list<
		boost::mp11::mp_list<tc::constant<0>, int, int*>,
		boost::mp11::mp_list<tc::constant<1>, float, float*>,
		boost::mp11::mp_list<tc::constant<2>, char, char*>
	>)
);

static_assert(!tc::mp_find_unique<boost::mp11::mp_list<int, float, void, char>, long>::found);
STATICASSERTEQUAL(TC_FWD(tc::mp_find_unique<boost::mp11::mp_list<int, float, void, char>, float>::index), 1);
static_assert(!tc::mp_find_unique_if<boost::mp11::mp_list<int, float, void, char>, std::is_pointer>::found);
STATICASSERTEQUAL(TC_FWD(tc::mp_find_unique_if<boost::mp11::mp_list<int, float, void, char>, std::is_void>::index), 2);

template<typename...> struct my_tuple {};

STATICASSERTSAME(TC_FWD(tc::mp_common_prefix<boost::mp11::mp_list<int, float>, boost::mp11::mp_list<float, int>>), boost::mp11::mp_list<>);
STATICASSERTSAME(TC_FWD(tc::mp_common_prefix<boost::mp11::mp_list<int, int, float>, boost::mp11::mp_list<int, int, double>>), TC_FWD(boost::mp11::mp_list<int, int>));
STATICASSERTSAME(TC_FWD(tc::mp_common_prefix<my_tuple<int, int, float>, boost::mp11::mp_list<int, int, double>>), TC_FWD(boost::mp11::mp_list<int, int>));
STATICASSERTSAME(TC_FWD(tc::mp_common_prefix<my_tuple<int, int, float>, my_tuple<int, int, double>>), TC_FWD(my_tuple<int, int>));
STATICASSERTSAME(TC_FWD(tc::mp_common_prefix<boost::mp11::mp_list<int, float, char>, boost::mp11::mp_list<int>, boost::mp11::mp_list<int, float, char, long>>), boost::mp11::mp_list<int>);

STATICASSERTSAME(TC_FWD(tc::mp_common_suffix<boost::mp11::mp_list<int, float, char>, boost::mp11::mp_list<long, float, char>>), TC_FWD(boost::mp11::mp_list<float, char>));
