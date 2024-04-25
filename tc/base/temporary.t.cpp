
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "temporary.h"

#include "return_decltype.h"
#include "type_traits.h"

STATICASSERTSAME(tc::prvalue_as_temporary_t<int>, TC_FWD(tc::temporary<int, 0>));
STATICASSERTSAME(tc::prvalue_as_temporary_t<int&>, int&);
STATICASSERTSAME(tc::prvalue_as_temporary_t<int&&>, int&&);
STATICASSERTSAME(TC_FWD(tc::prvalue_as_temporary_t<tc::temporary<int, 0>>), TC_FWD(tc::temporary<int, 0>));
STATICASSERTSAME(TC_FWD(tc::prvalue_as_temporary_t<tc::temporary<int, 1>>), TC_FWD(tc::temporary<int, 1>));

STATICASSERTSAME(tc::return_temporary_t<int>, int);
STATICASSERTSAME(tc::return_temporary_t<int&>, int&);
STATICASSERTSAME(tc::return_temporary_t<int&&>, int&&);
STATICASSERTSAME(TC_FWD(tc::return_temporary_t<tc::temporary<int, 0>>), int);
STATICASSERTSAME(TC_FWD(tc::return_temporary_t<tc::temporary<int const, 0>>), int);
STATICASSERTSAME(TC_FWD(tc::return_temporary_t<tc::temporary<int, 1>>), TC_FWD(tc::temporary<int, 1>));

STATICASSERTSAME(tc::store_temporary_t<int>, int);
STATICASSERTSAME(tc::store_temporary_t<int&>, int&);
STATICASSERTSAME(tc::store_temporary_t<int&&>, int&&);
STATICASSERTSAME(TC_FWD(tc::store_temporary_t<tc::temporary<int, 0>>), int);
STATICASSERTSAME(TC_FWD(tc::store_temporary_t<tc::temporary<int const, 0>>), int);
STATICASSERTSAME(TC_FWD(tc::store_temporary_t<tc::temporary<int, 1>>), int);

STATICASSERTSAME(tc::unwrap_temporary_t<int>, int);
STATICASSERTSAME(tc::unwrap_temporary_t<int&>, int&);
STATICASSERTSAME(tc::unwrap_temporary_t<int&&>, int&&);
STATICASSERTSAME(TC_FWD(tc::unwrap_temporary_t<tc::temporary<int, 0>>), int&&);
STATICASSERTSAME(TC_FWD(tc::unwrap_temporary_t<tc::temporary<int, 0>&&>), int&&);
STATICASSERTSAME(TC_FWD(tc::unwrap_temporary_t<tc::temporary<int, 0> const&&>), int&&);
STATICASSERTSAME(TC_FWD(tc::unwrap_temporary_t<tc::temporary<int, 0>&>), int&);
STATICASSERTSAME(TC_FWD(tc::unwrap_temporary_t<tc::temporary<int, 0> const&>), int&);

STATICASSERTSAME(TC_FWD(tc::rewrap_temporary_t<int, tc::temporary<int, 0>>), int);
STATICASSERTSAME(TC_FWD(tc::rewrap_temporary_t<int&, tc::temporary<int, 0>>), int&);
STATICASSERTSAME(TC_FWD(tc::rewrap_temporary_t<int&&, tc::temporary<int, 0>>), TC_FWD(tc::temporary<int, 0>));
STATICASSERTSAME(TC_FWD(tc::rewrap_temporary_t<int&&, tc::temporary<int, 0>, tc::temporary<int, 1>>), TC_FWD(tc::temporary<int, 0>));

STATICASSERTSAME(TC_FWD(tc::common_type_t<tc::temporary<int, 0>&&, short&, int const&&>), int);

STATICASSERTSAME(TC_FWD(tc::common_reference_t<tc::temporary<int, 0>, int&, int&&>), TC_FWD(tc::temporary<int const, 0>));
STATICASSERTSAME(TC_FWD(tc::common_reference_t<tc::temporary<int, 0>&&, int&, int&&>), TC_FWD(tc::temporary<int const, 0>));
STATICASSERTSAME(TC_FWD(tc::common_reference_t<tc::temporary<int, 0>, int&&>), TC_FWD(tc::temporary<int, 0>));
STATICASSERTSAME(TC_FWD(tc::common_reference_t<tc::temporary<int, 0>, int, int&&>), int);
STATICASSERTSAME(TC_FWD(tc::common_reference_t<tc::temporary<int, 0>, tc::temporary<int, 1>>), TC_FWD(tc::temporary<int, 0>));

STATICASSERTSAME(tc::auto_cref_t<int>, int const);
STATICASSERTSAME(tc::auto_cref_t<int&>, int const&);
STATICASSERTSAME(tc::auto_cref_t<int&&>, int const);
STATICASSERTSAME(TC_FWD(tc::auto_cref_t<tc::temporary<int, 0>>), int const);
STATICASSERTSAME(TC_FWD(tc::auto_cref_t<tc::temporary<int const, 0>>), int const);
STATICASSERTSAME(TC_FWD(tc::auto_cref_t<tc::temporary<int, 1>>), TC_FWD(tc::temporary<int const, 1> const));
