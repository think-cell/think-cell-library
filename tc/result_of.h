
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "type_traits.h"

namespace tc {
	template< typename Func, typename... Args >
	using decayed_invoke_result_t=tc::decay_t< std::invoke_result_t< Func, Args... > >;
}
