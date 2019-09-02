
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

namespace tc {
/*	template< typename T >
	T& as_lvalue( T& t ) noexcept {
		return t;
	}*/

	template< typename T >
	T& as_lvalue(T&& t) noexcept {
		return t;
	}
}
