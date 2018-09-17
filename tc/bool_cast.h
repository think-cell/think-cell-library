
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "type_traits.h"

namespace tc {
	template<typename T, std::enable_if_t<std::is_pointer<T>::value || std::is_same<std::remove_cv_t<T>,std::nullptr_t>::value || std::is_class<T>::value || tc::is_bool<T>::value>* =nullptr>
	constexpr auto bool_cast(T const& t) noexcept ->/*to trigger SFINAE*/decltype(static_cast<bool>(t)) {
		return static_cast<bool>( t );
	}
}
