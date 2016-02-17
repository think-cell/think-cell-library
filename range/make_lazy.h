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

#pragma once

#include "result_of.h"
#include "return_decltype.h"

template< typename Func >
struct lazy_impl final {
private:
	std::decay_t<Func> m_func;
public:
	lazy_impl(Func&& func) noexcept
	:	m_func(std::forward<Func>(func))
	{}
	operator std::result_of_t< std::decay_t<Func>() > () const MAYTHROW {
		return m_func();
	}
	std::result_of_t< std::decay_t<Func>() > operator()() const MAYTHROW {
		return m_func();
	}
};

template< typename Func >
auto make_lazy(Func&& func) noexcept
	return_ctor( lazy_impl<Func>, ( std::forward<Func>(func) ) )

#define MAKE_LAZY( ... ) make_lazy( [&] { return (__VA_ARGS__); } )
