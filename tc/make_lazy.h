
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "result_of.h"
#include "return_decltype.h"

template< typename Func >
struct lazy_impl final {
private:
	tc::decay_t<Func> m_func;
public:
	lazy_impl(Func&& func) noexcept
	:	m_func(std::forward<Func>(func))
	{}
	operator std::result_of_t< tc::decay_t<Func> const&() > () const& noexcept(noexcept(std::declval<tc::decay_t<Func> const&>()())) {
		return m_func();
	}
	std::result_of_t< tc::decay_t<Func> const&() > operator()() const& noexcept(noexcept(std::declval<tc::decay_t<Func> const&>()())) {
		return m_func();
	}
};

template< typename Func >
auto make_lazy(Func&& func) noexcept
	return_ctor( lazy_impl<Func>, ( std::forward<Func>(func) ) )

#define MAKE_LAZY( ... ) make_lazy( [&]() noexcept { return (__VA_ARGS__); } )
