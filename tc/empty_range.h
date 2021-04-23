
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "break_or_continue.h"

namespace tc {
	namespace empty_range_adl {
		struct empty_range {
			template< typename Func >
			constexpr auto operator()(Func const&) const& noexcept {
				return INTEGRAL_CONSTANT(tc::continue_)();
			}

			constexpr unsigned int size() const& noexcept {
				return 0;
			}
		};

		template< typename Func >
		constexpr auto for_each_reverse_impl(empty_range const&, Func const&) noexcept {
			return INTEGRAL_CONSTANT(tc::continue_)();
		}
	}
	using empty_range_adl::empty_range;
}
