
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../algorithm/break_or_continue.h"
#include "meta.h"

namespace tc {
	namespace empty_range_adl {
		struct TC_EMPTY_BASES empty_range {
			constexpr auto operator()(tc::unused /*sink*/) const& noexcept {
				return tc::constant<tc::continue_>();
			}
		};

		constexpr auto for_each_reverse_impl(empty_range const&, tc::unused /*sink*/) noexcept {
			return tc::constant<tc::continue_>();
		}

		auto range_output_t_impl(empty_range const&) noexcept -> tc::type::list<>; // declaration only
	}

	namespace no_adl {
		template<>
		struct constexpr_size_base<empty_range> : tc::constant<tc::explicit_cast<std::uint8_t>(0)> {};
	}
}
