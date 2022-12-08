
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/inplace.h"

namespace tc {
	namespace no_adl {
		struct any_accu final {
			constexpr explicit any_accu() noexcept :
				m_b(false)
			{}

			constexpr explicit any_accu(bool b) noexcept :
				m_b(b)
			{}

			constexpr operator bool() const& noexcept {
				return m_b;
			}

			constexpr void operator()(bool b) & noexcept {
				tc_inplace(m_b) || b;
			}

		private:
			bool m_b;
		};
	}

	using no_adl::any_accu;
}