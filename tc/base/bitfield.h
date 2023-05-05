
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"

#include <bit>
#include <limits>

#if defined(_MSC_VER) && defined(_M_ARM64EC)
#include <intrin.h>
#endif

namespace tc {
	template<typename Uint>
	[[nodiscard]] constexpr int index_of_least_significant_bit(Uint u) noexcept {
#if defined(_MSC_VER) && defined(_M_ARM64EC)
		// https://developercommunity.visualstudio.com/t/Usage-of-std::countr_zero-when-compiling/1645529
		if( std::is_constant_evaluated() ) {
			return std::_Countr_zero_fallback(VERIFYPRED(u, 0 != _));
		} else {
			unsigned long nIndex;
			VERIFY(_BitScanForward64(&nIndex, VERIFYPRED(u, 0 != _)));
			return nIndex;
		}
#else
		return std::countr_zero(VERIFYPRED(u, 0 != _));
#endif
	}

	template<typename Uint>
	[[nodiscard]] constexpr Uint least_significant_bit(Uint u) noexcept {
		static_assert( std::is_unsigned<Uint>::value );
MODIFY_WARNINGS_BEGIN(((disable)(4146))) // unary minus operator applied to unsigned type, result still unsigned
		return u & -u;
MODIFY_WARNINGS_END
	}

	template<typename Uint>
	[[nodiscard]] constexpr int index_of_most_significant_bit(Uint u) noexcept {
		// return std::bit_width() - 1;
		return std::numeric_limits<Uint>::digits - 1 - std::countl_zero(VERIFYPRED(u, 0 != _));
	}

	template<typename Uint>
	[[nodiscard]] constexpr Uint most_significant_bit(Uint u) noexcept {
		// return std::bit_floor(u);
		if( 0 == u ) {
			return 0;
		} else {
			return Uint{1} << tc::index_of_most_significant_bit(u);
		}
	}
}
