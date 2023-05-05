
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "size.h"

namespace tc
{
	namespace no_adl
	{
		struct[[maybe_unused]] SRestrictSizeDummy {
		};

#ifdef _CHECKS
		template< typename Rng >
		struct[[maybe_unused]] SRestrictSize {
			explicit constexpr SRestrictSize(Rng const& rng, typename boost::range_size<Rng>::type nSizeMin, typename boost::range_size<Rng>::type nSizeMax) noexcept
				: m_rng(rng)
				, m_nSizeMin(tc_move(nSizeMin))
				, m_nSizeMax(tc_move(nSizeMax)) {
			}

			constexpr ~SRestrictSize() {
				auto const nSize = tc::size(m_rng);
				_ASSERTE(m_nSizeMin <= nSize);
				_ASSERTE(nSize <= m_nSizeMax);
			}

		private:
			Rng const& m_rng;
			typename boost::range_size<Rng>::type m_nSizeMin;
			typename boost::range_size<Rng>::type m_nSizeMax;
		};
#endif
	}

	template< typename Rng >
	constexpr auto restrict_size_decrement(Rng const& rng, typename boost::range_size<Rng>::type nDecrementMin, typename boost::range_size<Rng>::type nDecrementMax) {
#ifdef _CHECKS
		if constexpr (tc::has_size<Rng const&>) {
			auto const nSize = tc::size(rng);
			return no_adl::SRestrictSize<Rng>(
				rng,
				nDecrementMax < nSize ? nSize - nDecrementMax : tc::explicit_cast<typename boost::range_size<Rng>::type>(0),
				nDecrementMin < nSize ? nSize - nDecrementMin : tc::explicit_cast<typename boost::range_size<Rng>::type>(0)
			);
		} else
#endif
		{
			return no_adl::SRestrictSizeDummy{};
		}
	}

	template< typename Rng >
	constexpr auto restrict_size_decrement(Rng const& rng) {
		return restrict_size_decrement(rng, 1, 1);
	}

}
