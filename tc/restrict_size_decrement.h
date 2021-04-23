
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "size.h"

namespace tc
{
	namespace no_adl
	{
		struct[[maybe_unused]] SRestrictSizeDummyBase {
		protected:
			constexpr void dtor() const {}
		};

#ifdef _CHECKS
		template< typename Rng >
		struct[[maybe_unused]] SRestrictSizeBase {
			explicit constexpr SRestrictSizeBase(Rng const& rng, typename boost::range_size<Rng>::type nSizeMin, typename boost::range_size<Rng>::type nSizeMax) noexcept
				: m_rng(rng)
				, m_nSizeMin(tc_move(nSizeMin))
				, m_nSizeMax(tc_move(nSizeMax)) {
			}

		protected:
			constexpr void dtor() const noexcept {
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

		template< typename Base, bool c_bUseDestructor >
		struct[[maybe_unused]] SRestrictSize;

		template< typename Base >
		struct[[maybe_unused]] SRestrictSize<Base, true> : Base {
			using Base::Base;
			~SRestrictSize() {
				this->dtor();
			}
		};

		template< typename Base >
		struct[[maybe_unused]] SRestrictSize<Base, false> : Base {
			using Base::Base;
			using Base::dtor;
		};
	}

	namespace restrict_size_decrement_impl
	{
		template< typename Rng, bool c_bUseDestructor >
		constexpr auto restrict_size_decrement_generic(Rng const& rng, typename boost::range_size<Rng>::type nDecrementMin, typename boost::range_size<Rng>::type nDecrementMax) {
#ifdef _CHECKS
			if constexpr (tc::has_size<Rng const&>::value) {
				auto const nSize = tc::size(rng);
				return no_adl::SRestrictSize<no_adl::SRestrictSizeBase<Rng>, c_bUseDestructor>(
					rng,
					nDecrementMax < nSize ? nSize - nDecrementMax : tc::explicit_cast<typename boost::range_size<Rng>::type>(0),
					nDecrementMin < nSize ? nSize - nDecrementMin : tc::explicit_cast<typename boost::range_size<Rng>::type>(0)
				);
			} else
#endif
			{
				return no_adl::SRestrictSize<no_adl::SRestrictSizeDummyBase, c_bUseDestructor>{};
			}
		}
	}

	template< typename Rng >
	auto restrict_size_decrement(Rng const& rng, typename boost::range_size<Rng>::type nDecrementMin, typename boost::range_size<Rng>::type nDecrementMax) {
		return restrict_size_decrement_impl::restrict_size_decrement_generic<Rng, true>(rng, nDecrementMin, nDecrementMax);
	}

	template< typename Rng >
	constexpr auto constexpr_restrict_size_decrement(Rng const& rng, typename boost::range_size<Rng>::type nDecrementMin, typename boost::range_size<Rng>::type nDecrementMax) {
		return restrict_size_decrement_impl::restrict_size_decrement_generic<Rng, false>(rng, nDecrementMin, nDecrementMax);
	}

	template< typename Rng >
	auto restrict_size_decrement(Rng const& rng) {
		return restrict_size_decrement_impl::restrict_size_decrement_generic<Rng, true>(rng, 1, 1);
	}

	template< typename Rng >
	constexpr auto constexpr_restrict_size_decrement(Rng const& rng) {
		return restrict_size_decrement_impl::restrict_size_decrement_generic<Rng, false>(rng, 1, 1);
	}

}
