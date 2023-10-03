
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/accessors.h"
#include "../base/noncopyable.h"
#include "../base/reference_or_value.h"

namespace tc {
	namespace no_adl {
		template<typename It>
		struct iterator_cache final : tc::nonmovable /*m_ref may contain pointer into m_it*/ {
		private:
			PRIVATE_MEMBER_PUBLIC_ACCESSOR(It, m_it)
			tc::reference_or_value< typename std::iterator_traits<It>::reference > m_ref;

		public:
			constexpr iterator_cache(It it) noexcept
				: m_it(tc_move(it))
				, m_ref(aggregate_tag, *m_it)
			{}

			constexpr iterator_cache& operator=(It it) & noexcept {
				m_it=tc_move(it);
				m_ref = tc::reference_or_value<typename std::iterator_traits<It>::reference>(tc::aggregate_tag, *m_it);
				return *this;
			}

			constexpr auto operator*() const& return_decltype_noexcept( *m_ref )
			constexpr auto operator*() && return_decltype_xvalue_by_ref_noexcept( *tc_move(m_ref) )
			constexpr auto operator*() const&& noexcept = delete;
		};
	}
	using no_adl::iterator_cache;
}
