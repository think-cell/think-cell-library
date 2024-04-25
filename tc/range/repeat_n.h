// 
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "iota_range.h"

namespace tc {
	////////////////////////
	// repeat_range

	namespace no_adl {
		template <typename T>
		struct [[nodiscard]] TC_EMPTY_BASES repeat_range : tc::iota_range_adaptor<repeat_range<T>, std::size_t>
		{
		private:
			using this_type = repeat_range;
			std::size_t m_ct;
			tc::reference_or_value< T > m_t;
		public:
			using typename this_type::index_range_adaptor::tc_index;
			static constexpr bool c_bHasStashingIndex=false;

			explicit constexpr repeat_range(std::size_t ct, T&& t) noexcept
				: m_ct(ct)
				, m_t(aggregate_tag, tc_move_if_owned(t))
			{}

		private:
			STATIC_FINAL_MOD(constexpr static, begin_index)() noexcept -> tc_index {
				return 0;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				return m_ct;
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc::unused /*idx*/) const& return_decltype_noexcept(
				*m_t
			)
		};
	}
	using no_adl::repeat_range;

	template<typename T>
	constexpr auto enable_stable_index_on_move<repeat_range<T>> = true;

	template<typename TSize, typename T>
	constexpr auto repeat_n( TSize&& ct, T && t ) return_ctor_noexcept(
		no_adl::repeat_range<T>, ( tc::explicit_cast<std::size_t>(tc_move_if_owned(ct)), tc_move_if_owned(t) )
	)
}
