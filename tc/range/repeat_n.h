// 
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "../base/assert_defs.h"
#include "range_adaptor.h"

namespace tc {
	////////////////////////
	// repeat_range

	namespace no_adl {
		template <typename T>
		struct [[nodiscard]] repeat_range
			: range_iterator_from_index<
				repeat_range<T>,
				std::size_t
			>
		{
		private:
			using this_type = repeat_range;
			std::size_t m_ct;
			reference_or_value< T > m_t;
		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex=false;

			explicit constexpr repeat_range(std::size_t ct, T&& t) noexcept
				: m_ct(ct)
				, m_t(aggregate_tag, tc_move_if_owned(t))
			{}

		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				return 0;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				return m_ct;
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				++idx;
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& noexcept -> void{
				--idx;
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index /*idx*/) const& return_decltype_noexcept(
				*m_t
			)

			STATIC_FINAL_MOD(constexpr, distance_to_index)(tc_index idxLhs, tc_index idxRhs) const& noexcept -> std::ptrdiff_t {
				return static_cast<std::ptrdiff_t>(idxRhs-idxLhs); // assumes two's complement negatives
			}

			STATIC_FINAL_MOD(constexpr, advance_index)(tc_index& idx, std::ptrdiff_t d) const& noexcept -> void {
				idx+=static_cast<std::size_t>(d); // modulo arithmetic
			}
		public:
			constexpr std::size_t size() const& noexcept {
				return m_ct;
			}
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
