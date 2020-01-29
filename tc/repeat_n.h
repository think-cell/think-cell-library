
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "range_defines.h"
#include "range_adaptor.h"

namespace tc {
	////////////////////////
	// repeat_range

	namespace no_adl {
		template <typename T>
		struct [[nodiscard]] repeat_range
#ifndef TC_RANGE_ITERATOR_HELPER_BASE_CLASS_WORKAROUND
			: range_iterator_generator_from_index<
				repeat_range<T>,
				std::size_t
			>
#endif
		{
		private:
			using this_type = repeat_range;
#ifdef TC_RANGE_ITERATOR_HELPER_BASE_CLASS_WORKAROUND
			using Derived = this_type;
#endif
			reference_or_value< T > m_t;
			std::size_t m_ct;
		public:

#ifndef TC_RANGE_ITERATOR_HELPER_BASE_CLASS_WORKAROUND
			using index = typename this_type::index;
#else
			using index = std::size_t;
			DEFINE_RANGE_ITERATOR_FROM_INDEX
#endif

			explicit constexpr repeat_range(T&& t, std::size_t ct) noexcept
				: m_t(aggregate_tag, std::forward<T>(t))
				, m_ct(ct)
			{}

			template<typename Sink>
			constexpr auto operator()(Sink sink) const& noexcept(noexcept(
				tc::continue_if_not_break(sink, *m_t)
			)) -> tc::common_type_t<decltype(tc::continue_if_not_break(sink, *m_t)), INTEGRAL_CONSTANT(tc::continue_)> {
				for (auto c = m_ct; 0 < c; --c) {
					RETURN_IF_BREAK(tc::continue_if_not_break(sink, *m_t)); // MAYTHROW
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> index {
				return 0;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> index {
				return m_ct;
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(index& idx) const& noexcept -> void {
				++idx;
			}

			STATIC_FINAL_MOD(constexpr, decrement_index)(index& idx) const& noexcept -> void{
				--idx;
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(index /*idx*/) const& return_decltype_noexcept(
				*m_t
			)

			STATIC_FINAL_MOD(constexpr, equal_index)(index idxLhs, index idxRhs) const& noexcept -> bool {
				return idxLhs==idxRhs;
			}

			STATIC_FINAL_MOD(constexpr, distance_to_index)(index idxLhs, index idxRhs) const& noexcept -> std::ptrdiff_t {
				return static_cast<std::ptrdiff_t>(idxRhs-idxLhs); // assumes two's complement negatives
			}

			STATIC_FINAL_MOD(constexpr, advance_index)(index& idx, std::ptrdiff_t d) const& noexcept -> void {
				idx+=static_cast<std::size_t>(d); // modulo arithmetic
			}
		public:
			constexpr std::size_t size() const& noexcept {
				return m_ct;
			}
		};
	}

	template<typename T >
	constexpr auto repeat_n( T && t, std::size_t ct ) return_ctor_noexcept(
		no_adl::repeat_range<T>, ( std::forward<T>(t), ct )
	)
}
