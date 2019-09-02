
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
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
		struct repeat_range
			: range_iterator_generator_from_index<
				repeat_range<T>,
				std::size_t,
				boost::iterators::random_access_traversal_tag
			>
		{
		private:
			using this_type = repeat_range;
			reference_or_value< T > m_t;
			std::size_t m_ct;
		public:
			using index = typename this_type::index;

			explicit repeat_range(T&& t, std::size_t ct) noexcept
				: m_t(aggregate_tag, std::forward<T>(t))
				, m_ct(ct)
			{}

			template<typename Sink>
			auto operator()(Sink sink) const& MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(sink, *m_t)), INTEGRAL_CONSTANT(tc::continue_)> {
				for (auto c = m_ct; 0 < c; --c) {
					RETURN_IF_BREAK(tc::continue_if_not_break(sink, *m_t)); // MAYTHROW
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return 0;
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return m_ct;
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				++idx;
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void{
				--idx;
			}

			STATIC_FINAL(dereference_index)(index /*idx*/) const& noexcept return_decltype(
				*m_t
			)

			STATIC_FINAL(equal_index)(index idxLhs, index idxRhs) const& noexcept -> bool {
				return idxLhs==idxRhs;
			}

			STATIC_FINAL(distance_to_index)(index idxLhs, index idxRhs) const& noexcept -> std::ptrdiff_t {
				return static_cast<std::ptrdiff_t>(idxRhs-idxLhs); // assumes two's complement negatives
			}

			STATIC_FINAL(advance_index)(index& idx, std::ptrdiff_t d) const& noexcept -> void {
				idx+=static_cast<std::size_t>(d); // modulo arithmetic
			}

			std::size_t size() const& noexcept {
				return m_ct;
			}
		};
	}

	template<typename T >
	auto repeat_n( T && t, std::size_t ct ) noexcept return_ctor(
		no_adl::repeat_range<T>, ( std::forward<T>(t), ct )
	)
}
