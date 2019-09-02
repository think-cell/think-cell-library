
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_adaptor.h"
#include "reference_or_value.h"
#include "index_range.h"
#include "for_each.h"

#include <functional>

namespace tc {
	namespace no_adl {
		template<
			typename RngRng
		>
		struct [[nodiscard]] join_adaptor: tc::value_type_base<join_adaptor<RngRng>> {
		private:
			reference_or_value<RngRng> m_baserng;

		public:
			template<typename Rhs>
			explicit join_adaptor(aggregate_tag_t, Rhs&& rhs) noexcept
				: m_baserng( aggregate_tag, std::forward<Rhs>(rhs) )
			{}

			template< typename Func >
			auto operator()(Func func) const& MAYTHROW {
				return tc::for_each(*m_baserng, [&](auto&& _) MAYTHROW { return tc::for_each(std::forward<decltype(_)>(_), func); });
			}
		};

		template<typename RngRng>
		struct value_type_base<join_adaptor<RngRng>, tc::void_t<tc::range_value_t<tc::range_value_t<RngRng>>>> {
			using value_type = tc::range_value_t<tc::range_value_t<RngRng>>;
		};
	}
	using no_adl::join_adaptor;

	template<typename RngRng>
	auto join(RngRng&& rng) noexcept return_ctor(
		join_adaptor< RngRng >,
		(aggregate_tag, std::forward<RngRng>(rng))
	)
}
