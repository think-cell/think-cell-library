
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
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
			typename Rng
		>
		struct join_adaptor {
		private:
			reference_or_value<Rng> m_baserng;

		public:
			template<typename Rhs>
			explicit join_adaptor(aggregate_tag, Rhs&& rhs) noexcept
				: m_baserng( aggregate_tag(), std::forward<Rhs>(rhs) )
			{}

			template< typename Func >
			auto operator()(Func func) const& MAYTHROW {
				return tc::for_each(*m_baserng, [&](auto&& _) MAYTHROW { return tc::for_each(std::forward<decltype(_)>(_), func); });
			}
		};
	}

	using no_adl::join_adaptor;

	namespace no_adl {
		template< typename Rng, bool bConst >
		struct range_reference_join_adaptor {
			using type = tc::range_reference_t<
				reference_for_value_or_reference_with_index_range_t<Rng, bConst>
			>;
		};

		template< typename Rng >
		struct range_reference<join_adaptor<Rng>> : range_reference_join_adaptor<Rng, false> {};

		template< typename Rng >
		struct range_reference<join_adaptor<Rng> const> : range_reference_join_adaptor<Rng, true> {};
	}

	template<typename Rng>
	auto join(Rng&& rng) noexcept return_ctor(
		join_adaptor< Rng >,
		(aggregate_tag(), std::forward<Rng>(rng))
	)

}
