//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"
#include "range_adaptor.h"
#include "reference_or_value.h"
#include "index_range.h"
#include "for_each.h"

#include <functional>

namespace tc {
	namespace flatten_adaptor_adl_barrier {
		template<
			typename Rng
		>
		struct flatten_adaptor {
		private:
			reference_or_value<index_range_t<Rng>> m_baserng;

		public:
			template<typename Rhs>
			explicit flatten_adaptor(aggregate_tag, Rhs&& rhs) noexcept
				: m_baserng( aggregate_tag(), std::forward<Rhs>(rhs) )
			{}

			template< typename Func >
			auto operator()(Func func) const& MAYTHROW {
				return tc::for_each(*m_baserng, [&](auto&& _) MAYTHROW { return tc::for_each(std::forward<decltype(_)>(_), func); });
			}
		};
	}

	using flatten_adaptor_adl_barrier::flatten_adaptor;

	namespace range_reference_adl_barrier {
		template< typename Rng, bool bConst >
		struct range_reference_flatten_adaptor {
			using type = tc::range_reference_t<
				reference_for_value_or_reference_with_index_range_t<Rng, bConst>
			>;
		};

		template< typename Rng >
		struct range_reference<flatten_adaptor<Rng>> : range_reference_flatten_adaptor<Rng, false> {};

		template< typename Rng >
		struct range_reference<flatten_adaptor<Rng> const> : range_reference_flatten_adaptor<Rng, true> {};
	}

	template<typename Rng>
	auto flatten(Rng&& rng) noexcept return_ctor(
		flatten_adaptor< view_by_value_t<Rng> >,
		(aggregate_tag(), std::forward<Rng>(rng))
	)

}
