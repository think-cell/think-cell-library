//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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
				return tc::for_each(*m_baserng, std::bind(tc::fn_for_each(), std::placeholders::_1, std::ref(func)));
			}
		};
	}

	using flatten_adaptor_adl_barrier::flatten_adaptor;

	template<typename Rng>
	auto flatten(Rng&& rng) noexcept return_ctor(
		flatten_adaptor< view_by_value_t<Rng> >,
		(aggregate_tag(), std::forward<Rng>(rng))
	)

}
