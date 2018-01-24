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

#include "enum.h"

//////////////////////////////////////
// EAlign

// persistent
DEFINE_ENUM_WITH_OFFSET(EAlign, ealign, -1,
	(LOW)
	(CENTER)
	(HIGH)
)

namespace EAlign_adl_barrier {
	constexpr EAlign operator-(EAlign ealign) noexcept {
		return ealignLOW+(ealignHIGH-ealign);
	}
}

namespace tc {
	namespace lohi_adl_barrier {
		enum class lohi { lo, hi, end__ };
		DEFINE_CONTIGUOUS_ENUM(lohi,lohi::lo,lohi::end__)
	}
	using lohi_adl_barrier::lohi;
	constexpr lohi lo=lohi::lo;
	constexpr lohi hi=lohi::hi;

	constexpr EAlign lohi_to_ealign(lohi lohi_) {
		return lohi::lo == lohi_ ? ealignLOW : ealignHIGH;
	}

	template< typename T >
	T negate_if( bool_context b, T t ) noexcept{
		if(b) {
			return -t;
		} else{
			return t;
		}
	}
	template< typename T >
	T not_if( bool_context b, T t ) noexcept{
		if(b) {
			return ~t;
		} else{
			return t;
		}
	}
}
