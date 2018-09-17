
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

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

namespace EAlign_adl {
	constexpr EAlign operator-(EAlign ealign) noexcept {
		return ealignLOW+(ealignHIGH-ealign);
	}
}

namespace tc {
	namespace lohi_adl {
		enum class lohi { lo, hi, end__ };
		DEFINE_CONTIGUOUS_ENUM(lohi,lohi::lo,lohi::end__)
	}
	using lohi_adl::lohi;
	constexpr lohi lo=lohi::lo;
	constexpr lohi hi=lohi::hi;

	constexpr EAlign lohi_to_ealign(lohi lohi_) noexcept {
		if( lohi::lo == lohi_ ) {
			return ealignLOW;
		} else {
			return ealignHIGH;
		}
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
