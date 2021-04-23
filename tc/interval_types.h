
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
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
	DEFINE_UNPREFIXED_ENUM(lohi, (lo)(hi))

	namespace lohi_adl {
		// support for multiplication with tc::sign
		constexpr lohi operator-(lohi lohi_) noexcept {
			return ~lohi_;
		}
	}

	[[nodiscard]] constexpr EAlign lohi_to_ealign(lohi lohi_) noexcept {
		if( lohi::lo == lohi_ ) {
			return ealignLOW;
		} else {
			return ealignHIGH;
		}
	}

	template< typename T >
	[[nodiscard]] constexpr T negate_if( bool_context b, T t ) noexcept{
		if(b) {
			return -t;
		} else{
			return t;
		}
	}
	template< typename T >
	[[nodiscard]] constexpr T not_if( bool_context b, T t ) noexcept{
		if(b) {
			return ~t;
		} else{
			return t;
		}
	}

	namespace interval_adl {
		template< typename T > struct interval;
	}
	using interval_adl::interval;
}
