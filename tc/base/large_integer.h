
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "integer.h"

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-conversion"
#pragma clang diagnostic ignored "-Wconversion"
#pragma clang diagnostic ignored "-Wcomma"
#else
MODIFY_WARNINGS_BEGIN(((disable)(4459))) // declaration hides global declaration
#endif
#include <boost/multiprecision/cpp_int.hpp>
#ifdef __clang__
#pragma clang diagnostic pop
#else
MODIFY_WARNINGS_END
#endif

namespace tc {
	namespace no_adl {
		template<int nBits> requires (std::numeric_limits<std::intmax_t>::digits<nBits)
		struct int_least<nBits> {
			using type=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits/*not -1 due to signed magnitude representation*/, nBits/*not -1 due to signed magnitude representation*/, boost::multiprecision::signed_magnitude, boost::multiprecision::unchecked, void>>;
		};

		template<int nBits> requires (std::numeric_limits<std::uintmax_t>::digits<nBits)
		struct uint_least<nBits> {
			using type=boost::multiprecision::number<boost::multiprecision::cpp_int_backend<nBits, nBits, boost::multiprecision::unsigned_magnitude, boost::multiprecision::unchecked, void>>;
		};
	}

	namespace actual_integer_like_detail {
		template<unsigned MinBits,unsigned MaxBits,boost::multiprecision::cpp_integer_type SignType,boost::multiprecision::cpp_int_check_type Checked,typename Alloc>
		inline constexpr bool actual_integer_like_impl<boost::multiprecision::number<
			boost::multiprecision::cpp_int_backend<
				MinBits,
				MaxBits,
				SignType,
				Checked,
				Alloc
			>
		>> = true;
	}
}
