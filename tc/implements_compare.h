
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "compare.h"
#include "empty_chain.h"
#include "type_traits.h"
#include "return_decltype.h"
#include "equality_comparable.h"
#include <boost/mpl/has_xxx.hpp>

namespace tc {
	////////////////////////////////////////////////////////////////////////////////////
	// curiously recurring template patterns mapping comparison operators to compare
	namespace no_adl {
		template< typename T >
		struct implements_compare_partial : public equality_comparable<T> {
		private:
			using base_ = equality_comparable<T>;
		public:
			using base_::base_;

#pragma push_macro("DEFINE_COMPARISON_OP")
#define DEFINE_COMPARISON_OP(op) \
			friend bool operator op( T const& lhs, T const& rhs ) noexcept { \
				return tc::compare( lhs, rhs ) op tc::order::equal; \
			}

			DEFINE_COMPARISON_OP(<)
			DEFINE_COMPARISON_OP(<=)
			// prefer < over > and <= over >= in our code
#if 0
			DEFINE_COMPARISON_OP(>)
			DEFINE_COMPARISON_OP(>=)
#endif
		};

		template< typename T >
		struct implements_compare : implements_compare_partial<T> {
		private:
			using base_ = implements_compare_partial<T>;
		public:
			using base_::base_;

			DEFINE_COMPARISON_OP(==)
		};

#pragma pop_macro("DEFINE_COMPARISON_OP")
	}
	using no_adl::implements_compare;
	using no_adl::implements_compare_partial;
}
