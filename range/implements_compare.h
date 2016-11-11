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

#include "compare.h"
#include "empty_chain.h"
#include "type_traits.h"
#include "return_decltype.h"
#include "equality_comparable.h"
#include <boost/mpl/has_xxx.hpp>

namespace tc {
	////////////////////////////////////////////////////////////////////////////////////
	// curiously recurring template patterns mapping comparison operators to compare
	namespace implements_compare_adl_barrier {
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
	using implements_compare_adl_barrier::implements_compare;
	using implements_compare_adl_barrier::implements_compare_partial;
}