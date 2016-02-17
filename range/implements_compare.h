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

namespace tc {
	////////////////////////////////////////////////////////////////////////////////////
	// curiously recurring template patterns mapping comparison operators to compare
	namespace implements_compare_impl {

		template< typename T, typename B=tc::empty_chain<T> >
		struct implements_compare_partial : public B {
			constexpr implements_compare_partial() noexcept {}
			using B::B;
			friend bool operator<( T const& lhs, T const& rhs ) noexcept {
				return tc::compare( lhs, rhs )<tc::order::equal;
			}
			friend bool operator<=( T const& lhs, T const& rhs ) noexcept {
				return tc::compare( lhs, rhs )<=tc::order::equal;
			}
			// prefer < over > and <= over >= in our code
#if 0
			friend bool operator>( T const& lhs, T const& rhs ) noexcept {
				return tc::order::equal<tc::compare( lhs, rhs );
			}
			friend bool operator>=( T const& lhs, T const& rhs ) noexcept {
				return tc::order::equal<=tc::compare( lhs, rhs );
			}
#endif
			friend bool operator!=( T const& lhs, T const& rhs ) noexcept {
				return !(lhs == rhs);
			}
		};

		template< typename T, typename B=tc::empty_chain<T> >
		struct implements_compare : implements_compare_partial<T,B> {
		private:
			using base_ = implements_compare_partial<T,B>;
		public:
			constexpr implements_compare() noexcept {}
			using base_::base_;
			friend bool operator==( T const& lhs, T const& rhs ) noexcept {
				return tc::order::equal==tc::compare( lhs, rhs );
			}
		};
	}
	using implements_compare_impl::implements_compare;
	using implements_compare_impl::implements_compare_partial;
}