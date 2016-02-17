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

#include <type_traits>

namespace tc {
	template< typename T >
	struct is_range;
	
	template<typename Rng>
	struct is_range_with_iterators;
	
	template<typename It>
	struct const_iterator_;

	template<typename It, typename ConstIt>
	struct iterator_base;

	namespace sub_range_impl {
		template< typename Rng >
		struct sub_range;
	}
	using sub_range_impl::sub_range;

	namespace transform_adaptor_impl {
		template< typename Func, typename Rng, bool HasIterator=is_range_with_iterators< Rng >::value >
		struct transform_adaptor;
	}
	using transform_adaptor_impl::transform_adaptor;

	template< typename Rng, typename Enable=void >
	struct make_sub_range_result;

	template<typename T>
	bool bool_cast(T const& t) noexcept {
		static_assert( std::is_pointer<T>::value || std::is_class<T>::value || std::is_same<T,bool>::value, "");
		return static_cast<bool>( VERIFYINITIALIZED(t) );
	}

	struct bool_context final {
		template< typename T >
		bool_context(T const& t) noexcept
			: m_b(tc::bool_cast(t))
		{}
		operator bool() const noexcept { return m_b; }
	private:
		bool const m_b;
	};
}
