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
#include <boost/mpl/identity.hpp>

namespace tc {
	template<typename T>
	using remove_cvref_t=std::remove_cv_t< std::remove_reference_t<T> >;

	template<typename T>
	struct add_const_also_to_ref final {
		using type = T const;
	};

	template<typename T>
	struct add_const_also_to_ref<T&> final {
		using type = T const&;
	};

	template<typename T>
	struct add_const_also_to_ref<T&&> final {
		using type = T const&&;
	};

	template<typename T>
	using add_const_also_to_ref_t=typename add_const_also_to_ref<T>::type;

	template<typename T>
	struct decay {
		using type=std::decay_t<T>; // must still do array-to-pointer, function-to-pointer
	};

	// forbid decaying of C arrays, they decay to pointers, very unlike std/tc::array
	struct do_not_decay_arrays;
#define DECAY_ARRAY_IMPL(cv) \
	template<typename T> \
	struct decay<T cv[]> { \
		using type=do_not_decay_arrays; \
	}; \
	template<typename T,std::size_t N> \
	struct decay<T cv[N]> { \
		using type = do_not_decay_arrays; \
	};

	DECAY_ARRAY_IMPL( BOOST_PP_EMPTY() )
#if !defined(_MSC_VER) || 190023725<=_MSC_FULL_VER
	DECAY_ARRAY_IMPL(const)
	DECAY_ARRAY_IMPL(volatile)
	DECAY_ARRAY_IMPL(const volatile)
#endif
#undef DECAY_ARRAY_IMPL

	template<typename T>
	struct decay<T volatile> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T const> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T const volatile> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct decay<T&> {
		using type=typename tc::decay<T>::type; // recursive
	};
	
	template<typename T>
	struct decay<T&&> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	using decay_t=typename decay<T>::type;

	//	auto a=b; uses std::decay
	// <=>
	//	auto a=make_copy(b); uses tc::decay_t
	template<typename T>
	tc::decay_t<T&&> make_copy(T&& t) noexcept {
		return static_cast< tc::decay_t<T&&> >( std::forward<T>(t) );
	}
}

