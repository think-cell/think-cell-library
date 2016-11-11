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

#include <boost/type_traits/detail/yes_no_type.hpp>

#include <type_traits>
#include <utility>

#define TC_HAS_MEM_FN_XXX_TRAIT_DEF( name ) \
\
struct has_mem_fn3_ ## name ## _base_t { void name( ) { } }; \
\
template< void (has_mem_fn3_ ## name ## _base_t::*)( ) > struct has_mem_fn3_ ## name ## _detector { \
	using type = boost::type_traits::no_type; \
}; \
\
template< typename T > typename has_mem_fn3_ ## name ## _detector< &T::name >::type has_mem_fn3_ ## name ## _test(int); \
\
template< typename T > boost::type_traits::yes_type has_mem_fn3_ ## name ## _test(...); \
\
template< typename T > struct has_mem_fn3_ ## name ## _derived_t : T { \
	using T::name; \
	struct dummy_result_t {}; /*something that is not convertible to anything*/ \
	dummy_result_t name(...) const; \
}; \
\
template< typename T, typename Signature > struct has_mem_fn3_helper_ ## name ; \
\
template< typename T, typename Signature = void > struct has_mem_fn3_ ## name : std::integral_constant< bool, \
	has_mem_fn3_ ## name< T, void >::value && has_mem_fn3_helper_ ## name < T, Signature >::value \
> {}; \
\
template< typename T > struct has_mem_fn3_ ## name< T, void > { \
	struct has_mem_fn3_ ## name ## _derived_t : T, has_mem_fn3_ ## name ## _base_t {}; \
	static constexpr bool value = sizeof( has_mem_fn3_ ## name ## _test< has_mem_fn3_ ## name ## _derived_t >(0) ) \
	                          == sizeof( boost::type_traits::yes_type ); \
	using type = has_mem_fn3_ ## name; \
}; \
\
template< typename T, typename Result, typename T0 > struct has_mem_fn3_helper_ ## name < T, Result ( T0 ) > \
	: std::is_convertible<decltype( std::declval< has_mem_fn3_ ## name ## _derived_t<T> >().name(std::declval<T0>()) ), \
	                      Result>::type {}; \
\
template< typename T, typename Signature, bool bClass > struct has_mem_fn2_ ## name; \
template< typename T, typename Signature > struct has_mem_fn2_ ## name<T,Signature,false> : std::false_type {}; \
template< typename T, typename Signature > struct has_mem_fn2_ ## name<T,Signature,true> : has_mem_fn3_##name<T, Signature> {}; \
\
template< typename T, typename Signature=void > struct has_mem_fn_ ## name : has_mem_fn2_##name<T, Signature,std::is_class<T>::value> {};