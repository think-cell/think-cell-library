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
template< typename T, typename Signature > struct has_mem_fn3_ ## name ## _helper;                                             \
                                                                                                                              \
template< typename T, typename Signature = void > struct has_mem_fn3_ ## name : std::integral_constant< bool,                  \
	has_mem_fn3_ ## name< T, void >::value && has_mem_fn3_ ## name ## _helper< T, Signature >::value                            \
> {};                                                                                                                         \
                                                                                                                              \
template< typename T >        struct has_mem_fn3_ ## name< T, void > {                                                         \
	struct has_mem_fn3_ ## name ## _derived_t : T, has_mem_fn3_ ## name ## _base_t {};                                          \
	static bool const value = sizeof( has_mem_fn3_ ## name ## _test< has_mem_fn3_ ## name ## _derived_t >(0) )                  \
	                          == sizeof( boost::type_traits::yes_type );                                                      \
	using type = has_mem_fn3_ ## name;                                                                                         \
};                                                                                                                            \
                                                                                                                              \
template< typename T, typename Result, typename T0 > struct has_mem_fn3_ ## name ## _helper< T, Result ( T0 ) >                \
	: std::is_convertible<decltype( std::declval< has_mem_fn3_ ## name ## _derived_t<T> >().name(std::declval<T0>()) ),        \
	                      Result>::type {}; \
\
template< typename T, typename Signature, bool bClass > struct has_mem_fn2_ ## name; \
template< typename T, typename Signature > struct has_mem_fn2_ ## name<T,Signature,false> : std::false_type {}; \
template< typename T, typename Signature > struct has_mem_fn2_ ## name<T,Signature,true> : has_mem_fn3_##name<T, Signature> {}; \
\
template< typename T, typename Signature=void > struct has_mem_fn_ ## name : has_mem_fn2_##name<T, Signature,std::is_class<T>::value> {};