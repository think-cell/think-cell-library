#pragma once

#include <boost/mpl/and.hpp>
#include <boost/type_traits/detail/yes_no_type.hpp>

#include <type_traits>
#include <utility>

#define TC_HAS_MEM_FN_XXX_TRAIT_DEF( name )                                                                                   \
                                                                                                                              \
struct has_mem_fn_ ## name ## _base_t { void name( ) { } };                                                                   \
                                                                                                                              \
template< void (has_mem_fn_ ## name ## _base_t::*)( ) > struct has_mem_fn_ ## name ## _detector {                             \
	typedef boost::type_traits::no_type  type;                                                                                \
};                                                                                                                            \
                                                                                                                              \
template< typename T > typename has_mem_fn_ ## name ## _detector< &T::name >::type has_mem_fn_ ## name ## _test(int);         \
                                                                                                                              \
template< typename T > boost::type_traits::yes_type has_mem_fn_ ## name ## _test(...);                                        \
                                                                                                                              \
template< typename T > struct has_mem_fn_ ## name ## _derived_t : T {                                                         \
	using T::name;                                                                                                            \
	struct dummy_result_t {}; /*something that is not convertible to anything*/                                               \
	dummy_result_t name(...) const;                                                                                           \
};                                                                                                                            \
                                                                                                                              \
template< typename T, typename Signature > struct has_mem_fn_ ## name ## _helper;                                             \
                                                                                                                              \
template< typename T, typename Signature = void > struct has_mem_fn_ ## name                                                        \
	: boost::mpl::and_< has_mem_fn_ ## name< T, void >,                                                                       \
	                    has_mem_fn_ ## name ## _helper< T, Signature >                                                        \
	                  >                                                                                                       \
{};                                                                                                                           \
                                                                                                                              \
template< typename T >        struct has_mem_fn_ ## name< T, void > {                                                         \
	struct has_mem_fn_ ## name ## _derived_t : T, has_mem_fn_ ## name ## _base_t {};                                          \
	static const bool value = sizeof( has_mem_fn_ ## name ## _test< has_mem_fn_ ## name ## _derived_t >(0) )                  \
	                          == sizeof( boost::type_traits::yes_type );                                                      \
	typedef has_mem_fn_ ## name type;                                                                                         \
};                                                                                                                            \
                                                                                                                              \
template< typename A, std::size_t N > struct has_mem_fn_ ## name< A[N], void > : std::false_type {};                                  \
/* support array-of-unknown-bounds incomplete type: */ \
template< typename A > struct has_mem_fn_ ## name< A[], void > : std::false_type {};                                  \
template< typename A >           struct has_mem_fn_ ## name< A*, void >   : std::false_type {};                                  \
                                                                                                                              \
template< typename T, typename Result, typename T0 > struct has_mem_fn_ ## name ## _helper< T, Result ( T0 ) >                \
	: std::is_convertible<decltype( std::declval< has_mem_fn_ ## name ## _derived_t<T> >().name(std::declval<T0>()) ),        \
	                      Result>::type {};

