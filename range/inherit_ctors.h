#pragma once


#define INHERIT_CTORS( Derived, Base ) \
	template< typename T0, std::enable_if_t< \
		( std::is_same< Base, std::decay_t<T0> >::value || \
		!std::is_base_of< Base, std::decay_t<T0> >::value ) && \
		std::is_convertible< T0 &&, Base >::value \
	>* =nullptr> \
	Derived( T0&& t0 ) \
	:	Base( std::forward<T0>(t0) ) {} \
	template< typename T0, std::enable_if_t< \
		( std::is_same< Base, std::decay_t<T0> >::value || \
		!std::is_base_of< Base, std::decay_t<T0> >::value ) && \
		!std::is_convertible< T0 &&, Base >::value \
	>* =nullptr> \
	explicit Derived( T0&& t0) \
	:	Base( std::forward<T0>(t0) ) {} \
	template< typename T0, typename T1, typename ...Ts > \
	explicit Derived( T0&& t0, T1&& t1, Ts&& ... ts ) \
	:	Base( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<Ts>(ts)... ) {}

// Cannot test for sizeof(Base)==sizeof(Derived) in enable_if.
// Derived is not a complete type yet and clang does not compile that.
// Visual Studio did compile it, but it never worked correctly: 
// _graphlabelposition had a different size than tc::geo::point<int> (because
// empty base class optimization failed) but the inherited operator=
// was used nonetheless.
#define INHERIT_ASSIGN( Derived, Base ) \
	template<typename T> \
	typename std::enable_if< \
		( std::is_same< Base, std::decay_t<T> >::value || \
		!std::is_base_of< Base, std::decay_t<T> >::value ) \
	, Derived& >::type operator=(T&& t){ \
		static_assert( sizeof(Base)==sizeof(Derived), "move other members?"); \
		Base::operator=( std::forward<T>(t) ); \
		return *this; \
	}

#define INHERIT_CTORS_ASSIGN( Derived, Base ) \
	INHERIT_CTORS( Derived, Base ) \
	INHERIT_ASSIGN( Derived, Base )
