#pragma once


#define INHERIT_CTORS( Derived, Base ) \
	template< typename T0 > \
	Derived( T0 && t0, typename std::enable_if< \
		( std::is_same< Base, typename std::decay<T0>::type >::value || \
		!std::is_base_of< Base, typename std::decay<T0>::type >::value ) && \
		std::is_convertible< T0 &&, Base >::value \
	, unused_arg >::type=unused_arg() ) \
	:	Base( std::forward<T0>(t0) ) {} \
	template< typename T0 > \
	explicit Derived( T0 && t0, typename std::enable_if< \
		( std::is_same< Base, typename std::decay<T0>::type >::value || \
		!std::is_base_of< Base, typename std::decay<T0>::type >::value ) && \
		!std::is_convertible< T0 &&, Base >::value \
	, unused_arg >::type=unused_arg() ) \
	:	Base( std::forward<T0>(t0) ) {} \
	template< typename T0, typename T1, typename ...Ts > \
	explicit Derived( T0 && t0, T1 && t1, Ts && ... ts ) \
	:	Base( std::forward<T0>(t0),std::forward<T1>(t1),std::forward<Ts>(ts)... ) {}

// Cannot test for sizeof(Base)==sizeof(Derived) in enable_if.
// Derived is not a complete type yet and clang does not compile that.
// Visual Studio did compile it, but it never worked correctly: 
// _graphlabelposition had a different size than _intPoint (because
// empty base class optimization failed) but the inherited operator=
// was used nonetheless.
#define INHERIT_ASSIGN( Derived, Base ) \
	template<typename T> \
	typename std::enable_if< \
		( std::is_same< Base, typename std::decay<T>::type >::value || \
		!std::is_base_of< Base, typename std::decay<T>::type >::value ) \
	, Derived& >::type operator=( T && t ){ \
		static_assert( sizeof(Base)==sizeof(Derived), "move other members?"); \
		Base::operator=( std::forward<T>(t) ); \
		return *this; \
	}

#define INHERIT_CTORS_ASSIGN( Derived, Base ) \
	INHERIT_CTORS( Derived, Base ) \
	INHERIT_ASSIGN( Derived, Base )

#define DEFAULT_MOVE_CTOR( Derived, Base ) \
	public: \
		Derived( Derived && t ) \
		:	Base(tc::base_cast<Base>(tc_move(t))) { \
			static_assert(sizeof(Derived)==sizeof(Base),"move other members?"); \
			static_assert( !std::is_base_of< tc::nonmovable, Derived >::value, "nonmovable" ); \
		} \
	private: \
		Derived( Derived const& t ); \
		Derived& operator=( Derived && t ); \
		Derived& operator=( Derived const& t ); \
	public:

#define DEFAULT_MOVE_CTOR_AND_ASSIGN( Derived, Base ) \
	public: \
		Derived( Derived && t ) \
		:	Base(tc::base_cast<Base>(tc_move(t))) { \
			static_assert(sizeof(Derived)==sizeof(Base),"move other members?"); \
			static_assert( !std::is_base_of< tc::nonmovable, Derived >::value, "nonmovable" ); \
		} \
		Derived& operator=( Derived && t ) { \
			static_assert(sizeof(Derived)==sizeof(Base),"move-assign other members?"); \
			static_assert( !std::is_base_of< tc::nonmovable, Derived >::value, "nonmovable" ); \
			Base::operator=(tc::base_cast<Base>(tc_move(t))); \
			return *this; \
		} \
	private: \
		Derived( Derived const& t ); \
		Derived& operator=( Derived const& t ); \
	public:

#define NONMOVABLE( Derived, Base ) \
	private: \
		Derived( Derived && t ); \
		Derived( Derived const& t ); \
		Derived& operator=( Derived && t ); \
		Derived& operator=( Derived const& t ); \
	public:
