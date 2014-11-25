#pragma once

#include "Library/ErrorReporting/functors.h"
#include "Library/ErrorReporting/tc_move.h"

#include "decltype_return.h"
#include "remove_cvref.h"
#include "conversion_traits.h"

#include <boost/implicit_cast.hpp>

#include <type_traits>
#include <cstring>
#include <boost/mpl/identity.hpp>

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {
	template< typename T >
	struct is_plain_type
		: std::is_same< T, typename std::remove_pointer< typename remove_cvref<T>::type >::type >::type
	{};

	template<typename Dst, typename Src>
	struct same_cvref;

	#define SAME_CVREF_IMPL(cvref) \
	template<typename Dst, typename Src> \
	struct same_cvref<Dst, Src cvref> { \
		static_assert( is_plain_type<Dst>::value, "" ); \
		typedef Dst cvref type; \
	};

	SAME_CVREF_IMPL(&)
	SAME_CVREF_IMPL(&&)
	SAME_CVREF_IMPL(*)
	SAME_CVREF_IMPL(const &)
	SAME_CVREF_IMPL(const &&)
	SAME_CVREF_IMPL(const *)
	SAME_CVREF_IMPL(volatile &)
	SAME_CVREF_IMPL(volatile &&)
	SAME_CVREF_IMPL(volatile *)
	SAME_CVREF_IMPL(volatile const &)
	SAME_CVREF_IMPL(volatile const &&)
	SAME_CVREF_IMPL(volatile const *)

	#undef SAME_CVREF_IMPL

	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// base_cast
	// (cannot be implemented like derived_cast because when deriving protected, derived to base cast is often publicly inaccessible)

	#define BASE_CAST_IMPL(cvref)                                                                                             \
	template< typename T >                                                                                                    \
	T cvref base_cast( typename boost::mpl::identity<T>::type cvref t ) {                                                     \
		return static_cast<T cvref >(t);                                                                                      \
	};

	BASE_CAST_IMPL(&)
	BASE_CAST_IMPL(&&)
	BASE_CAST_IMPL(*)
	BASE_CAST_IMPL(const &)
	BASE_CAST_IMPL(const &&)
	BASE_CAST_IMPL(const *)
	BASE_CAST_IMPL(volatile &)
	BASE_CAST_IMPL(volatile &&)
	BASE_CAST_IMPL(volatile *)
	BASE_CAST_IMPL(volatile const &)
	BASE_CAST_IMPL(volatile const &&)
	BASE_CAST_IMPL(volatile const *)

	#undef BASE_CAST_IMPL

	template< typename Derived, typename Base, typename T >
	typename std::conditional< is_base_of<Derived, typename remove_cvref<T>::type>::value, typename same_cvref< Base, T&& >::type, T&& >::type ctor_base_cast( T&& t ) {
		static_assert( is_plain_type<Derived>::value, "" );
		static_assert( is_plain_type<Base>::value, "" );
		static_assert( std::is_base_of<Base, Derived>::value, "");
		return std::forward<T>(t);
	};

	/////////////////////////////////////////////
	// derived_cast

#if defined _MSC_VER && _MSC_VER <= 1800

	template< typename To, typename From >
	typename same_cvref< To, From&>::type derived_cast( From& t ) {
		static_assert( std::is_base_of<typename std::remove_reference<From>::type, To>::value, "derived_cast is for downcasts only.");
		return static_cast<typename same_cvref< To, From&>::type >(t);
	};

	template< typename To, typename From >
	typename same_cvref< To, From&&>::type derived_cast( From&& t ) {
		static_assert( std::is_base_of<typename std::remove_reference<From>::type, To>::value, "derived_cast is for downcasts only.");
		return tc_move_always(static_cast<typename same_cvref< To BOOST_PP_COMMA() From&>::type >(t));
	};

#else

	template< typename To, typename From >
	typename same_cvref< To, From&&>::type derived_cast( From&& t ) {
		static_assert( std::is_base_of<typename std::remove_reference<From>::type, To>::value, "derived_cast is for downcasts only.");
		return static_cast<typename same_cvref< To, From&&>::type >(t);
	};

#endif


	template< typename To, typename From >
	typename same_cvref< To, From*>::type derived_cast( From* pt ) {
		static_assert( std::is_base_of<typename std::remove_pointer<From>::type, To>::value, "derived_cast is for downcasts only.");
		return static_cast<typename same_cvref< To, From*>::type >(pt);
	};

#if defined _MSC_VER && _MSC_VER <= 1800

	template< typename To, typename From >
	typename same_cvref< To, From&>::type derived_or_base_cast( From& t ) {
		return static_cast<typename same_cvref< To, From&>::type >(t);
	};

	template< typename To, typename From >
	typename same_cvref< To, From&&>::type derived_or_base_cast( From&& t ) {
		return tc_move_always(static_cast<typename same_cvref< To, From&>::type >(t));
	};

#else

	template< typename To, typename From >
	typename same_cvref< To, From&&>::type derived_or_base_cast( From&& t ) {
		return static_cast<typename same_cvref< To, From&&>::type >(t);
	};

#endif

	template< typename To, typename From >
	typename same_cvref< To, From*>::type derived_or_base_cast( From* pt ) {
		return static_cast<typename same_cvref< To, From*>::type >(pt);
	};

    DEFINE_FN2_TMPL( derived_cast, (typename) );

	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// enum_cast

	template< typename Enum, typename T >
	Enum enum_cast( T const& t ) {
		static_assert( std::is_enum<Enum>::value, "" ); 
		static_assert( !std::is_enum<T>::value, "Use enum_cast only for upcasts from integer to enum. For casting between enums, use Convert." ); 
		return static_cast<Enum>( boost::implicit_cast< typename std::underlying_type<Enum>::type >(t) );
	}

	/////////////////////////////////////////////
	// bit_cast

	class any_ptr {
		void* m_pv;
	public:
		any_ptr( void* pv ) 
		:	m_pv(pv)
		{}
		template<typename T> operator T() const {
			static_assert(sizeof(T)==sizeof(void*), "" );
			static_assert( std::is_pointer<T>::value || std::is_member_pointer<T>::value, "" );
			T t;
			std::memcpy( std::addressof(t), std::addressof(m_pv), sizeof(t) ); // bit_cast to allow cast to member function pointers
			return t;
		}
	};

	class any_const_ptr {
		void const* m_pv;
	public:
		any_const_ptr( void const* pv ) 
		:	m_pv(pv)
		{}
		template<typename T> operator T const*() const {
			return static_cast<T const*>(m_pv);
		}
	};

	template<typename T>
	struct aliasing_ref {
		static_assert( !std::is_reference<T>::value, "" );
		static_assert( std::is_pod<T>::value, "" );
		class type {
			typename same_cvref<char,T*>::type m_pb;
		public:
			explicit type(typename same_cvref<char,T*>::type pb)
			: m_pb(pb)
			{}
			operator typename std::remove_cv<T>::type() const {
				typename std::remove_cv<T>::type t;
				std::memcpy( std::addressof(t), m_pb, sizeof(t) );
				return t;
			}
			type& operator=( typename std::remove_cv<T>::type const& rhs ) {
				std::memcpy( m_pb, std::addressof(rhs), sizeof(T) );
				return *this;
			}
		};
		static type construct(typename same_cvref<char,T*>::type pb) {
			return type(pb);
		}
	};

	template<typename T>
	struct aliasing_ref<T const> {
		static_assert( !std::is_reference<T>::value, "" );
		static_assert( std::is_pod<T>::value, "" );
		typedef typename std::remove_cv<T>::type type;
		static type construct(typename same_cvref<char,T const*>::type pb) {
			type t;
			std::memcpy( std::addressof(t), pb, sizeof(t) );
			return t;
		}
	};

	template< typename T, typename Enable=void >
	struct aliasing_ptr;
	
	template< typename T >
	struct aliasing_ptr<T,typename std::enable_if<!std::is_function<T>::value>::type> {
		static_assert( !std::is_reference<T>::value, "" );
		static_assert( std::is_pod<T>::value, "" );
		class type {
			typename same_cvref<char,T*>::type m_pb;
		public:
			type() {}
			explicit type(T* pt)
			: m_pb(reinterpret_cast<typename same_cvref<char,T*>::type>(pt))
			{}
			explicit operator bool() const {
				return m_pb;
			}
			type& operator=(std::nullptr_t) {
				m_pb=nullptr;
				return *this;
			}
			typename aliasing_ref<T>::type operator*() const {
				return aliasing_ref<T>::construct(m_pb);
			}
			type& operator+=( std::ptrdiff_t n ) {
				m_pb+=n*sizeof(T);
				return *this;
			}
			type& operator-=( std::ptrdiff_t n ) {
				return *this+=-n;
			}
			template< typename Offset > friend type operator+( type ptr, Offset n ) {
				return ptr+=n;
			}
			template< typename Offset > friend type operator-( type ptr, Offset n ) {
				return ptr-=n;
			}
			typename aliasing_ref<T>::type operator[]( std::ptrdiff_t n ) const {
				return *(*this+n);
			}
		};
	};

	template< typename T >
	struct aliasing_ptr<T,typename std::enable_if<std::is_function<T>::value>::type> {
		typedef T* type;
	};

	template<>
	struct aliasing_ptr<char> {
		typedef char* type;
	};

	template<>
	struct aliasing_ptr<char const> {
		typedef char const* type;
	};

	template<>
	struct aliasing_ptr<unsigned char> {
		typedef unsigned char* type;
	};

	template<>
	struct aliasing_ptr<unsigned char const> {
		typedef unsigned char const* type;
	};

	// no danger of aliasing because Src is not a pointer:
	template< typename Dst, typename Src >
	typename std::enable_if<!(
		std::is_pointer<Src>::value && std::is_pointer<Dst>::value
	), Dst >::type bit_cast( Src const& src ) {
		static_assert( std::is_same< typename tc::remove_cvref<Dst>::type, Dst >::value, "" );
		static_assert(sizeof(Dst)==sizeof(Src),"bit_cast source and destination must be same size");
		static_assert(
			std::is_pod<Dst>::value && std::is_pod<Src>::value || 
			std::is_member_function_pointer<Dst>::value && std::is_same<Src,void const*>::value ||
			std::is_same<Dst,void const*>::value && std::is_member_function_pointer<Src>::value
		, "" );
		Dst dst;
		std::memcpy(std::addressof(dst), std::addressof(src), sizeof(dst));
		return dst;
	}

	// danger of aliasing because Src is a pointer:
	template< typename Dst, typename Src >
	typename std::enable_if<
		std::is_pointer<Src>::value && std::is_pointer<Dst>::value
	, typename aliasing_ptr< typename std::remove_pointer<Dst>::type >::type >::type bit_cast( Src const& src ) {
		static_assert( std::is_same< typename tc::remove_cvref<Dst>::type, Dst >::value, "" );
		return typename aliasing_ptr< typename std::remove_pointer<Dst>::type >::type(reinterpret_cast<Dst>(src));
	}

	template< typename T >
	auto make_unsigned_modulo( T t )
		return_decltype( static_cast<typename std::make_unsigned<T>::type>(t) )

#ifndef __GNUC__
	template< typename T >
	auto make_unsigned( T t )
		return_decltype( _ASSERTE( 0<=t ), make_unsigned_modulo(t) )
#else 
	// gcc (4.8.3) does not like the string literals inside _ASSERTE so:
	template< typename T >
	auto make_unsigned(T t) -> decltype(static_cast<typename std::make_unsigned<T>::type>(t)) {
		_ASSERTE( 0<=t );
		return make_unsigned_modulo(t);
	}
#endif

	#pragma warning( push )
	#pragma warning( disable: 4180 ) // qualifier applied to function type has no meaning; ignored

	#define MAKE_CONST_IMPL(vref) \
		template< typename T > \
		T const vref make_const( T const vref t ) { \
			return static_cast<T const vref >(t); \
		}; \
		template< typename T > \
		T vref make_mutable( T const vref t ) { \
			return const_cast<T vref >(t); \
		};

	MAKE_CONST_IMPL(&)
	MAKE_CONST_IMPL(&&)
	MAKE_CONST_IMPL(volatile &)
	MAKE_CONST_IMPL(volatile &&)

	#undef MAKE_CONST_IMPL

	#pragma warning( pop )

	template< typename T >
	T const* make_const_ptr( T const* pt ) {
		return pt;
	}

	template< typename T >
	T* make_mutable_ptr( T const* pt ) {
		return const_cast<T*>(pt);
	}

	template<typename Func>
	struct make_arg_mutable_impl {
	private:
		typename std::decay<Func>::type m_func;
	public:
		make_arg_mutable_impl(Func && func) : m_func(std::forward<Func>(func)) {}
		template<typename T> auto operator()(T const& t) const return_decltype( m_func( make_mutable(t) ) )
	};

	template<typename Func>
	auto make_arg_mutable(Func && func) return_decltype( make_arg_mutable_impl<Func>( std::forward<Func>(func) ) )
}

