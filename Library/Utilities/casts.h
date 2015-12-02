#pragma once

#include "Library/ErrorReporting/functors.h"
#include "Library/ErrorReporting/tc_move.h"

#include "return_decltype.h"
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
		: std::is_same< T, std::remove_pointer_t< tc::remove_cvref_t<T> > >::type
	{};

	template<typename Dst, typename Src>
	struct same_cvref;

	#define SAME_CVREF_IMPL(cvref) \
	template<typename Dst, typename Src> \
	struct same_cvref<Dst, Src cvref> { \
		static_assert( is_plain_type<Dst>::value, "use non-cv-qualified non-reference type" ); \
		using type = Dst cvref; \
	};

	SAME_CVREF_IMPL(&)
	SAME_CVREF_IMPL(&&)
	SAME_CVREF_IMPL(*)
	SAME_CVREF_IMPL(const&)
	SAME_CVREF_IMPL(const&&)
	SAME_CVREF_IMPL(const*)
	SAME_CVREF_IMPL(volatile&)
	SAME_CVREF_IMPL(volatile&&)
	SAME_CVREF_IMPL(volatile*)
	SAME_CVREF_IMPL(volatile const&)
	SAME_CVREF_IMPL(volatile const&&)
	SAME_CVREF_IMPL(volatile const*)

	#undef SAME_CVREF_IMPL

	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// base_cast
	// (cannot be implemented like derived_cast because when deriving protected, derived to base cast is often publicly inaccessible)

	#define BASE_CAST_IMPL(cvref) \
	template< typename Dst > \
	Dst cvref base_cast( typename boost::mpl::identity<Dst>::type cvref t ) { \
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" ); \
		static_assert(std::is_class< Dst >::value, "" ); \
		return static_cast<Dst cvref >(t); \
	};

	BASE_CAST_IMPL(&)
	BASE_CAST_IMPL(&&)
	BASE_CAST_IMPL(*)
	BASE_CAST_IMPL(const&)
	BASE_CAST_IMPL(const&&)
	BASE_CAST_IMPL(const*)
	BASE_CAST_IMPL(volatile&)
	BASE_CAST_IMPL(volatile&&)
	BASE_CAST_IMPL(volatile*)
	BASE_CAST_IMPL(volatile const&)
	BASE_CAST_IMPL(volatile const&&)
	BASE_CAST_IMPL(volatile const*)

	#undef BASE_CAST_IMPL

	template< typename Derived, typename Base, typename T >
	std::conditional_t< is_base_of<Derived, tc::remove_cvref_t<T> >::value, typename same_cvref< Base, T&& >::type, T&& > ctor_base_cast(T&& t) {
		static_assert( is_plain_type<Derived>::value, "" );
		static_assert( is_plain_type<Base>::value, "" );
		static_assert( std::is_base_of<Base, Derived>::value, "");
		return std::forward<T>(t);
	};

	/////////////////////////////////////////////
	// derived_cast

	template< typename To, typename From >
	typename same_cvref< To, From&&>::type derived_cast( From&& t ) {
		static_assert( std::is_base_of<std::remove_reference_t<From>, To>::value, "derived_cast is for downcasts only.");
		return static_cast<typename same_cvref< To, From&&>::type >(t);
	};

	template< typename To, typename From >
	typename same_cvref< To, From*>::type derived_cast( From* pt ) {
		static_assert( std::is_base_of<std::remove_pointer_t<From>, To>::value, "derived_cast is for downcasts only.");
		return static_cast<typename same_cvref< To, From*>::type >(pt);
	};

	template< typename To, typename From >
	typename same_cvref< To, From&&>::type derived_or_base_cast( From&& t ) {
		return static_cast<typename same_cvref< To, From&&>::type >(t);
	};

	template< typename To, typename From >
	typename same_cvref< To, From*>::type derived_or_base_cast( From* pt ) {
		return static_cast<typename same_cvref< To, From*>::type >(pt);
	};

    DEFINE_FN2_TMPL( derived_cast, (typename) );

	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// bit_cast

	struct any_ptr {
	private:
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

	struct any_const_ptr {
	private:
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
		static_assert( std::is_trivially_copyable<T>::value, "" );
		struct type {
		private:
			typename same_cvref<unsigned char,T*>::type m_pb;
		public:
			explicit type(typename same_cvref<unsigned char,T*>::type pb)
			: m_pb(pb)
			{}
			operator std::remove_cv_t<T>() const {
				std::remove_cv_t<T> t;
				std::memcpy( std::addressof(t), m_pb, sizeof(t) );
				return t;
			}
			type& operator=( std::remove_cv_t<T> const& rhs ) {
				boost::copy( tc::as_blob(rhs), m_pb );
				return *this;
			}
		};
		static type construct(typename same_cvref<unsigned char,T*>::type pb) {
			return type(pb);
		}
	};

	template<typename T>
	struct aliasing_ref<T const> {
		static_assert( !std::is_reference<T>::value, "" );
		static_assert( std::is_trivially_copyable<T>::value, "" );
		using type = std::remove_cv_t<T>;
		static type construct(typename same_cvref<unsigned char,T const*>::type pb) {
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
		static_assert( std::is_trivially_copyable<T>::value, "" );
		struct type {
		private:
			typename same_cvref<unsigned char,T*>::type m_pb;
		public:
			type() {}
			explicit type(T* pt)
			: m_pb(reinterpret_cast<typename same_cvref<unsigned char,T*>::type>(pt))
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
		using type = T*;
	};

	template<>
	struct aliasing_ptr<char> {
		using type = char*;
	};

	template<>
	struct aliasing_ptr<char const> {
		using type = char const*;
	};

	template<>
	struct aliasing_ptr<unsigned char> {
		using type = unsigned char*;
	};

	template<>
	struct aliasing_ptr<unsigned char const> {
		using type = unsigned char const*;
	};

	// no danger of aliasing because Src is not a pointer:
	template< typename Dst, typename Src >
	typename std::enable_if<!(
		std::is_pointer<Src>::value && std::is_pointer<Dst>::value
	), Dst >::type bit_cast( Src const& src ) {
		static_assert( std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		static_assert(sizeof(Dst)==sizeof(Src),"bit_cast source and destination must be same size");
		static_assert(
			std::is_trivially_copyable<Dst>::value && std::is_trivially_copyable<Src>::value ||
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
	, typename aliasing_ptr< std::remove_pointer_t<Dst> >::type >::type bit_cast( Src const& src ) {
		static_assert( std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		return typename aliasing_ptr< std::remove_pointer_t<Dst> >::type(reinterpret_cast<Dst>(src));
	}

	template< typename T >
	auto unsigned_modulo_cast( T t )
		return_decltype( static_cast<std::make_unsigned_t<T>>(t) )

	// gcc (4.8.3) does not like the string literals inside _ASSERTE so:
	template< typename T >
	std::make_unsigned_t<T> unsigned_cast(T t) {
		_ASSERT( 0<=t );
		return tc::unsigned_modulo_cast(t);
	}

	// gcc (4.8.3) does not like the string literals inside _ASSERTE so:
	template< typename T >
	std::make_signed_t<T> signed_cast(T t) {
		_ASSERT( std::is_signed<T>::value || t<=tc::unsigned_cast( std::numeric_limits<std::make_signed_t<T>>::max() ) );
		return static_cast<std::make_signed_t<T>>(t);
	}

	#pragma warning( push )
	#pragma warning( disable: 4180 ) // qualifier applied to function type has no meaning; ignored

		template< typename T >
		std::remove_reference_t<T> const& make_const(T&& t) { 
			static_assert(std::is_lvalue_reference<T>::value, "");
			return static_cast<std::remove_reference_t<T> const&>(t);
		};
		template< typename T >
		std::remove_const_t< std::remove_reference_t<T> > & make_mutable(T&& t) {
			static_assert(std::is_lvalue_reference<T>::value, "");
			return const_cast<std::remove_const_t< std::remove_reference_t<T> > &>(t);
		};

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
		std::decay_t<Func> m_func;
	public:
		make_arg_mutable_impl(Func&& func) : m_func(std::forward<Func>(func)) {}
		template<typename T> auto operator()(T const& t) const return_decltype( m_func( make_mutable(t) ) )
	};

	template<typename Func>
	auto make_arg_mutable(Func&& func) return_decltype( make_arg_mutable_impl<Func>( std::forward<Func>(func) ) )

	/////////////////////////////////////////////
	// void_cast

	template<typename Dst, typename Src>
	Dst* void_cast(Src* p){
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst*>(p);
	}

	template<typename Dst, typename Src>
	Dst const* void_cast(Src const* p){
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst const*>(p);
	}

	template<typename Dst, typename Src>
	Dst volatile* void_cast(Src volatile* p){
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst volatile*>(p);
	}

	template<typename Dst, typename Src>
	Dst volatile const* void_cast(Src volatile const* p){
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst volatile const*>(p);
	}
}

