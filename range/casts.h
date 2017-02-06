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

#include "range_defines.h"
#include "functors.h"
#include "tc_move.h"

#include "return_decltype.h"
#include "type_traits.h"

#include <boost/implicit_cast.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/range/algorithm/copy.hpp>

#include <type_traits>
#include <cstring>

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {
	template< typename T >
	struct is_plain_type final
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
	
	template< typename Dst, typename Src >
	using same_cvref_t = typename same_cvref<Dst, Src>::type;

	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// base_cast
	// (cannot be implemented like derived_cast because when deriving protected, derived to base cast is often publicly inaccessible)

	#define BASE_CAST_IMPL(cvref) \
	template< typename Dst > \
	Dst cvref base_cast( typename boost::mpl::identity<Dst>::type cvref t ) noexcept { \
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" ); \
		static_assert(std::is_class< Dst >::value, "" ); \
		return static_cast<Dst cvref >(t); \
	}

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

	/////////////////////////////////////////////
	// derived_cast

	template< typename To, typename From >
	same_cvref_t< To, From&&> derived_cast( From&& t ) noexcept {
		static_assert( std::is_base_of<std::remove_reference_t<From>, To>::value, "derived_cast is for downcasts only.");
		return static_cast< same_cvref_t< To, From&&> >(t);
	}

	template< typename To, typename From >
	same_cvref_t< To, From*> derived_cast( From* pt ) noexcept {
		static_assert( std::is_base_of<std::remove_pointer_t<From>, To>::value, "derived_cast is for downcasts only.");
		return static_cast< same_cvref_t< To, From*> >(pt);
	}

	template< typename To, typename From >
	same_cvref_t< To, From&&> derived_or_base_cast( From&& t ) noexcept {
		return static_cast< same_cvref_t< To, From&&> >(t);
	}

	template< typename To, typename From >
	same_cvref_t< To, From*> derived_or_base_cast( From* pt ) noexcept {
		return static_cast< same_cvref_t< To, From*> >(pt);
	}

	DEFINE_FN2_TMPL( derived_cast, (typename) );

	//-------------------------------------------------------------------------------------------------------------------------

	/////////////////////////////////////////////
	// bit_cast

	struct any_ptr final {
	private:
		void* m_pv;
	public:
		any_ptr( void* pv ) noexcept 
		:	m_pv(pv)
		{}
		template<typename T> operator T() const& noexcept {
			static_assert(sizeof(T)==sizeof(void*), "" );
			static_assert( std::is_pointer<T>::value || std::is_member_pointer<T>::value, "" );
			T t;
			std::memcpy( std::addressof(t), std::addressof(m_pv), sizeof(t) ); // bit_cast to allow cast to member function pointers
			return t;
		}
	};

	struct any_const_ptr final {
	private:
		void const* m_pv;
	public:
		any_const_ptr( void const* pv ) noexcept 
		:	m_pv(pv)
		{}
		template<typename T> operator T const*() const& noexcept {
			return static_cast<T const*>(m_pv);
		}
	};

	namespace aliasing_ref_adl_barrier {
		template <typename T>
		struct type {
		private:
			same_cvref_t<unsigned char,T*> m_pb;
		public:
			explicit type(same_cvref_t<unsigned char,T*> pb) noexcept
			: m_pb(pb)
			{}
			operator std::remove_cv_t<T>() const& noexcept {
				std::remove_cv_t<T> t;
				std::memcpy( std::addressof(t), m_pb, sizeof(t) );
				return t;
			}
			type const& operator=( std::remove_cv_t<T> const& rhs ) const& noexcept {
				boost::copy( tc::as_blob(rhs), m_pb );
				return *this;
			}
		};
	}

	template<typename T>
	struct aliasing_ref final {
		static_assert( !std::is_reference<T>::value, "" );
		static_assert( std::is_trivially_copyable<T>::value, "" );
		using type=aliasing_ref_adl_barrier::type<T>;
		static type construct(same_cvref_t<unsigned char,T*> pb) noexcept {
			return type(pb);
		}
	};

	template <typename T>
	struct decay<aliasing_ref_adl_barrier::type<T>> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct aliasing_ref<T const> final {
		static_assert( !std::is_reference<T>::value, "" );
		static_assert( std::is_trivially_copyable<T>::value, "" );
		using type = std::remove_cv_t<T>;
		static type construct(same_cvref_t<unsigned char,T const*> pb) noexcept {
			type t;
			std::memcpy( std::addressof(t), pb, sizeof(t) );
			return t;
		}
	};

	template< typename T, typename Enable=void >
	struct aliasing_ptr;
	
	template< typename T >
	struct aliasing_ptr<T,std::enable_if_t<!std::is_function<T>::value>> final {
		static_assert( !std::is_reference<T>::value, "" );
		static_assert( std::is_trivially_copyable<T>::value, "" );
		struct type {
		private:
			same_cvref_t<unsigned char,T*> m_pb;
		public:
			type() noexcept {}
			explicit type(T* pt) noexcept
			: m_pb(reinterpret_cast<same_cvref_t<unsigned char,T*>>(pt))
			{}
			explicit operator bool() const& noexcept {
				return m_pb;
			}
			type& operator=(std::nullptr_t) & noexcept {
				m_pb=nullptr;
				return *this;
			}
			typename aliasing_ref<T>::type operator*() const& noexcept {
				return aliasing_ref<T>::construct(m_pb);
			}
			type& operator+=( std::ptrdiff_t n ) & noexcept {
				m_pb+=n*sizeof(T);
				return *this;
			}
			type& operator-=( std::ptrdiff_t n ) & noexcept {
				return *this+=-n;
			}
			template< typename Offset > friend type operator+( type ptr, Offset n ) noexcept {
				return ptr+=n;
			}
			template< typename Offset > friend type operator-( type ptr, Offset n ) noexcept {
				return ptr-=n;
			}
			typename aliasing_ref<T>::type operator[]( std::ptrdiff_t n ) const& noexcept {
				return *(*this+n);
			}
		};
	};

	template< typename T >
	struct aliasing_ptr<T,std::enable_if_t<std::is_function<T>::value>> final {
		using type = T*;
	};

	template<>
	struct aliasing_ptr<char> final {
		using type = char*;
	};

	template<>
	struct aliasing_ptr<char const> final {
		using type = char const*;
	};

	template<>
	struct aliasing_ptr<unsigned char> final {
		using type = unsigned char*;
	};

	template<>
	struct aliasing_ptr<unsigned char const> final {
		using type = unsigned char const*;
	};

	// no danger of aliasing because Src is not a pointer:
	template< typename Dst, typename Src, std::enable_if_t<!(
		std::is_pointer<Src>::value && std::is_pointer<Dst>::value
	)>* = nullptr>
	Dst bit_cast( Src const& src ) noexcept {
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
	template< typename Dst, typename Src, std::enable_if_t<
		std::is_pointer<Src>::value && std::is_pointer<Dst>::value
	>* = nullptr>
	typename aliasing_ptr< std::remove_pointer_t<Dst> >::type bit_cast( Src const& src ) noexcept {
		static_assert( std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		return typename aliasing_ptr< std::remove_pointer_t<Dst> >::type(reinterpret_cast<Dst>(src));
	}

	template< typename T, std::enable_if_t<tc::is_char<T>::value>* =nullptr>
	auto unsigned_char_cast( T t ) noexcept
		return_decltype( static_cast<std::make_unsigned_t<T>>(t) )

	// gcc (4.8.3) does not like the string literals inside _ASSERTE so:
	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* =nullptr>
	auto unsigned_cast(T t) noexcept code_return_decltype( 
		_ASSERT( 0<=t );,
		static_cast<std::make_unsigned_t<T>>(t)
	)

	// gcc (4.8.3) does not like the string literals inside _ASSERTE so:
	template< typename T, std::enable_if_t<tc::is_actual_integer<T>::value>* =nullptr>
	std::make_signed_t<T> signed_cast(T t) noexcept {
		_ASSERT( std::is_signed<T>::value || t<=tc::unsigned_cast( std::numeric_limits<std::make_signed_t<T>>::max() ) );
		return static_cast<std::make_signed_t<T>>(t);
	}

	#pragma warning( push )
	#pragma warning( disable: 4180 ) // qualifier applied to function type has no meaning; ignored

		template< typename T >
		T const& as_const(T& t) noexcept { // intention is to avoid side-effects
			return static_cast<T const&>(t);
		}
		template <typename T>
		T&& as_const(T&& t) noexcept { // needed in generic code when both values and references can occur
			static_assert(!std::is_lvalue_reference<T&&>::value, "");
			return std::forward<T&&>(t);
		}

		template< typename T >
		std::remove_const_t<T>& as_mutable(T& t) noexcept {
			return const_cast<std::remove_const_t<T>&>(t);
		}

	#pragma warning( pop )

	template< typename T >
	T const* make_const_ptr( T const* pt ) noexcept {
		return pt;
	}

	template< typename T >
	T* make_mutable_ptr( T const* pt ) noexcept {
		return const_cast<T*>(pt);
	}

	template<typename Func>
	struct make_arg_mutable_impl final {
	private:
		tc::decay_t<Func> m_func;
	public:
		make_arg_mutable_impl(Func&& func) noexcept : m_func(std::forward<Func>(func)) {}
		template<typename T> auto operator()(T const& t) const& MAYTHROW return_decltype( m_func( as_mutable(t) ) )
	};

	template<typename Func>
	auto make_arg_mutable(Func&& func) noexcept return_decltype( make_arg_mutable_impl<Func>( std::forward<Func>(func) ) )

	/////////////////////////////////////////////
	// void_cast

	template<typename Dst, typename Src>
	Dst* void_cast(Src* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst*>(p);
	}

	template<typename Dst, typename Src>
	Dst const* void_cast(Src const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst const*>(p);
	}

	template<typename Dst, typename Src>
	Dst volatile* void_cast(Src volatile* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst volatile*>(p);
	}

	template<typename Dst, typename Src>
	Dst volatile const* void_cast(Src volatile const* p) noexcept{
		static_assert(std::is_void<Src>::value,"Src must be possibly qualified void*");
		static_assert(std::is_same< tc::remove_cvref_t<Dst>, Dst >::value, "" );
		// static_assert(!std::is_void<Dst>::value,""); // practical for generic code to allow it
		return static_cast<Dst volatile const*>(p);
	}

	/////////////////////////////////////////////
	// reluctant_implicit_cast
	// Returns a reference to its argument whenever possible, otherwise performs an implicit conversion.

	template<typename TTarget, typename TSource>
	std::conditional_t<
		tc::is_base_of_decayed<TTarget, TSource>::value,
		TSource&&,
		TTarget
	> reluctant_implicit_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	/////////////////////////////////////////////
	// reluctant_static_cast
	// Returns a reference to its argument whenever possible, otherwise performs an explicit conversion.

	template<typename TTarget, typename TSource, std::enable_if_t<tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TSource&& reluctant_static_cast(TSource&& src) noexcept {
		return std::forward<TSource>(src);
	}

	template<typename TTarget, typename TSource, std::enable_if_t<!tc::is_base_of_decayed<TTarget, TSource>::value>* = nullptr>
	TTarget reluctant_static_cast(TSource&& src) noexcept {
		return static_cast<TTarget>(std::forward<TSource>(src));
	}
}

