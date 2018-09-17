
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "casts.h"
#include "sub_range.h"
#include <boost/range/algorithm/copy.hpp>

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {
	/////////////////////////////////////////////
	// bit_cast

	struct any_ptr final {
	private:
		void* m_pv;
	public:
		any_ptr( void* pv ) noexcept
		:	m_pv(pv)
		{}

		template<typename T, std::enable_if_t<
			std::is_pointer<T>::value || std::is_member_pointer<T>::value
		>* = nullptr> operator T() const& noexcept {
			static_assert(sizeof(T)==sizeof(void*));
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

	namespace no_adl {
		template <typename T>
		struct type {
		private:
			same_cvref_t<unsigned char,T>* m_pb;
		public:
			explicit type(same_cvref_t<unsigned char,T>* pb) noexcept
			: m_pb(pb)
			{}
			explicit type(tc::any_ptr p) noexcept
			: m_pb(p)
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
		static_assert( !std::is_reference<T>::value );
		static_assert( std::is_trivially_copyable<T>::value );
		using type=no_adl::type<T>;
		static type construct(same_cvref_t<unsigned char,T>* pb) noexcept {
			return type(pb);
		}
	};

	template <typename T>
	struct decay<no_adl::type<T>> {
		using type=typename tc::decay<T>::type; // recursive
	};

	template<typename T>
	struct aliasing_ref<T const> final {
		static_assert( !std::is_reference<T>::value );
		static_assert( std::is_trivially_copyable<T>::value );
		using type = std::remove_cv_t<T>;
		static type construct(same_cvref_t<unsigned char,T const>* pb) noexcept {
			type t;
			std::memcpy( std::addressof(t), pb, sizeof(t) );
			return t;
		}
	};

	template< typename T, typename Enable=void >
	struct aliasing_ptr;

	template< typename T >
	struct aliasing_ptr<T,std::enable_if_t<!std::is_function<T>::value>> final {
		static_assert( !std::is_reference<T>::value );
		static_assert( std::is_trivially_copyable<T>::value );
		struct type {
		private:
			same_cvref_t<unsigned char,T>* m_pb;
		public:
			type() noexcept {}
			explicit type(T* pt) noexcept
			: m_pb(reinterpret_cast<same_cvref_t<unsigned char,T>*>(pt))
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
		static_assert( std::is_same< tc::remove_cvref_t<Dst>, Dst >::value );
		static_assert(sizeof(Dst)==sizeof(Src),"bit_cast source and destination must be same size");
		static_assert(
			(std::is_trivially_copyable<Dst>::value && std::is_trivially_copyable<Src>::value) ||
			(std::is_member_function_pointer<Dst>::value && std::is_same<Src,void const*>::value) ||
			(std::is_same<Dst,void const*>::value && std::is_member_function_pointer<Src>::value)
		);
		Dst dst;
		std::memcpy(std::addressof(dst), std::addressof(src), sizeof(dst));
		return dst;
	}

	// danger of aliasing because Src is a pointer:
	template< typename Dst, typename Src, std::enable_if_t<
		std::is_pointer<Src>::value && std::is_pointer<Dst>::value
	>* = nullptr>
	typename aliasing_ptr< std::remove_pointer_t<Dst> >::type bit_cast( Src const& src ) noexcept {
		static_assert( std::is_same< tc::remove_cvref_t<Dst>, Dst >::value );
		return typename aliasing_ptr< std::remove_pointer_t<Dst> >::type(reinterpret_cast<Dst>(src));
	}
}

