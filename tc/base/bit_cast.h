
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "casts.h"
#include "../range/subrange.h"
#include <boost/range/algorithm/copy.hpp>

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {
	/////////////////////////////////////////////
	// bit_cast

	namespace any_ptr_adl {
		struct any_ptr final {
		private:
			void* m_pv;
		public:
			any_ptr( void* pv ) noexcept
			:	m_pv(pv)
			{}

			template<typename T> requires std::is_pointer<T>::value || std::is_member_pointer<T>::value
			operator T() const& noexcept {
				STATICASSERTEQUAL(sizeof(T), sizeof(void*));
				T t;
				std::memcpy( std::addressof(t), std::addressof(m_pv), sizeof(t) ); // bit_cast to allow cast to member function pointers
				return t;
			}
		};
	}
	using any_ptr_adl::any_ptr;

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

		template <typename T, bool bPreventSlicing>
		struct decay<no_adl::type<T>, bPreventSlicing> {
			using type=typename tc::decay<T, bPreventSlicing>::type; // recursive
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

	template< typename T>
	struct aliasing_ptr final {
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
				m_pb+=n*static_cast<std::ptrdiff_t>(sizeof(T)); // cast to signed ptrdiff_t to silence UB sanitizer
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

	template< typename T > requires std::is_function<T>::value
	struct aliasing_ptr<T> final {
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

	template<typename Dst>
	[[nodiscard]] Dst bit_cast_range(tc::ptr_range<unsigned char const> src) noexcept {
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		_ASSERTEQUAL(tc::size(src), sizeof(Dst));
		static_assert(std::is_trivially_copyable< Dst >::value);
		Dst dst;
		std::memcpy(std::addressof(dst), tc::ptr_begin(src), sizeof(dst));
		return dst;
	}

	template<typename Dst, typename Src> requires tc::has_ptr_begin<Src>::value
	[[nodiscard]] Dst bit_cast_range(Src const& src) noexcept {
		return tc::bit_cast_range<Dst>(tc::range_as_blob(src));
	}

	// no danger of aliasing
	template<typename Dst, typename Src>
	[[nodiscard]] constexpr Dst bit_cast(Src const& src) noexcept {
		static_assert( !std::is_pointer<Src>::value || !std::is_pointer<Dst>::value );
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst );
		STATICASSERTEQUAL(sizeof(Dst), sizeof(Src), "bit_cast source and destination must be same size");
		static_assert(std::is_trivially_copyable<Dst>::value && std::is_trivially_copyable<Src>::value);
		// Visual Studio use __builtin_bit_cast to implement std::bit_cast.
		// clang has __builtin_bit_cast but does not support std::bit_cast (Xcode 13).
		return __builtin_bit_cast(Dst, src);
	}

	// danger of aliasing
	template<typename Dst, typename Src>
	[[nodiscard]] typename aliasing_ptr<std::remove_pointer_t<Dst>>::type aliasing_cast(Src const& src) noexcept {
		static_assert( std::is_pointer<Src>::value );
		static_assert( std::is_pointer<Dst>::value );
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		return typename aliasing_ptr<std::remove_pointer_t<Dst>>::type(reinterpret_cast<Dst>(src));
	}
}

