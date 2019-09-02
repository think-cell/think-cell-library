
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "container_traits.h"
#include "explicit_cast.h"
#include "integer.h"
#include "meta.h"
#include "type_traits.h"
#include "generic_macros.h"
#include "tag_type.h"

#include <boost/range/traversal.hpp>
#include <boost/implicit_cast.hpp>

#include <limits>

namespace tc {
	// forward definitions
	namespace size_proxy_adl {
		template< typename T > struct size_proxy;
	}
	using size_proxy_adl::size_proxy;

	// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
	template< typename T >
	constexpr std::enable_if_t<!tc::is_actual_integer<T>::value, T > make_size_proxy(T t) noexcept;

	// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
	template< typename T >
	constexpr std::enable_if_t< tc::is_actual_integer<T>::value, size_proxy<T> > make_size_proxy(T t) noexcept;

	////////////////////////////////
	// tc::size_proxy in ADL barrier
	namespace size_proxy_adl {
		template< typename T >
		struct size_proxy final {
			static_assert(tc::is_actual_integer<T>::value);
		private:
			void AssertInvariant() & noexcept {
				// npos should only be represented by a signed number. Otherwise casts to (larger) unsigned types may not preserve npos as static_cast<T>(-1)
				_ASSERTE( static_cast<T>(-1)==m_t ? std::is_signed<T>::value : 0<=m_t );
			}
		public:
			T m_t;
			constexpr size_proxy() noexcept : m_t() {
				AssertInvariant();
			}
			constexpr explicit size_proxy(T t) noexcept : m_t(t) {
				AssertInvariant();
			}

			template<typename S>
			explicit size_proxy(size_proxy<S> const& other) noexcept
				: m_t(tc::explicit_cast<T>(other.m_t))
			{
				AssertInvariant();
			}

			template< typename S >
			size_proxy& operator=(S&& s) & noexcept {
				tc::assign_explicit_cast(m_t,std::forward<S>(s));
				AssertInvariant();
				return *this;
			}

#pragma push_macro("CONVERT")
#define CONVERT( S ) \
			operator S() const& noexcept { \
				_ASSERT(  /*good idea to require signed?*/ std::is_signed<S>::value && static_cast<T>(-1)==m_t || tc::unsigned_cast(m_t)<=tc::unsigned_cast(std::numeric_limits<S>::max()) ); \
				return static_cast<S>(m_t); \
			}
			
			CONVERT(signed char)
			CONVERT(unsigned char)
			CONVERT(short)
			CONVERT(unsigned short)
			CONVERT(int)
			CONVERT(unsigned int)
			CONVERT(long)
			CONVERT(unsigned long)
			CONVERT(long long)
			CONVERT(unsigned long long)
#pragma pop_macro("CONVERT")
			operator double() const& noexcept {
				_ASSERT(0 <= m_t || static_cast<T>(-1) == m_t);
				return tc::explicit_cast<double>(m_t);
			}

			template< typename U >
			operator size_proxy<U>() const& noexcept {
				return size_proxy<U>(boost::implicit_cast<U>(*this));
			}

#pragma push_macro("operator_bool")
#define operator_bool( op ) \
			template< typename S, std::enable_if_t< tc::is_actual_integer<S>::value>* = nullptr> friend bool operator op( size_proxy const& lhs, S const& rhs ) noexcept \
						{ return tc::prepare_argument< T,S >::prepare(lhs.m_t) op tc::prepare_argument< T,S >::prepare(rhs); } \
			template< typename S, std::enable_if_t< tc::is_actual_integer<S>::value>* = nullptr> friend bool operator op( S const& lhs, size_proxy const& rhs ) noexcept \
						{ return tc::prepare_argument< T,S >::prepare(lhs) op tc::prepare_argument< T,S >::prepare(rhs.m_t); } \
			friend bool operator op( size_proxy const& lhs, size_proxy const& rhs ) \
						{ return lhs op rhs.m_t; } \
			template< typename S > friend bool operator op( size_proxy const& lhs, size_proxy<S> const& rhs ) noexcept \
						{ return lhs op rhs.m_t; }

			operator_bool(< )
			operator_bool(<= )
			operator_bool(== )
			operator_bool(!= )
#pragma pop_macro("operator_bool")
			
			friend T ConvertToUnderlying(size_proxy const& src) noexcept {
				return src.m_t;
			}
		};

#pragma push_macro("operator_size_proxy")
#define operator_size_proxy( op ) \
		template< \
			typename Lhs, typename Rhs, \
			std::enable_if_t<tc::is_actual_integer<Rhs>::value >* = nullptr \
		> auto operator op( size_proxy<Lhs> const& lhs, Rhs const& rhs ) \
		return_decltype( \
			make_size_proxy( lhs.m_t op rhs ) \
		) \
		template< \
			typename Lhs, typename Rhs, \
			std::enable_if_t<tc::is_actual_arithmetic<Lhs>::value >* = nullptr \
		> auto operator op( Lhs const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( \
			tc::explicit_cast<Lhs>(lhs op rhs.m_t) \
		) \
		template< \
			typename Lhs, typename Rhs, \
			std::enable_if_t<!tc::is_actual_arithmetic<Lhs>::value >* = nullptr \
		> auto operator op( Lhs const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( \
			lhs op rhs.m_t \
		) \
		template< typename Lhs, typename Rhs > auto operator op( size_proxy<Lhs> const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( \
			make_size_proxy( lhs.m_t op rhs.m_t ) \
		)
		
		operator_size_proxy(+)
		operator_size_proxy(-)
		operator_size_proxy(*)
		operator_size_proxy(/ )
		operator_size_proxy(%)
#pragma pop_macro("operator_size_proxy")
		
#pragma push_macro("assign_operator_size_proxy")
#define assign_operator_size_proxy( assign_op, op ) \
		template< typename Lhs, typename Rhs, std::enable_if_t< std::is_integral<Lhs>::value >* = nullptr > \
		Lhs& operator assign_op ( Lhs& lhs, size_proxy<Rhs> const& rhs ) noexcept { \
			lhs=tc:: op (tc::as_const(lhs),rhs.m_t); \
			return lhs; \
		} \
		template< typename Lhs, typename Rhs, std::enable_if_t< !std::is_integral<Lhs>::value >* = nullptr > \
		Lhs& operator assign_op ( Lhs& lhs, size_proxy<Rhs> const& rhs ) noexcept { \
			lhs assign_op rhs.m_t; \
			return lhs; \
		}

		assign_operator_size_proxy(+=,add)
		assign_operator_size_proxy(-=,sub)
#pragma pop_macro("assign_operator_size_proxy")

		template< typename Lhs, typename Rhs, std::enable_if_t<tc::is_actual_integer<Lhs>::value>* = nullptr>
		Lhs& operator %= ( Lhs& lhs, size_proxy<Rhs> const& rhs ) noexcept {
			lhs%=rhs.m_t;
			return lhs;
		}
	}

	namespace no_adl {
		template<typename T, typename = void>
		struct is_random_access_range final : std::false_type {};

		template<typename T>
		struct is_random_access_range<T, typename std::enable_if_t<tc::is_range_with_iterators<T>::value>> final :
			std::is_convertible<typename boost::range_traversal<T>::type, boost::iterators::random_access_traversal_tag>
		{};
	}
	using no_adl::is_random_access_range;

	// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
	template< typename T >
	constexpr std::enable_if_t<!tc::is_actual_integer<T>::value, T > make_size_proxy(T t) noexcept {
		return t;
	}

	// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
	template< typename T >
	constexpr std::enable_if_t< tc::is_actual_integer<T>::value, size_proxy<T> > make_size_proxy(T t) noexcept {
		return size_proxy<T>(t);
	}

	namespace no_adl {
		template<typename T0, typename T1, typename Enable=void>
		struct common_type_decayed_size_proxy_base {};

		template<typename T0, typename T1>
		struct common_type_decayed_size_proxy_base<T0, T1, tc::void_t<tc::common_type_decayed_t<T0, T1>>> {
			using type = tc::size_proxy<tc::common_type_decayed_t<T0,T1>>;
		};

		template<typename T0, typename T1>
		struct common_type_decayed<tc::size_proxy<T0>, tc::size_proxy<T1>>: common_type_decayed_size_proxy_base<T0, T1> {};

		template<typename T0, typename T1>
		struct common_type_decayed<tc::size_proxy<T0>, T1> {
			using type = T1;
		};

		template<typename T0, typename T1>
		struct common_type_decayed<T0, tc::size_proxy<T1>> : tc::common_type_decayed<tc::size_proxy<T1>, T0> {};
	}

	namespace size_adl {
		// tc::size() requires a range with either:
		//  - a size member function, which is assumed to run in O(1)
		//  - random_access iterators, which are assumed to be able to calculate size in O(1)

		DEFINE_ADL_TAG_TYPE(size_tag);

		template<typename Rng, std::enable_if_t<
			has_mem_fn_size<Rng const>::value
		>* = nullptr >
		constexpr auto size(size_tag_t, Rng const& rng) noexcept return_decltype(
			rng.size()
		)

		template<typename Rng, std::enable_if_t<
			!has_mem_fn_size<Rng const>::value
			&& is_random_access_range<Rng>::value
		>* = nullptr>
		auto size(size_tag_t, Rng const& rng) noexcept return_decltype(
			tc::unsigned_cast(tc::end(rng) - tc::begin(rng))
		) // Do not use boost::size. It always uses std::distance, which is O(n) for
		  // ranges with boost::iterators::random_access_traversal_tag but not std::random_access_iterator_tag,
		  // e.g., boost::transform_iterator

		template<typename T, std::size_t N, std::enable_if_t<!tc::is_char< T >::value>* = nullptr>
		constexpr auto size(size_tag_t, T (&)[N]) noexcept return_decltype(
			N
		)
	}

	namespace no_adl {
		template<typename Rng>
		struct constexpr_size {};

		template<typename T, std::size_t N>
		struct constexpr_size<T[N]> : std::integral_constant<std::size_t, N> {};
	}
	using no_adl::constexpr_size;

	template<typename T>
	constexpr auto size_raw(T const& t) noexcept return_decltype(
		size(tc::size_adl::size_tag, t)
	)

	template<typename T>
	constexpr auto size(T const& t) noexcept return_decltype(
		make_size_proxy(tc::size_raw(t))
	)

	TC_HAS_EXPR(size, (T), size_raw(std::declval<T>()))
}

