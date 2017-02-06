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
#include "container_traits.h"

#include "casts.h"
#include "round.h"
#include "meta.h"
#include "type_traits.h"
#include <limits>
#include <boost/range/traversal.hpp>

namespace tc {
	// forward definitions
	namespace size_impl {
		template< typename T > struct size_proxy;
	}
	using namespace size_impl;

	// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
	template< typename T >
	constexpr std::enable_if_t<!tc::is_actual_integer<T>::value, T > make_size_proxy(T t) noexcept;

	// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
	template< typename T >
	constexpr std::enable_if_t< tc::is_actual_integer<T>::value, size_proxy<T> > make_size_proxy(T t) noexcept;

	////////////////////////////////
	// tc::size_proxy in ADL barrier
	namespace size_impl {
		template< typename T >
		struct size_proxy final {
			static_assert(tc::is_actual_integer<T>::value, "");
		private:
			void AssertInvariant() & noexcept {
				// npos should only be represented by a signed number. Otherwise casts to (larger) unsigned types may not preserve npos as static_cast<T>(-1)
				_ASSERT( static_cast<T>(-1)==m_t ? std::is_signed<T>::value : 0<=m_t );
			}
		public:
			T m_t;
			constexpr explicit size_proxy(T t) noexcept : m_t(t) {
				// AssertInvariant(); // not constexpr :-(
			}

			template<typename S>
			explicit size_proxy(size_proxy<S> const& other) noexcept
				: m_t(tc::numeric_cast<T>(other.m_t))
			{
				AssertInvariant();
			}

			template< typename S >
			size_proxy& operator=(S&& s) & noexcept {
				tc::assign_numeric_cast(m_t,std::forward<S>(s));
				AssertInvariant();
				return *this;
			}


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
#undef CONVERT

			template< typename U >
			operator size_proxy<U>() const& noexcept {
				return size_proxy<U>(boost::implicit_cast<U>(*this));
			}

// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
#define operator_bool( op ) \
			template< typename S > friend std::enable_if_t< tc::is_actual_integer<S>::value, bool > operator op( size_proxy const& lhs, S const& rhs ) noexcept \
						{ return tc::prepare_argument< T,S >::prepare(lhs.m_t) op tc::prepare_argument< T,S >::prepare(rhs); } \
			template< typename S > friend std::enable_if_t< tc::is_actual_integer<S>::value, bool > operator op( S const& lhs, size_proxy const& rhs ) noexcept \
						{ return tc::prepare_argument< T,S >::prepare(lhs) op tc::prepare_argument< T,S >::prepare(rhs.m_t); } \
			friend bool operator op( size_proxy const& lhs, size_proxy const& rhs ) \
						{ return lhs op rhs.m_t; } \
			template< typename S > friend bool operator op( size_proxy const& lhs, size_proxy<S> const& rhs ) noexcept \
						{ return lhs op rhs.m_t; }

			operator_bool(< )
			operator_bool(<= )
			operator_bool(== )
			operator_bool(!= )
#undef operator_bool
			
			friend T ConvertToUnderlying(size_proxy const& src) noexcept {
				return src.m_t;
			}
		};

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
			tc::numeric_cast<Lhs>(lhs op rhs.m_t) \
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
#undef operator_size_proxy
		
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
#undef assign_operator_size_proxy

		template< typename Lhs, typename Rhs, std::enable_if_t<tc::is_actual_integer<Lhs>::value>* = nullptr>
		Lhs& operator %= ( Lhs& lhs, size_proxy<Rhs> const& rhs ) noexcept {
			lhs%=rhs.m_t;
			return lhs;
		}
	}

	namespace is_random_access_range_adl_barrier {
		template<typename T, typename = void>
		struct is_random_access_range final : std::false_type {};

		template<typename T>
		struct is_random_access_range<T, typename std::enable_if_t<tc::is_range_with_iterators<std::remove_reference_t<T>>::value>> final :
			std::is_convertible<typename boost::range_traversal<std::remove_reference_t<T>>::type, boost::iterators::random_access_traversal_tag>
		{};
	}
	using is_random_access_range_adl_barrier::is_random_access_range;

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

	template<typename T0, typename T1>
	struct common_type_decayed<tc::size_proxy<T0>, tc::size_proxy<T1>> {
		using type = tc::size_proxy<common_type_decayed_t<T0,T1>>;
	};

	template<typename T0, typename T1>
	struct common_type_decayed<tc::size_proxy<T0>, T1> {
		using type = T1;
	};

	template<typename T0, typename T1>
	struct common_type_decayed<T0, tc::size_proxy<T1>> final : common_type_decayed<tc::size_proxy<T1>, T0> {};

	namespace size_impl {
		// tc::size() requires a range with either:
		//  - a size member function, which is assumed to run in O(1)
		//  - random_access iterators, which are assumed to be able to calculate size in O(1)

		template<typename Rng, std::enable_if_t<
			has_mem_fn_size<Rng const>::value
			&& is_range_with_iterators<Rng const>::value
		>* = nullptr >
		auto size(Rng const& rng) noexcept return_decltype(
			rng.size()
		)

		template<typename Rng, std::enable_if_t<
			!has_mem_fn_size<Rng const>::value
			&& is_random_access_range<Rng>::value
		>* = nullptr>
		auto size(Rng const& rng) noexcept return_decltype(
			tc::unsigned_cast(boost::end(rng) - boost::begin(rng))
		) // Do not use boost::size. It always uses std::distance, which is O(n) for
		  // ranges with boost::iterators::random_access_traversal_tag but not std::random_access_iterator_tag,
		  // e.g., boost::transform_iterator

		template<typename T, std::size_t N, std::enable_if_t<!tc::is_char< T >::value>* = nullptr>
		constexpr auto size(T (&)[N]) noexcept return_decltype(
			N
		)

		TC_HAS_EXPR(size, tc::size_impl::size(std::declval<T>()))

		template<typename Rng, std::enable_if_t<!tc::size_impl::has_size<Rng const>::value>* =nullptr>
		auto size_linear(Rng const& rng) noexcept return_decltype(
			boost::distance(rng)
		)

		template<typename Rng, std::enable_if_t<tc::size_impl::has_size<Rng const>::value>* =nullptr>
		auto size_linear(Rng const& rng) noexcept return_decltype(
			tc::size_impl::size(rng)
		)
	}

	template<typename T, std::size_t N>
	auto constexpr_size(T (&)[N]) -> std::integral_constant<std::size_t, N>;

	template<typename T>
	auto size(T&& t) noexcept return_decltype(
		make_size_proxy(tc::size_impl::size(std::forward<T>(t)))
	)

	template<typename T>
	auto size_linear(T&& t) noexcept return_decltype(
		make_size_proxy(tc::size_impl::size_linear(std::forward<T>(t)))
	)

}

