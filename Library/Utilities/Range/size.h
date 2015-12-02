#pragma once

#include "range_defines.h"
#include "container_traits.h"

#include "Library/Utilities/casts.h"
#include "Library/Utilities/round.h"
#include <type_traits>
#include <limits>
#include <boost/range/traversal.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {
	// forward definitions
	namespace size_impl {
		template< typename T > struct size_proxy;
	}
	using namespace size_impl;
	
	template< typename T >
	typename std::enable_if<!std::numeric_limits<T>::is_integer, T >::type make_size_proxy(T t);
	
	template< typename T >
	typename std::enable_if< std::numeric_limits<T>::is_integer, size_proxy<T> >::type make_size_proxy(T t);

	////////////////////////////////
	// tc::size_proxy in ADL barrier
	namespace size_impl {
		template< typename T >
		struct size_proxy {
		private:
			void AssertInvariant() {
				// npos should only be represented by a signed number. Otherwise casts to (larger) unsigned types may not preserve npos as static_cast<T>(-1)
				_ASSERT( static_cast<T>(-1)==m_t ? std::is_signed<T>::value : 0<=m_t );
			}
		public:
			T m_t;
			explicit size_proxy(T t) : m_t(t) {
				AssertInvariant();
			}

			template< typename S >
			size_proxy& operator=(S&& s) {
				m_t = tc::numeric_cast<T>(std::forward<S>(s));
				AssertInvariant();
				return *this;
			}


#define CONVERT( S ) \
			operator S() const { \
				_ASSERT(  /*good idea to require signed?*/ std::is_signed<S>::value && static_cast<T>(-1)==m_t || tc::unsigned_cast(m_t)<=tc::unsigned_cast(std::numeric_limits<S>::max()) ); \
				return static_cast<S>(m_t); \
			}
			
			CONVERT(char)
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
			operator size_proxy<U>() const {
				return size_proxy<U>(boost::implicit_cast<U>(*this));
			}

			template< typename Char >
			friend std::basic_ostream<Char>& operator<<(std::basic_ostream<Char>& os, size_proxy const& rhs) {
				os << rhs.m_t;
				return os;
			}

#define operator_bool( op ) \
			template< typename S > friend typename std::enable_if< tc::is_non_char_integral<S>::value, bool >::type operator op( size_proxy const& lhs, S const& rhs ) \
						{ return tc::prepare_argument< T,S >::prepare(lhs.m_t) op tc::prepare_argument< T,S >::prepare(rhs); } \
			template< typename S > friend typename std::enable_if< tc::is_non_char_integral<S>::value, bool >::type operator op( S const& lhs, size_proxy const& rhs ) \
						{ return tc::prepare_argument< T,S >::prepare(lhs) op tc::prepare_argument< T,S >::prepare(rhs.m_t); } \
			friend bool operator op( size_proxy const& lhs, size_proxy const& rhs ) \
						{ return lhs op rhs.m_t; } \
			template< typename S > friend bool operator op( size_proxy const& lhs, size_proxy<S> const& rhs ) \
						{ return lhs op rhs.m_t; }

			operator_bool(< )
			operator_bool(<= )
			operator_bool(== )
			operator_bool(!= )
#undef operator_bool
			
			friend T ConvertToUnderlying(size_proxy const& src) {
				return src.m_t;
			}
		};

#define operator_size_proxy( op ) \
		template< \
			typename Lhs, typename Rhs, \
			std::enable_if_t<std::is_arithmetic<Rhs>::value || std::is_enum<Rhs>::value || std::numeric_limits<Rhs>::is_integer>* = nullptr \
		> auto operator op( size_proxy<Lhs> const& lhs, Rhs const& rhs ) \
		return_decltype( \
			make_size_proxy( lhs.m_t op rhs ) \
		) \
		template< \
			typename Lhs, typename Rhs, \
			std::enable_if_t<std::is_arithmetic<Lhs>::value || std::is_enum<Lhs>::value || std::numeric_limits<Lhs>::is_integer>* = nullptr \
		> auto operator op( Lhs const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( \
			make_size_proxy( lhs op rhs.m_t ) \
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
		template< typename Lhs, typename Rhs > \
		typename std::enable_if< std::is_integral<Lhs>::value, \
		Lhs& >::type operator assign_op ( Lhs& lhs, size_proxy<Rhs> const& rhs ) { \
			lhs=tc:: op (tc::make_const(lhs),rhs.m_t); \
			return lhs; \
		} \
		template< typename Lhs, typename Rhs > \
		typename std::enable_if< !std::is_integral<Lhs>::value, \
		Lhs& >::type operator assign_op ( Lhs& lhs, size_proxy<Rhs> const& rhs ) { \
			lhs assign_op rhs.m_t; \
			return lhs; \
		}

		assign_operator_size_proxy(+=,add)
		assign_operator_size_proxy(-=,sub)
#undef assign_operator_size_proxy

		template< typename Lhs, typename Rhs >
		typename std::enable_if< tc::is_non_char_integral<Lhs>::value,
		Lhs& >::type operator %= ( Lhs& lhs, size_proxy<Rhs> const& rhs ) {
			lhs%=rhs.m_t;
			return lhs;
		}

		template<typename T, typename = void>
		struct is_random_access_range : std::false_type {};

		template<typename T>
		struct is_random_access_range<T, typename boost::enable_if<is_range_with_iterators<std::remove_reference_t<T>>>::type> :
			std::is_convertible<typename boost::range_traversal<std::remove_reference_t<T>>::type, boost::iterators::random_access_traversal_tag>
		{};
	}

	template< typename T >
	typename std::enable_if<!std::numeric_limits<T>::is_integer, T >::type make_size_proxy(T t) {
		return t;
	}

	template< typename T >
	typename std::enable_if< std::numeric_limits<T>::is_integer, size_proxy<T> >::type make_size_proxy(T t) {
		return size_proxy<T>(t);
	}

	namespace size_impl {
		// tc::size() requires a range with either:
		//  - a size member function, which is assumed to run in O(1)
		//  - random_access iterators, which are assumed to be able to calculate size in O(1)

		template<typename Rng, std::enable_if_t<has_mem_fn_size< std::decay_t<Rng> >::value>* = nullptr >
		auto size(Rng const& rng) return_decltype(
			rng.size()
		)

		template<typename Rng, std::enable_if_t<
			!has_mem_fn_size< std::decay_t<Rng> >::value &&
			!std::is_pointer<std::decay_t<Rng>>::value &&
			size_impl::is_random_access_range<Rng>::value
		>* = nullptr>
		auto size(Rng const& rng) return_decltype(
			boost::size(rng)
		)

		template<typename T, std::enable_if_t<tc::is_char< T >::value>* = nullptr>
		auto size(T * pt) return_decltype(
			strlen(pt)
		)

		template<typename T, std::size_t N, std::enable_if_t<!tc::is_char< T >::value>* = nullptr>
		auto size(T (&)[N]) return_decltype(
			N
		)

		template <typename T, typename = decltype(tc::size_impl::size(std::declval<T>()))>
		static std::true_type test_has_size(int);

		template <typename T>
		static std::false_type test_has_size(...);

		template <typename T>
		struct has_size : decltype(test_has_size<T>(0)) {
		};
	}

	template<typename T>
	auto size(T&& t) return_decltype(
		make_size_proxy(tc::size_impl::size(std::forward<T>(t)))
	)
}

