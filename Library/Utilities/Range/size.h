#pragma once

#include "range_defines.h"
#include "container_traits.h"

#include "Library/Utilities/casts.h"
#include "Library/Utilities/convert.h"

#include <type_traits>
#include <limits>

namespace RANGE_PROPOSAL_NAMESPACE {

	// forward definitions
	namespace size_impl {
		template< typename T > class size_proxy;
	}
	using namespace size_impl;
	
	template< typename T >
	typename std::enable_if<!std::numeric_limits<T>::is_integer, T >::type make_size_proxy(T t);
	
	template< typename T >
	typename std::enable_if< std::numeric_limits<T>::is_integer, size_proxy<T> >::type make_size_proxy(T t);

	////////////////////////////////
	// tc::size_proxy in ADL barrier
	namespace size_impl {
	
		template< bool Signed >
		struct prepare_argument;

		template<>
		struct prepare_argument < true > {
			template< typename T > static T prepare(T t){ return t; }
		};

		template<>
		struct prepare_argument < false > {
			template< typename T > static typename std::make_unsigned<T>::type prepare(T t){ return make_unsigned(t); }
		};

		template< typename T >
		class size_proxy {
		public:
			T m_t;
			explicit size_proxy(T t) : m_t(t) {}
			template< typename S >
			size_proxy& operator=(S && s) {
				m_t = Convert<T>(std::forward<S>(s));
				return *this;
			}

#define CONVERT( S ) \
			operator S() const { \
				return static_cast<S>(m_t); \
						} \
			operator unsigned S() const { \
				return static_cast<unsigned S>(m_t); \
						} \
			operator size_proxy<S>() const { \
				return size_proxy<S>(boost::implicit_cast<S>(*this)); \
						} \
			operator size_proxy<unsigned S>() const { \
				return size_proxy<unsigned S>(boost::implicit_cast<unsigned S>(*this)); \
						}

			CONVERT(char)
			CONVERT(short)
			CONVERT(int)
			CONVERT(long)
			CONVERT(long long)
#undef CONVERT

			template< typename Char >
			friend std::basic_ostream<Char>& operator<<(std::basic_ostream<Char>& os, size_proxy const& rhs) {
				os << rhs.m_t;
				return os;
			}

#define operator_bool( op ) \
			template< typename S > friend typename std::enable_if< std::is_integral<S>::value, bool >::type operator op( size_proxy const& lhs, S const& rhs ) \
						{ return prepare_argument< std::is_signed<T>::value && std::is_signed<S>::value >::prepare(lhs.m_t) op prepare_argument< std::is_signed<T>::value && std::is_signed<S>::value >::prepare(rhs); } \
			template< typename S > friend typename std::enable_if< std::is_integral<S>::value, bool >::type operator op( S const& lhs, size_proxy const& rhs ) \
						{ return prepare_argument< std::is_signed<T>::value && std::is_signed<S>::value >::prepare(lhs) op prepare_argument< std::is_signed<T>::value && std::is_signed<S>::value >::prepare(rhs.m_t); } \
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
		template< typename Lhs, typename Rhs > auto operator op( size_proxy<Lhs> const& lhs, Rhs const& rhs ) \
		return_decltype( make_size_proxy( lhs.m_t op rhs ) ) \
		template< typename Lhs, typename Rhs > auto operator op( Lhs const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( make_size_proxy( lhs op rhs.m_t ) ) \
		template< typename Lhs, typename Rhs > auto operator op( size_proxy<Lhs> const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( make_size_proxy( lhs.m_t op rhs.m_t ) )
		
		operator_size_proxy(+)
		operator_size_proxy(-)
		operator_size_proxy(*)
		operator_size_proxy(/ )
		operator_size_proxy(%)
#undef operator_size_proxy
		
#define assign_operator_size_proxy( assign_op ) \
		template< typename Lhs, typename Rhs > \
		Lhs& operator assign_op ( Lhs& lhs, size_proxy<Rhs> const& rhs ) { \
			lhs assign_op rhs.m_t; /*TODO (?): conversion*/ \
			return lhs; \
				}

		assign_operator_size_proxy(+= )
		assign_operator_size_proxy(-= )
#undef assign_operator_size_proxy

	}

	template< typename T >
	typename std::enable_if<!std::numeric_limits<T>::is_integer, T >::type make_size_proxy(T t) {
		return t;
	}

	template< typename T >
	typename std::enable_if< std::numeric_limits<T>::is_integer, size_proxy<T> >::type make_size_proxy(T t) {
		return size_proxy<T>(t);
	}
	
	template<typename Rng>
	auto size(Rng const& rng) enable_if_return_decltype(
		has_mem_fn_size< typename std::decay<Rng>::type >::value,
		make_size_proxy( rng.size() )
	)

#ifndef _MSC_VER // MSVC does not yet support the proper solution:
	template<typename Rng, typename enabled = typename std::enable_if< !has_mem_fn_size< typename std::decay<Rng>::type >::value 
																	&& !std::is_pointer<typename std::decay<Rng>::type>::value> ::type >
	auto size(Rng const& rng) return_decltype(
		make_size_proxy( boost::size(rng) )
	)
#else
	template<typename Rng>
	auto size(Rng const& rng) enable_if_return_decltype(
		!has_mem_fn_size< typename std::decay<Rng>::type >::value && !std::is_pointer<typename std::decay<Rng>::type>::value,
		make_size_proxy( boost::size(rng) )
	)
#endif 

	template<typename T>
	auto size(T * pt) enable_if_return_decltype(
		tc::is_char< T >::value,
		make_size_proxy( strlen(pt) )
	)

	template<typename T, std::size_t N>
	auto size(T (&)[N]) enable_if_return_decltype(
		!tc::is_char< T >::value,
		make_size_proxy(N)
	)

}

