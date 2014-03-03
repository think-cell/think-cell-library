#pragma once

#include "range_defines.h"
#include "container_traits.h"

#include "Library/Utilities/casts.h"
#include "Library/Utilities/convert.h"

#include <type_traits>
#include <limits>

namespace RANGE_PROPOSAL_NAMESPACE {

	/////////////////////////////////////////////////////
	// size

	template< bool Signed >
	struct prepare_argument;

	template<>
	struct prepare_argument<true> {
		template< typename T > static T prepare( T t ){ return t; }
	};

	template<>
	struct prepare_argument<false> {
		template< typename T > static typename std::make_unsigned<T>::type prepare( T t ){ return make_unsigned(t); }
	};

	template< typename T >
	class size_proxy {
	public:
		T m_t;
		explicit size_proxy(T t): m_t(t) {}
		template< typename S >
		size_proxy& operator=( S && s ) {
			m_t=Convert<T>(std::forward<S>(s));
			return *this;
		}

#define CONVERT( S ) \
		operator S() const { \
			return static_cast<S>(m_t); \
		} \
		operator unsigned S() const { \
			return static_cast<unsigned S>(m_t); \
		}

		CONVERT( char )
		CONVERT( short )
		CONVERT( int )
		CONVERT( long )
#ifdef WIN32
		CONVERT( __int64 )
#endif
        
#undef CONVERT

		template< typename Char >
		friend std::basic_ostream<Char>& operator<<( std::basic_ostream<Char>& os, size_proxy const& rhs ) {
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

		operator_bool( < )
		operator_bool( <= )
		operator_bool( == )
		operator_bool( != )
#undef operator_bool

		friend T ConvertToUnderlying( size_proxy const& src ) {
			return src.m_t;
		}
	};

	template< typename T >
	typename boost::disable_if_c< std::numeric_limits<T>::is_integer, T >::type make_size_proxy(T t) {
		return t;
	}

	template< typename T >
	typename boost::enable_if_c< std::numeric_limits<T>::is_integer, size_proxy<T> >::type make_size_proxy(T t) {
		return size_proxy<T>(t);
	}

#define operator_size_proxy( op ) \
	template< typename Lhs, typename Rhs > auto operator op( size_proxy<Lhs> const& lhs, Rhs const& rhs ) \
		return_decltype( make_size_proxy( lhs.m_t op rhs ) ) \
	template< typename Lhs, typename Rhs > auto operator op( Lhs const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( make_size_proxy( lhs op rhs.m_t ) ) \
	template< typename Lhs, typename Rhs > auto operator op( size_proxy<Lhs> const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype( make_size_proxy( lhs.m_t op rhs.m_t ) )

		operator_size_proxy( + )
		operator_size_proxy( - )
		operator_size_proxy( * )
		operator_size_proxy( / )
		operator_size_proxy( % )

	template<typename Rng>
	auto size(Rng const& rng) enable_if_decltype_return( has_mem_fn_size< typename remove_cvref<Rng>::type >::value,
		make_size_proxy( rng.size() ) )

	template<typename Rng>
	auto size(Rng const& rng) enable_if_decltype_return( !has_mem_fn_size< typename remove_cvref<Rng>::type >::value && !std::is_array<typename remove_cvref<Rng>::type>::value,
		make_size_proxy( boost::size(rng) ) )

	template<typename T>
	auto size(T * pt ) return_decltype(
		make_size_proxy( strlen(pt) ) )

	template<typename T, std::size_t N>
	auto size(T (&)[N])->decltype( make_size_proxy(N) ) {
		static_assert( !is_char<T>::value, "" ); // zero-terminated pointer, zero-terminated array or plain array? be specific
		return make_size_proxy(N);
	}

}

