#pragma once

#include "range_defines.h"
#include "Library/Utilities/remove_cvref.h"

#include <boost/range/has_range_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/value_type.hpp>
#include <boost/mpl/or.hpp>
#include <boost/mpl/not.hpp>

#include <type_traits>
#include <ctype.h>

namespace RANGE_PROPOSAL_NAMESPACE {
	inline std::size_t strlen( char const* pt ) {
		return std::strlen(pt);
	}
	inline std::size_t strlen( wchar_t const* pt ) {
		return std::wcslen(pt);
	}

	inline int strcmp( char const* lhs, char const* rhs ) {
		return std::strcmp(lhs,rhs);
	}
	inline int strcmp( wchar_t const* lhs, wchar_t const* rhs ) {
		return std::wcscmp(lhs,rhs);
	}

	// cast may not be necessary, but let's avoid problems even in case of user-defined T
	template< typename T >
	bool isasciidigit( T ch ) {
		return static_cast<T>('0')<=ch && ch<=static_cast<T>('9');
	}

	template< typename T >
	bool isasciiupper( T ch ) {
		return static_cast<T>('A')<=ch && ch<=static_cast<T>('Z');
	}

	template< typename T >
	bool isasciilower( T ch ) {
		return static_cast<T>('a')<=ch && ch<=static_cast<T>('z');
	}

	template< typename T >
	T toasciiupper( T ch ) {
		if( isasciilower(ch) ) {
			return static_cast<T>( ch-('a'-'A') );
		} else {
			return ch;
		}
	}
	template< typename T >
	T toasciilower( T ch ) {
		if( isasciiupper(ch) ) {
			return static_cast<T>( ch+('a'-'A') );
		} else {
			return ch;
		}
	}
}

namespace boost {
	#define CHAR_RANGE( xchar ) \
		template<> \
		struct range_mutable_iterator<xchar*> { \
			typedef xchar* type; \
		}; \
		template<> \
		struct range_const_iterator<xchar*> { \
			typedef std::add_const<xchar>::type* type; \
		}; \
		template<> \
		struct range_mutable_iterator<xchar* const> { \
			typedef xchar* type; \
		}; \
		template<> \
		struct range_const_iterator<xchar* const> { \
			typedef std::add_const<xchar>::type* type; \
		}; \
		template<std::size_t N> \
		struct range_mutable_iterator<xchar[N]> { \
			typedef xchar* type; \
		}; \
		template<std::size_t N> \
		struct range_const_iterator<xchar[N]> { \
			typedef std::add_const<xchar>::type* type; \
		}; \
		inline xchar* range_begin(xchar* pch) { \
			return pch; \
		} \
		template<std::size_t N> \
		inline xchar* range_begin(xchar (&ach)[N]) { \
			return ach; \
		} \
		inline xchar* range_end(xchar* pch) { \
			return pch+tc::strlen(pch); \
		} \
		template<std::size_t N> \
		inline xchar* range_end(xchar (&ach)[N]) { \
			std::size_t cch=tc::strlen(ach); \
			_ASSERT(cch<N); \
			return ach+cch; \
		}

	CHAR_RANGE(char)
	CHAR_RANGE(char const)
	CHAR_RANGE(wchar_t)
	CHAR_RANGE(wchar_t const)
}

namespace RANGE_PROPOSAL_NAMESPACE {
	namespace is_char_detail {
		template< typename T >
		struct is_char: std::false_type {};
		template<>
		struct is_char<char>: std::true_type {};
		template<>
		struct is_char<wchar_t>: std::true_type {};
	}
	template< typename T >
	struct is_char: is_char_detail::is_char< typename std::remove_cv< T >::type > {};

	namespace is_range_with_iterators_detail {
		template< typename Rng >
		struct is_range_with_iterators : boost::mpl::or_<
				boost::has_range_const_iterator<Rng>,
				boost::has_range_iterator<Rng>
			>::type
		{};

		template<typename T>
		struct is_range_with_iterators<T*> : tc::is_char<T>::type {};

		template<typename T, std::size_t N>
		struct is_range_with_iterators<T[N]> : std::true_type {};
	}
	template<typename Rng>
	struct is_range_with_iterators : is_range_with_iterators_detail::is_range_with_iterators<typename tc::remove_cvref<Rng>::type> {};
}
