
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "bitfield.h"

#include "meta.h"
#include "equality_comparable.h"
#include "binary_operators.h"
#include "integer.h"
#include "size.h"
#include "tag_type.h"

#include <boost/integer.hpp>
#include <boost/numeric/conversion/cast.hpp>
#include <boost/preprocessor/seq/enum.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/push_back.hpp>

#include <bitset>
#include <cstdint>

#define _ASSERT_NOT_ORDERED(Enum) \
	bool operator<(Enum, Enum) noexcept; \
	bool operator<=(Enum, Enum) noexcept; \
	bool operator>=(Enum, Enum) noexcept; \
	bool operator>(Enum, Enum) noexcept;

namespace tc {
	struct no_contiguous_enum final {};

	template<typename T>
	no_contiguous_enum contiguous_enum_impl(T&& t) noexcept;

	template< typename Impl >
	struct contiguous_enum_eval : std::true_type {
		static constexpr typename Impl::type begin() noexcept { return Impl::begin; } \
		static constexpr typename Impl::type end() noexcept { return Impl::end; } \
	};

	template<>
	struct contiguous_enum_eval<no_contiguous_enum> : std::false_type {};

	template<typename Enum>
	struct contiguous_enum : contiguous_enum_eval< decltype(contiguous_enum_impl(std::declval<tc::decay_t<Enum>>())) > {};

	template< typename Enum, std::enable_if_t< std::is_enum<Enum>::value >* =nullptr >
	constexpr std::underlying_type_t<Enum> underlying_cast( Enum e ) noexcept {
		return static_cast<std::underlying_type_t<Enum>>(e);
	}

	constexpr unsigned char underlying_cast(bool b) noexcept {
		STATICASSERTEQUAL(sizeof(bool), sizeof(unsigned char));
		return static_cast<unsigned char>(b);
	}
}

template<typename Enum, std::enable_if_t<!tc::contiguous_enum<Enum>::value>* = nullptr>
constexpr bool IsWellDefinedEnum(std::underlying_type_t<Enum> const& /*n*/) noexcept { // reference to avoid error C4701: potentially uninitialized local variable
	// TODO: Implement IsWellDefinedEnum(...) for all persisted enum types.
	return true;
}

template<typename Enum, std::enable_if_t<tc::contiguous_enum<Enum>::value>* = nullptr>
constexpr bool IsWellDefinedEnum(std::underlying_type_t<Enum> const& n) noexcept { // reference to avoid error C4701: potentially uninitialized local variable
	// There are values that e can have without UB that are not one its enum values, in particular when e has a fixed underlying_type or the value fits
	// into the bits needed for representing the enum values:
	// http://stackoverflow.com/questions/18195312/what-happens-if-you-static-cast-invalid-value-to-enum-class
	// We do not allow such values here.
	return tc::underlying_cast(tc::contiguous_enum<Enum>::begin()) <= n && n <= tc::underlying_cast(tc::contiguous_enum<Enum>::end());
}

namespace tc {
	template< typename Enum, typename T >
	constexpr Enum enum_cast(T const& t) noexcept {
		static_assert( std::is_enum<Enum>::value ); 
		static_assert( !std::is_enum<T>::value, "Use enum_cast only for upcasts from integer to enum. For casting between enums, use tc::explicit_cast (implement SConversions<> if necessary)." ); 
		auto const n=tc::explicit_cast< std::underlying_type_t<Enum> >(t);
		_ASSERTDEBUG( IsWellDefinedEnum<Enum>(n) );
		return static_cast<Enum>(n);
	}

	// integral constants until we have constexpr operator-
	template<typename Enum, Enum e1, Enum e2>
	using enum_difference=std::integral_constant< int, static_cast<std::underlying_type_t<Enum>>(e1)-static_cast<std::underlying_type_t<Enum>>(e2) >;

	template <typename Enum>
	using enum_count=enum_difference<Enum, tc::contiguous_enum<Enum>::end(), tc::contiguous_enum<Enum>::begin()>;
}

#define BITMASK_OPS(Enum) \
constexpr Enum operator&(Enum _Left, Enum _Right) \
{	/* return _Left & _Right */ \
	return tc::enum_cast<Enum>(tc::underlying_cast(_Left) & tc::underlying_cast(_Right)); \
} \
\
constexpr Enum operator|(Enum _Left, Enum _Right) \
{	/* return _Left | _Right */ \
	return tc::enum_cast<Enum>(tc::underlying_cast(_Left) | tc::underlying_cast(_Right)); \
} \
\
constexpr Enum operator^(Enum _Left, Enum _Right) \
{	/* return _Left ^ _Right */ \
	return tc::enum_cast<Enum>(tc::underlying_cast(_Left) ^ tc::underlying_cast(_Right)); \
} \
\
constexpr Enum operator~(Enum _Left) \
{	/* return ~_Left */ \
	return tc::enum_cast<Enum>(~tc::underlying_cast(_Left)); \
} \
\
constexpr Enum& operator&=(Enum& _Left, Enum _Right) \
{	/* return _Left &= _Right */ \
	_Left = _Left & _Right; \
	return _Left; \
} \
\
constexpr Enum& operator|=(Enum& _Left, Enum _Right) \
{	/* return _Left |= _Right */ \
	_Left = _Left | _Right; \
	return _Left; \
} \
\
constexpr Enum& operator^=(Enum& _Left, Enum _Right) \
{	/* return _Left ^= _Right */ \
	_Left = _Left ^ _Right; \
	return _Left; \
} \
\
inline Enum least_significant_bit(Enum _Left) \
{ \
	return tc::enum_cast<Enum>(tc::least_significant_bit(tc::underlying_cast(_Left))); \
} \
\
inline Enum most_significant_bit(Enum _Left) \
{ \
	return tc::enum_cast<Enum>(tc::most_significant_bit(tc::underlying_cast(_Left))); \
} \
\
constexpr bool HasAllOf(Enum _Left, Enum _Right) \
{	/* return _Left HasAllOf _Right */\
	return !(~_Left & _Right); \
}\
\
_ASSERT_NOT_ORDERED(Enum)

namespace tc {
	DEFINE_TAG_TYPE(enumset_all_set_tag)
	DEFINE_TAG_TYPE(enumset_all_set_but_one_tag)

#ifdef TC_PRIVATE
	namespace no_adl {
		struct report_appender;
	}
	using no_adl::report_appender;
#endif

	template< typename Enum >
	struct enumset /*final*/
		: tc::setlike< tc::equality_comparable<enumset<Enum>> >
	{
	private:
		template< typename OtherEnum > friend struct enumset;
		using value_type = typename tc::integer<enum_count<Enum>::value>::unsigned_;
		value_type m_bitset;

		static constexpr value_type mask() noexcept {
			if constexpr( 0==enum_count<Enum>::value ) {
				return 0;
			} else {
				static_assert( 0 < enum_count<Enum>::value );
				return static_cast<value_type>(-1)>>(std::numeric_limits<value_type>::digits-enum_count<Enum>::value);
			}
		}
	public:
		constexpr enumset() noexcept
		: m_bitset(0)
		{} // makes all bits 0
		constexpr enumset( enumset_all_set_tag_t ) noexcept
		: m_bitset(mask()) {}
		constexpr enumset( Enum e, enumset_all_set_but_one_tag_t ) noexcept
		: m_bitset(static_cast<value_type>(static_cast<value_type>(1)<<(e-tc::contiguous_enum<Enum>::begin()))^mask()) {
			verify_not_end(e);
		}
		constexpr enumset( Enum e ) noexcept
		: m_bitset(static_cast<value_type>(static_cast<value_type>(1)<<(e-tc::contiguous_enum<Enum>::begin()))) {
			verify_not_end(e);
		}
		template< typename OtherEnum >
		enumset( enumset<OtherEnum> const& sete ) noexcept
		: m_bitset(sete.m_bitset) {
			STATICASSERTEQUAL(tc::underlying_cast(tc::contiguous_enum<OtherEnum>::begin()), tc::underlying_cast(tc::contiguous_enum<Enum>::begin()));
			static_assert(tc::underlying_cast(tc::contiguous_enum<OtherEnum>::end()) <= tc::underlying_cast(tc::contiguous_enum<Enum>::end()));
	#ifdef _DEBUG
			tc::explicit_cast<Enum>(tc::contiguous_enum<OtherEnum>::begin()); // check convertibility
	#endif
		}
		constexpr void bitwise_not() & noexcept {
			m_bitset^=mask();
		}
		// operators
		constexpr enumset operator~() const& noexcept {
			enumset copy=*this;
			copy.bitwise_not();
			return copy;
		}
		constexpr enumset& operator&=( enumset const& sete ) & noexcept {
			m_bitset&=sete.m_bitset;
			return *this;
		}
		constexpr enumset& operator|=( enumset const& sete ) & noexcept {
			m_bitset|=sete.m_bitset;
			return *this;
		}
		constexpr enumset& operator^=( enumset const& sete ) & noexcept {
			m_bitset^=sete.m_bitset;
			return *this;
		}
		friend bool operator==( enumset const& lhs, enumset const& rhs ) noexcept {
			return lhs.m_bitset==rhs.m_bitset;
		}
		friend bool operator==( enumset const& lhs, Enum rhs ) noexcept {
			return lhs==enumset(rhs);
		}
		friend bool is_subset( enumset const& lhs, Enum rhs ) noexcept {
			return !(lhs & ~rhs);
		}
		friend bool is_subset( enumset const& lhs, enumset const& rhs ) noexcept {
			return !(lhs & ~rhs);
		}
		Enum min() const& noexcept {
			static_assert( enum_count<Enum>::value<=std::numeric_limits<unsigned long>::digits );
			return tc::contiguous_enum<Enum>::begin()+tc::index_of_least_significant_bit(tc::explicit_cast<unsigned long>(m_bitset));
		}
		Enum max() const& noexcept {
			static_assert( enum_count<Enum>::value<=std::numeric_limits<unsigned long>::digits );
			return tc::contiguous_enum<Enum>::begin()+tc::index_of_most_significant_bit(tc::explicit_cast<unsigned long>(m_bitset));
		}
		Enum single() const& noexcept {
			return VERIFYEQUAL( min(), max() );
		}

		template<typename T=value_type,
			std::enable_if_t<std::numeric_limits<T>::digits<=std::numeric_limits<unsigned long long>::digits>* = nullptr>
		std::size_t size() const& noexcept {
			return std::bitset<enum_count<Enum>::value>(m_bitset).count();
		}
		explicit operator bool() const& noexcept {
			return 0!=m_bitset;
		}
#ifdef TC_PRIVATE
		void operator()(tc::report_appender appdr) const& noexcept;
#endif
		static constexpr enumset none() noexcept {
			return enumset<Enum>();
		}
		static constexpr enumset all() noexcept {
			return enumset<Enum>(enumset_all_set_tag);
		}
	};

	namespace no_adl {
		template<typename Enum, bool bConst>
		struct range_reference_base_with_const<enumset<Enum>, bConst> {
			using type=Enum;
		};
	}

	namespace enum_impl {
		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template< typename Enum >
		constexpr std::enable_if_t< 2 == enum_count<Enum>::value, Enum > binary_not(Enum e) noexcept {
			return tc::contiguous_enum<Enum>::end() - (verify_not_end(e) - tc::contiguous_enum<Enum>::begin() + 1);
		}

		// TODO: move std::enable_if_t to template argument list, doesn't work with MSVC
		template< typename Enum >
		constexpr std::enable_if_t< 2 != enum_count<Enum>::value, tc::enumset<Enum> > binary_not(Enum e) noexcept {
			return tc::enumset<Enum>(e, tc::enumset_all_set_but_one_tag);
		}
	}

	template< typename Enum >
	auto size(tc::enumset<Enum> const& sete) noexcept return_decltype(
		make_size_proxy(sete.size())
	)
}

// tc::contiguous_enum::begin() / end() must be constexpr methods, not constants.
// Constants that are only defined in a header must not be "odr-used" according to the std,
// i.e., they must never be passed by reference. MSVC ignores this and lets us pass the 
// constants by const&, but clang is strict about it.

// WORKAROUND until MSVC supports constexpr:
// 1. Define the begin()/end() functions but not as constexpr. Use these by default.
// 2. Define the constants too, when we need the value in const expression contexts.
#define DEFINE_CONTIGUOUS_ENUM(Enum, enumBegin, enumEnd) \
	namespace no_adl { \
		struct Enum ## _helper { \
			using type=Enum; \
			static constexpr Enum begin = enumBegin; \
			static constexpr Enum end = enumEnd; \
		}; \
	} \
	no_adl::Enum ## _helper contiguous_enum_impl(Enum&&); \
	constexpr bool check_initialized_impl(Enum const& e) noexcept { /*reference to avoid error C4701: potentially uninitialized local variable*/ \
		return IsWellDefinedEnum<Enum>(tc::underlying_cast(e)); \
	} \
	constexpr Enum verify_not_end(Enum e) noexcept { \
		_ASSERTDEBUG( \
			/* There are values that e can have without UB that are not one its enum values, in particular when e has  */ \
			/* a fixed underlying_type or the value fits into the bits needed for representing the enum values:        */ \
			/* http://stackoverflow.com/questions/18195312/what-happens-if-you-static-cast-invalid-value-to-enum-class */ \
			IsWellDefinedEnum<Enum>(tc::underlying_cast(e)) \
			&& tc::contiguous_enum<Enum>::end()!=e \
		); \
		return e; \
	} \
	constexpr boost::int_max_value_t< tc::enum_count<Enum>::value >::least operator-(Enum e1, Enum e2) noexcept { \
		return static_cast<boost::int_max_value_t< tc::enum_count<Enum>::value >::least>(tc::underlying_cast(e1)-tc::underlying_cast(e2)); \
	} \
	constexpr tc::enumset<Enum> operator|(Enum lhs, Enum rhs) noexcept { \
		tc::enumset<Enum> sete(lhs); \
		return sete|=rhs; \
	} \
	constexpr tc::enumset<Enum> operator&(Enum lhs, Enum rhs) noexcept { \
		tc::enumset<Enum> sete(lhs); \
		return sete&=rhs; \
	} \
	inline Enum& operator++(Enum& e) noexcept { \
		_ASSERTDEBUG( e!=tc::contiguous_enum<Enum>::end() ); \
		e = tc::enum_cast<Enum>( tc::underlying_cast(e)+1); \
		return e; \
	} \
	inline Enum& operator--(Enum& e) noexcept { \
		_ASSERTDEBUG( e!=tc::contiguous_enum<Enum>::begin() ); \
		e = tc::enum_cast<Enum>( tc::underlying_cast(e)-1); \
		return e; \
	} \
	template<typename N, std::enable_if_t< tc::is_actual_integer<N>::value >* =nullptr > \
	constexpr Enum& operator+=(Enum& e, N const& n) noexcept { \
		e = tc::enum_cast<Enum>( tc::add( tc::underlying_cast(e), n ) ); \
		return e; \
	} \
	template<typename N, std::enable_if_t< tc::is_actual_integer<N>::value >* =nullptr > \
	constexpr Enum& operator-=(Enum& e, N const& n) noexcept { \
		e = tc::enum_cast<Enum>( tc::sub( tc::underlying_cast(e), n ) ); \
		return e; \
	} \
	template<typename N, std::enable_if_t< tc::is_actual_integer<N>::value >* =nullptr > \
	constexpr Enum operator+(Enum e, N const& n) noexcept { \
		e+=n; \
		return e; \
	} \
	template<typename N, std::enable_if_t< tc::is_actual_integer<N>::value >* =nullptr > \
	constexpr Enum operator-(Enum e, N const& n) noexcept { \
		e-=n; \
		return e; \
	} \
	constexpr auto operator~(Enum e) return_decltype( \
		tc::enum_impl::binary_not(e) \
	)

#define PREFIX_CONSTANT_STRING( _, prefix, constant ) #prefix #constant

#ifdef _DEBUG
#define DEFINE_ENUM_REPORTSTREAM_PIPE( Enum, prefix, constants ) \
	inline char const* enum_literal(Enum e) noexcept { \
		static constexpr char const* c_map[]={ \
			BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(PREFIX_CONSTANT_STRING, prefix, constants)), \
			"tc::contiguous_enum<" #Enum ">::end()" \
		}; \
		return c_map[e-tc::contiguous_enum<Enum>::begin()]; \
	}
#else
#define DEFINE_ENUM_REPORTSTREAM_PIPE( Enum, prefix, constants )
#endif

// pair == ( Enum, prefix )
// need to forward BOOST_PP_TUPLE_ELEM result two times, otherwise concatenation does not work
#define DEFINE_ENUM_CONSTANT_INTERN_INTERN( Enum, prefix, constant ) constexpr Enum prefix##constant = Enum :: constant;
#define DEFINE_ENUM_CONSTANT_INTERN(a,b,c) DEFINE_ENUM_CONSTANT_INTERN_INTERN(a,b,c)
#define DEFINE_ENUM_CONSTANT( state, pair, constant ) DEFINE_ENUM_CONSTANT_INTERN( BOOST_PP_TUPLE_ELEM(2, 0, pair),  BOOST_PP_TUPLE_ELEM(2, 1, pair), constant)

#define DEFINE_SCOPED_ENUM_WITH_OFFSET( Enum, prefix, offset, constants ) \
	namespace Enum ## _adl { \
		enum class Enum { \
			BOOST_PP_SEQ_HEAD(constants) = offset, \
			BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_TAIL(BOOST_PP_SEQ_PUSH_BACK(constants, _END) ) ) \
		}; \
		DEFINE_CONTIGUOUS_ENUM(Enum, static_cast<Enum>(offset), Enum::_END) \
		DEFINE_ENUM_REPORTSTREAM_PIPE( Enum, prefix, constants ) \
	} \
	using Enum ## _adl::Enum;

#define DEFINE_SCOPED_ENUM( Enum, prefix, constants ) \
	DEFINE_SCOPED_ENUM_WITH_OFFSET( Enum, prefix, 0, constants )

#define DEFINE_ENUM_WITH_OFFSET( Enum, prefix, offset, constants ) \
	DEFINE_SCOPED_ENUM_WITH_OFFSET( Enum, prefix, offset, constants ) \
	BOOST_PP_SEQ_FOR_EACH(DEFINE_ENUM_CONSTANT, (Enum, prefix), constants)

#define DEFINE_ENUM( Enum, prefix, constants ) \
	DEFINE_ENUM_WITH_OFFSET( Enum, prefix, 0, constants )

#define DEFINE_UNPREFIXED_ENUM( Enum, constants ) \
	DEFINE_ENUM(Enum, , constants)
