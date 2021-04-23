
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"

#include "type_traits.h"
#include "meta.h"
#include "integer.h"
#include "casts.h"

#include <boost/integer.hpp>
#include <boost/preprocessor/seq/enum.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/push_back.hpp>
#include <boost/preprocessor/seq/for_each.hpp>

#include <bitset>
#include <cstdint>

#define _ASSERT_NOT_ORDERED(Enum) \
	bool operator<(Enum, Enum) noexcept; \
	bool operator<=(Enum, Enum) noexcept; \
	bool operator>=(Enum, Enum) noexcept; \
	bool operator>(Enum, Enum) noexcept;

namespace tc {
	namespace no_adl {
		struct no_contiguous_enum final {};
	}

	template<typename T>
	no_adl::no_contiguous_enum contiguous_enum_impl(T&& t) noexcept;

	namespace no_adl {
		template< typename Impl >
		struct contiguous_enum_eval : std::true_type {
			static constexpr typename Impl::type begin() noexcept { return Impl::begin; } \
			static constexpr typename Impl::type end() noexcept { return Impl::end; } \
		};

		template<>
		struct contiguous_enum_eval<no_contiguous_enum> : std::false_type {};

		template<typename Enum>
		struct contiguous_enum : contiguous_enum_eval< decltype(contiguous_enum_impl(std::declval<tc::decay_t<Enum>>())) > {};
	}
	using no_adl::contiguous_enum;
}

template<typename Enum, typename Integer, std::enable_if_t<!tc::contiguous_enum<Enum>::value && tc::is_actual_integer<Integer>::value>* = nullptr>
[[nodiscard]] constexpr bool IsWellDefinedEnum(Integer const& /*n*/) noexcept { // reference to avoid error C4701: potentially uninitialized local variable
	// TODO: Implement IsWellDefinedEnum(...) for all persisted enum types.
	return true;
}

template<typename Enum, typename Integer, std::enable_if_t<tc::contiguous_enum<Enum>::value && tc::is_actual_integer<Integer>::value>* = nullptr>
[[nodiscard]] constexpr bool IsWellDefinedEnum(Integer const& n) noexcept { // reference to avoid error C4701: potentially uninitialized local variable
	// There are values that e can have without UB that are not one its enum values, in particular when e has a fixed underlying_type or the value fits
	// into the bits needed for representing the enum values:
	// http://stackoverflow.com/questions/18195312/what-happens-if-you-static-cast-invalid-value-to-enum-class
	// We do not allow such values here.
	return tc::explicit_cast<Integer>(tc::underlying_cast(tc::contiguous_enum<Enum>::begin())) <= n
		&& n <= tc::explicit_cast<Integer>(tc::underlying_cast(tc::contiguous_enum<Enum>::end()));
}

template<typename Enum, typename Integer, std::enable_if_t<tc::contiguous_enum<Enum>::value && tc::is_actual_integer<Integer>::value>* = nullptr>
[[nodiscard]] constexpr bool IsWellDefinedEnumNotEnd(Integer const& n) noexcept {
	return tc::explicit_cast<Integer>(tc::underlying_cast(tc::contiguous_enum<Enum>::begin())) <= n
		&& n < tc::explicit_cast<Integer>(tc::underlying_cast(tc::contiguous_enum<Enum>::end()));
}

namespace tc {
	template< typename Enum, typename T >
	[[nodiscard]] constexpr Enum enum_cast(T const& t) noexcept {
		static_assert( std::is_enum<Enum>::value ); 
		static_assert( !std::is_enum<T>::value, "Use enum_cast only for upcasts from integer to enum. For casting between enums, use tc::explicit_cast (customize tc::explicit_convert if necessary)." ); 
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
[[nodiscard]] constexpr Enum operator&(Enum _Left, Enum _Right) \
{	/* return _Left & _Right */ \
	return tc::enum_cast<Enum>(tc::underlying_cast(_Left) & tc::underlying_cast(_Right)); \
} \
\
[[nodiscard]] constexpr Enum operator|(Enum _Left, Enum _Right) \
{	/* return _Left | _Right */ \
	return tc::enum_cast<Enum>(tc::underlying_cast(_Left) | tc::underlying_cast(_Right)); \
} \
\
[[nodiscard]] constexpr Enum operator^(Enum _Left, Enum _Right) \
{	/* return _Left ^ _Right */ \
	return tc::enum_cast<Enum>(tc::underlying_cast(_Left) ^ tc::underlying_cast(_Right)); \
} \
\
[[nodiscard]] constexpr Enum operator~(Enum _Left) \
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
[[nodiscard]] inline Enum least_significant_bit(Enum _Left) \
{ \
	return tc::enum_cast<Enum>(tc::least_significant_bit(tc::underlying_cast(_Left))); \
} \
\
[[nodiscard]] inline Enum most_significant_bit(Enum _Left) \
{ \
	return tc::enum_cast<Enum>(tc::most_significant_bit(tc::underlying_cast(_Left))); \
} \
\
[[nodiscard]] constexpr bool HasAllOf(Enum _Left, Enum _Right) \
{	/* return _Left HasAllOf _Right */\
	return !(~_Left & _Right); \
}\
\
_ASSERT_NOT_ORDERED(Enum)

#ifdef TC_PRIVATE
struct CSaveHandler;
struct CXmlReader;
#endif

namespace tc {
	template< typename Enum, tc::void_t<decltype(tc::contiguous_enum<Enum>::end())>* = nullptr >
	constexpr void assert_not_end(Enum e) noexcept {
		_ASSERTDEBUG(
			// There are values that e can have without UB that are not one its enum values, in particular when e has
			//	a fixed underlying_type or the value fits into the bits needed for representing the enum values:
			//	http://stackoverflow.com/questions/18195312/what-happens-if-you-static-cast-invalid-value-to-enum-class 
			//	We don't allow such values here
			IsWellDefinedEnum<Enum>(tc::underlying_cast(e))
			&& tc::contiguous_enum<Enum>::end()!=e
		);
	}

	template< typename Enum, tc::void_t<decltype(tc::contiguous_enum<Enum>::end())>* = nullptr >
	[[nodiscard]] constexpr Enum verify_not_end(Enum e) noexcept {
		tc::assert_not_end(e);
		return e;
	}

	namespace enumset_adl {
		template< typename Enum >
		struct enumset;
	}
	using enumset_adl::enumset;
}

#define DEFINE_CONTIGUOUS_ENUM(Enum, enumBegin, enumEnd) \
	namespace no_adl { \
		struct Enum ## _helper { \
			using type=Enum; \
			static constexpr Enum begin = enumBegin; \
			static constexpr Enum end = enumEnd; \
		}; \
	} \
	no_adl::Enum ## _helper contiguous_enum_impl(Enum&&); \
	[[nodiscard]] constexpr bool check_initialized_impl(Enum const& e) noexcept { /*reference to avoid error C4701: potentially uninitialized local variable*/ \
		return IsWellDefinedEnum<Enum>(tc::underlying_cast(e)); \
	} \
	[[nodiscard]] constexpr boost::int_max_value_t< tc::enum_count<Enum>::value >::least operator-(Enum e1, Enum e2) noexcept { \
		return static_cast<boost::int_max_value_t< tc::enum_count<Enum>::value >::least>(tc::underlying_cast(e1)-tc::underlying_cast(e2)); \
	} \
	template<ENABLE_SFINAE> \
	[[nodiscard]] constexpr tc::enumset<SFINAE_TYPE(Enum)> operator|(Enum lhs, Enum rhs) noexcept { \
		tc::enumset<SFINAE_TYPE(Enum)> sete(lhs); \
		return sete|=rhs; \
	} \
	template<ENABLE_SFINAE> \
	[[nodiscard]] constexpr tc::enumset<SFINAE_TYPE(Enum)> operator&(Enum lhs, Enum rhs) noexcept { \
		tc::enumset<SFINAE_TYPE(Enum)> sete(lhs); \
		return sete&=rhs; \
	} \
	constexpr Enum& operator++(Enum& e) noexcept { \
		_ASSERTDEBUG( e!=tc::contiguous_enum<Enum>::end() ); \
		e = tc::enum_cast<Enum>( tc::underlying_cast(e)+1); \
		return e; \
	} \
	constexpr Enum& operator--(Enum& e) noexcept { \
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
	[[nodiscard]] constexpr Enum operator+(Enum e, N const& n) noexcept { \
		e+=n; \
		return e; \
	} \
	template<typename N, std::enable_if_t< tc::is_actual_integer<N>::value >* =nullptr > \
	[[nodiscard]] constexpr Enum operator-(Enum e, N const& n) noexcept { \
		e-=n; \
		return e; \
	} \
	template<ENABLE_SFINAE> \
	[[nodiscard]] constexpr auto operator~(Enum e) noexcept { \
		tc::assert_not_end(e); \
		if constexpr( 2 == tc::enum_count<Enum>::value ) { \
			return tc::contiguous_enum<Enum>::end() - (e - tc::contiguous_enum<Enum>::begin() + 1); \
		} else { \
			return ~tc::enumset<SFINAE_TYPE(Enum)>(e); \
		} \
	}

#define PREFIX_CONSTANT_STRING( _, prefix, constant ) #prefix #constant

#ifdef _DEBUG
#define DEFINE_ENUM_REPORTSTREAM_PIPE( Enum, prefix, constants ) \
	[[nodiscard]] inline char const* enum_literal(Enum e) noexcept { \
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
#define DEFINE_ENUM_CONSTANT_INTERN_INTERN( Enum, prefix, constant ) inline constexpr Enum prefix##constant = Enum :: constant;
#define DEFINE_ENUM_CONSTANT_INTERN(a,b,c) DEFINE_ENUM_CONSTANT_INTERN_INTERN(a,b,c)
#define DEFINE_ENUM_CONSTANT( state, pair, constant ) DEFINE_ENUM_CONSTANT_INTERN( BOOST_PP_TUPLE_ELEM(2, 0, pair),  BOOST_PP_TUPLE_ELEM(2, 1, pair), constant)

namespace tc::enum_detail {
	template<typename IntOffset>
	constexpr auto max_value_for_underlying_type(IntOffset nOffset, int nConstants) noexcept {
		static_assert( std::is_signed<IntOffset>::value, "unsigned underlying type not supported" );
		// Assume two's complement.
		// -1 - nOffset, like ~nOffset never overflows.
		auto const nAbsBeginValue = nOffset < 0 ? ~nOffset : nOffset; // Can store an additional negative value
		auto const nEndValue = nOffset + nConstants;
		return nAbsBeginValue < nEndValue ? nEndValue : nAbsBeginValue; // Avoids dependency on tc::max.
	}
}

#define DEFINE_SCOPED_ENUM_WITH_OFFSET( Enum, prefix, offset, constants ) \
	namespace Enum ## _adl { \
		enum class Enum : boost::int_max_value_t<tc::enum_detail::max_value_for_underlying_type((offset), BOOST_PP_SEQ_SIZE(constants))>::least { \
			BOOST_PP_SEQ_HEAD(constants) = offset, \
			BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_TAIL(BOOST_PP_SEQ_PUSH_BACK(constants, _END) ) ) \
		}; \
		DEFINE_CONTIGUOUS_ENUM(Enum, Enum::BOOST_PP_SEQ_HEAD(constants), Enum::_END) \
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

#define VERIFY_EQUALITY_BETWEEN_ENUMSUPER_AND_ENUMSUB(state, pair, constant) \
	static_assert(tc::underlying_cast(BOOST_PP_TUPLE_ELEM(2, 0, pair)::constant) == tc::underlying_cast(BOOST_PP_TUPLE_ELEM(2, 1, pair)::constant));

namespace tc {
	namespace no_adl {
		template<typename EnumSub>
		struct sub_enum_trait;

		template<typename EnumSub, typename EnumSuper, typename=void>
		struct is_sub_enum_of /* not final */: std::false_type {};

		template<typename EnumSub, typename EnumSuper>
		struct is_sub_enum_of<EnumSub, EnumSuper, std::enable_if_t<std::is_same<typename sub_enum_trait<tc::remove_cvref_t<EnumSub>>::super, tc::remove_cvref_t<EnumSuper>>::value>> /* not final */
			: std::true_type {};

		template<typename EnumSub, typename EnumSuper>
		struct is_sub_enum_of<EnumSub, EnumSuper, std::enable_if_t<!std::is_same<typename sub_enum_trait<tc::remove_cvref_t<EnumSub>>::super, tc::remove_cvref_t<EnumSuper>>::value>> /* not final */
			: is_sub_enum_of<typename sub_enum_trait<tc::remove_cvref_t<EnumSub>>::super, tc::remove_cvref_t<EnumSuper>> {};
	}
	using no_adl::is_sub_enum_of;

	namespace explicit_convert_adl {
		template<typename EnumSuper, typename EnumSub, std::enable_if_t<tc::is_sub_enum_of<EnumSub, EnumSuper>::value>* = nullptr>
		constexpr EnumSuper explicit_convert_impl(adl_tag_t, tc::type::identity<EnumSuper>, EnumSub const esub) noexcept {
			return static_cast<EnumSuper>(esub); // cast from sub to super is always safe
		}

		template<typename EnumSub, typename EnumSuper, std::enable_if_t<tc::is_sub_enum_of<EnumSub, EnumSuper>::value>* = nullptr>
		constexpr EnumSub explicit_convert_impl(adl_tag_t, tc::type::identity<EnumSub>, EnumSuper const esuper) noexcept {
			return tc::enum_cast<EnumSub>(tc::underlying_cast(esuper));
		}
	}
}

#define DEFINE_SUB_ENUM(EnumSuper, EnumSub, prefixsub, constants) \
	DEFINE_ENUM_WITH_OFFSET(EnumSub, prefixsub, tc::underlying_cast(EnumSuper::BOOST_PP_SEQ_HEAD(constants)), constants) \
	BOOST_PP_SEQ_FOR_EACH(VERIFY_EQUALITY_BETWEEN_ENUMSUPER_AND_ENUMSUB, (EnumSuper, EnumSub), constants) \
	namespace tc::no_adl { \
		template<> \
		struct sub_enum_trait<EnumSub> final { \
			using super = EnumSuper; \
		}; \
	}
