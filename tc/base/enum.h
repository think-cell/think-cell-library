
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "casts.h"
#include "integer.h"

#include <boost/integer.hpp>
#include <boost/preprocessor/seq/enum.hpp>
#include <boost/preprocessor/seq/seq.hpp>
#include <boost/preprocessor/seq/push_back.hpp>
#include <boost/preprocessor/seq/for_each.hpp>

#include <bitset>
#include <cstdint>

namespace tc {
	template<typename T>
	concept contiguous_enum_type = tc::enum_type<T> && requires(T const& value) {
		contiguous_enum_impl(value);
	};

	namespace no_adl {
		template <typename Enum>
		struct contiguous_enum : tc::constant<false> {};

		template <contiguous_enum_type Enum>
		struct contiguous_enum<Enum> : tc::constant<true> {
			using impl = decltype(contiguous_enum_impl(std::declval<Enum>()));

			static constexpr auto begin() noexcept {
				return impl::start();
			}

			static constexpr auto end() noexcept {
				// Note that we cannot use from_underlying due to a recursion.
				return static_cast<decltype(impl::last())>(tc::to_underlying(impl::last()) + 1);
			}
		};
	}
	using no_adl::contiguous_enum;

	template<typename Enum, typename Integer>
	[[nodiscard]] constexpr bool is_enum_value_or_end(Integer const& n) noexcept { // reference to avoid error C4701: potentially uninitialized local variable
		static_assert( tc::actual_integer<Integer> );
		if constexpr( tc::contiguous_enum<Enum>::value ) {
			// There are values that e can have without UB that are not one its enum values, in particular when e has a fixed underlying_type or the value fits
			// into the bits needed for representing the enum values:
			// http://stackoverflow.com/questions/18195312/what-happens-if-you-static-cast-invalid-value-to-enum-class
			// We do not allow such values here.
			return tc::explicit_cast<Integer>(tc::to_underlying(tc::contiguous_enum<Enum>::begin())) <= n
				&& n <= tc::explicit_cast<Integer>(tc::to_underlying(tc::contiguous_enum<Enum>::end()));
		} else {
			// TODO: Implement is_enum_value_or_end(...) for all persisted enum types.
			return true;
		}
	}

	template<typename Enum, typename Integer>
	[[nodiscard]] constexpr bool is_enum_value(Integer const& n) noexcept {
		static_assert( tc::actual_integer<Integer> );
		if constexpr( tc::contiguous_enum<Enum>::value ) { 
			return tc::explicit_cast<Integer>(tc::to_underlying(tc::contiguous_enum<Enum>::begin())) <= n
				&& n < tc::explicit_cast<Integer>(tc::to_underlying(tc::contiguous_enum<Enum>::end()));
		} else {
			// TODO: Implement is_enum_value(...) for all persisted enum types.
			return true;
		}
	}

	template<tc::contiguous_enum_type Enum>
	[[nodiscard]] constexpr auto from_underlying_impl(std::type_identity<Enum>, tc::underlying_type_t<Enum> n) {
		_ASSERTDEBUG(is_enum_value_or_end<Enum>(n)); // _ASSERT triggers win/ARM64EC/release ICE
		return static_cast<Enum>(n);
	}

	// integral constants until we have constexpr operator-
	template<typename Enum, Enum e1, Enum e2>
	using enum_difference=tc::constant<static_cast<std::underlying_type_t<Enum>>(e1)-static_cast<std::underlying_type_t<Enum>>(e2)>;

	template <typename Enum>
	using enum_count=tc::constant<tc::as_unsigned(enum_difference<Enum, tc::contiguous_enum<Enum>::end(), tc::contiguous_enum<Enum>::begin()>::value)>;
}

#define TC_BITMASK_OPS(Enum) \
[[nodiscard]] constexpr Enum operator&(Enum _Left, Enum _Right) \
{	/* return _Left & _Right */ \
	return tc::from_underlying<Enum>(tc::to_underlying(_Left) & tc::to_underlying(_Right)); \
} \
\
[[nodiscard]] constexpr Enum operator|(Enum _Left, Enum _Right) \
{	/* return _Left | _Right */ \
	return tc::from_underlying<Enum>(tc::to_underlying(_Left) | tc::to_underlying(_Right)); \
} \
\
[[nodiscard]] constexpr Enum operator^(Enum _Left, Enum _Right) \
{	/* return _Left ^ _Right */ \
	return tc::from_underlying<Enum>(tc::to_underlying(_Left) ^ tc::to_underlying(_Right)); \
} \
\
[[nodiscard]] constexpr Enum operator~(Enum _Left) \
{	/* return ~_Left */ \
	return tc::from_underlying<Enum>(~tc::to_underlying(_Left)); \
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
[[nodiscard]] constexpr bool HasAllOf(Enum _Left, Enum _Right) \
{	/* return _Left HasAllOf _Right */\
	return !static_cast<bool>(~_Left & _Right); \
}\
\
std::weak_ordering operator<=>(Enum, Enum) = delete; \
bool operator<(Enum, Enum) = delete; \
bool operator<=(Enum, Enum) = delete; \
bool operator>=(Enum, Enum) = delete; \
bool operator>(Enum, Enum) = delete;

#ifdef TC_PRIVATE
struct CSaveHandler;
struct CXmlReader;
#endif

namespace tc {
	template< typename Enum> requires requires { tc::contiguous_enum<Enum>::end(); }
	constexpr void assert_not_end(Enum e) noexcept {
		_ASSERTDEBUG(
			// There are values that e can have without UB that are not one its enum values, in particular when e has
			//	a fixed underlying_type or the value fits into the bits needed for representing the enum values:
			//	http://stackoverflow.com/questions/18195312/what-happens-if-you-static-cast-invalid-value-to-enum-class 
			//	We don't allow such values here
			tc::is_enum_value<Enum>(tc::to_underlying(e))
		);
	}

	template< typename Enum> requires requires { tc::contiguous_enum<Enum>::end(); }
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

#define TC_DEFINE_CONTIGUOUS_ENUM(Enum, enumStart, enumLast) \
	[[nodiscard]] inline auto contiguous_enum_impl(Enum const&) { \
		struct impl { \
			static constexpr Enum start() noexcept { return (enumStart); } \
			static constexpr Enum last() noexcept { return (enumLast); } \
		}; \
		return impl{}; \
	} \
	[[nodiscard]] inline bool check_initialized_impl(Enum const& e) noexcept { /*reference to avoid error C4701: potentially uninitialized local variable*/ \
		return tc::is_enum_value_or_end<Enum>(tc::to_underlying(e)); \
	} \
	[[nodiscard]] constexpr tc::int_value_least_t< tc::enum_count<Enum>::value > operator-(Enum const e1, Enum const e2) noexcept { \
		return static_cast<tc::int_value_least_t< tc::enum_count<Enum>::value >>(tc::to_underlying(e1)-tc::to_underlying(e2)); \
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
		e = tc::from_underlying<Enum>( tc::to_underlying(e)+1); \
		return e; \
	} \
	constexpr Enum& operator--(Enum& e) noexcept { \
		_ASSERTDEBUG( e!=tc::contiguous_enum<Enum>::begin() ); \
		e = tc::from_underlying<Enum>( tc::to_underlying(e)-1); \
		return e; \
	} \
	template<tc::actual_integer N> \
	constexpr Enum& operator+=(Enum& e, N const& n) noexcept { \
		e = tc::from_underlying<Enum>( tc::add( tc::to_underlying(e), n ) ); \
		return e; \
	} \
	template<tc::actual_integer N> \
	constexpr Enum& operator-=(Enum& e, N const& n) noexcept { \
		e = tc::from_underlying<Enum>( tc::sub( tc::to_underlying(e), n ) ); \
		return e; \
	} \
	template<tc::actual_integer N> \
	[[nodiscard]] constexpr Enum operator+(Enum e, N const& n) noexcept { \
		e+=n; \
		return e; \
	} \
	template<tc::actual_integer N> \
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

#ifdef _DEBUG
#define TC_PREFIX_CONSTANT_STRING( _, prefix, constant ) #prefix #constant

#define TC_DEFINE_ENUM_REPORTSTREAM_PIPE( Enum, prefix, constants ) \
		namespace Enum ## _detail { \
			/* TODO: move c_map back as static local after MSVC compiler bug is solved: https://developercommunity.visualstudio.com/t/code-generation-bug-on-static-variable-i/10541326 */ \
			inline constexpr char const* c_map[]={ \
				BOOST_PP_SEQ_ENUM(BOOST_PP_SEQ_TRANSFORM(TC_PREFIX_CONSTANT_STRING, prefix, constants)), \
				"tc::contiguous_enum<" #Enum ">::end()" \
			}; \
		} \
		[[nodiscard]] inline char const* enum_literal(Enum e) noexcept { \
			return Enum ## _detail::c_map[e-tc::contiguous_enum<Enum>::begin()]; \
		}
#else
#define TC_DEFINE_ENUM_REPORTSTREAM_PIPE( Enum, prefix, constants )
#endif

// pair == ( Enum, prefix )
// need to forward BOOST_PP_TUPLE_ELEM result two times, otherwise concatenation does not work
#define TC_DEFINE_ENUM_CONSTANT_INTERN_INTERN( Enum, prefix, constant ) inline constexpr Enum prefix##constant = Enum :: constant;
#define TC_DEFINE_ENUM_CONSTANT_INTERN(a,b,c) TC_DEFINE_ENUM_CONSTANT_INTERN_INTERN(a,b,c)
#define TC_DEFINE_ENUM_CONSTANT( state, pair, constant ) TC_DEFINE_ENUM_CONSTANT_INTERN( BOOST_PP_TUPLE_ELEM(2, 0, pair),  BOOST_PP_TUPLE_ELEM(2, 1, pair), constant)

namespace tc::enum_detail {
	template<typename IntOffset>
	constexpr auto max_value_for_underlying_type(IntOffset nOffset, int const nConstants) noexcept {
		static_assert( std::is_signed<IntOffset>::value, "unsigned underlying type not supported" );
		// Assume two's complement.
		// -1 - nOffset, like ~nOffset never overflows.
		auto const nAbsBeginValue = nOffset < 0 ? ~nOffset : nOffset; // Can store an additional negative value
		auto const nEndValue = nOffset + nConstants;
		return nAbsBeginValue < nEndValue ? nEndValue : nAbsBeginValue; // Avoids dependency on tc::max.
	}
}

#define TC_DEFINE_SCOPED_ENUM_WITH_OFFSET( Enum, prefix, offset, constants ) \
	namespace Enum ## _adl { \
		enum class Enum : tc::int_value_least_t<tc::enum_detail::max_value_for_underlying_type((offset), BOOST_PP_SEQ_SIZE(constants))> { \
			BOOST_PP_SEQ_HEAD(constants) = offset, \
			BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_TAIL(BOOST_PP_SEQ_PUSH_BACK(constants, _END) ) ) \
		}; \
		TC_DEFINE_CONTIGUOUS_ENUM(Enum, Enum::BOOST_PP_SEQ_HEAD(constants), static_cast<Enum>(tc::to_underlying(Enum::_END) - 1)) \
		TC_DEFINE_ENUM_REPORTSTREAM_PIPE( Enum, prefix, constants ) \
	} \
	using Enum ## _adl::Enum;

#define TC_DEFINE_SCOPED_ENUM( Enum, prefix, constants ) \
	TC_DEFINE_SCOPED_ENUM_WITH_OFFSET( Enum, prefix, 0, constants )

#define TC_DEFINE_ENUM_WITH_OFFSET( Enum, prefix, offset, constants ) \
	TC_DEFINE_SCOPED_ENUM_WITH_OFFSET( Enum, prefix, offset, constants ) \
	BOOST_PP_SEQ_FOR_EACH(TC_DEFINE_ENUM_CONSTANT, (Enum, prefix), constants)

#define TC_DEFINE_ENUM( Enum, prefix, constants ) \
	TC_DEFINE_ENUM_WITH_OFFSET( Enum, prefix, 0, constants )

#define TC_DEFINE_UNPREFIXED_ENUM( Enum, constants ) \
	TC_DEFINE_ENUM(Enum, , constants)

namespace tc {
	namespace no_adl {
		template<typename EnumSub>
		struct sub_enum_trait;

		template<typename EnumSub, typename EnumSuper>
		struct is_sub_enum_of /* not final */: tc::constant<false> {};

		template<typename EnumSub, typename EnumSuper> requires
			std::is_same<typename sub_enum_trait<std::remove_cvref_t<EnumSub>>::super, std::remove_cvref_t<EnumSuper>>::value
		struct is_sub_enum_of<EnumSub, EnumSuper> /* not final */
			: tc::constant<true> {};

		template<typename EnumSub, typename EnumSuper> requires
			(!std::is_same<typename sub_enum_trait<std::remove_cvref_t<EnumSub>>::super, std::remove_cvref_t<EnumSuper>>::value)
		struct is_sub_enum_of<EnumSub, EnumSuper> /* not final */
			: is_sub_enum_of<typename sub_enum_trait<std::remove_cvref_t<EnumSub>>::super, std::remove_cvref_t<EnumSuper>> {};
	}
	using no_adl::is_sub_enum_of;

	namespace explicit_convert_adl {
		template<typename EnumSuper, typename EnumSub> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
		constexpr EnumSuper explicit_convert_impl(adl_tag_t, std::type_identity<EnumSuper>, EnumSub const esub) noexcept {
			return static_cast<EnumSuper>(esub); // cast from sub to super is always safe
		}

		template<typename EnumSub, typename EnumSuper> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
		constexpr EnumSub explicit_convert_impl(adl_tag_t, std::type_identity<EnumSub>, EnumSuper const esuper) noexcept {
			return tc::from_underlying<EnumSub>(tc::to_underlying(esuper));
		}

		template<typename EnumSub, typename EnumSuper> requires tc::is_sub_enum_of<EnumSub, EnumSuper>::value
		constexpr std::optional<EnumSub> explicit_convert_impl(adl_tag_t, std::type_identity<std::optional<EnumSub>>, EnumSuper const esuper) noexcept {
			if (auto const n = tc::to_underlying(esuper); tc::is_enum_value<EnumSub>(n)) {
				return static_cast<EnumSub>(n);
			} else {
				return std::nullopt;
			}
		}
	}
}

#define TC_VERIFY_EQUALITY_BETWEEN_ENUMSUPER_AND_ENUMSUB(state, pair, constant) \
	static_assert(tc::to_underlying(BOOST_PP_TUPLE_ELEM(2, 0, pair)::constant) == tc::to_underlying(BOOST_PP_TUPLE_ELEM(2, 1, pair)::constant));

#define TC_DEFINE_SUB_ENUM(EnumSuper, EnumSub, prefixsub, constants) \
	TC_DEFINE_ENUM_WITH_OFFSET(EnumSub, prefixsub, tc::to_underlying(EnumSuper::BOOST_PP_SEQ_HEAD(constants)), constants) \
	BOOST_PP_SEQ_FOR_EACH(TC_VERIFY_EQUALITY_BETWEEN_ENUMSUPER_AND_ENUMSUB, (EnumSuper, EnumSub), constants) \
	namespace tc::no_adl { \
		template<> \
		struct sub_enum_trait<EnumSub> final { \
			using super = EnumSuper; \
		}; \
	}
