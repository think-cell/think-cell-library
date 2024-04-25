
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "casts.h"
#ifndef __EMSCRIPTEN__
#include <atomic>
#endif

/////////////////////////////////////////////////////////////////////
// comparison functors, required for assign_min/assign_max


namespace tc {

namespace unbounded_adl {
	struct greatest {};

	[[nodiscard]] constexpr auto operator<(tc::unused, greatest) return_ctor_noexcept(tc::constant<true>, ())
	[[nodiscard]] constexpr auto operator<=(tc::unused, greatest) return_ctor_noexcept(tc::constant<true>, ())
	[[nodiscard]] constexpr auto operator<(greatest, tc::unused) return_ctor_noexcept(tc::constant<false>, ())
	[[nodiscard]] constexpr auto operator<=(greatest, tc::unused) return_ctor_noexcept(tc::constant<false>, ())

	struct least {};

	[[nodiscard]] constexpr auto operator<(tc::unused, least) return_ctor_noexcept(tc::constant<false>, ())
	[[nodiscard]] constexpr auto operator<=(tc::unused, least) return_ctor_noexcept(tc::constant<false>, ())
	[[nodiscard]] constexpr auto operator<(least, tc::unused) return_ctor_noexcept(tc::constant<true>, ())
	[[nodiscard]] constexpr auto operator<=(least, tc::unused) return_ctor_noexcept(tc::constant<true>, ())

	[[nodiscard]] constexpr auto operator<(least, greatest) return_ctor_noexcept(tc::constant<true>, ())
	[[nodiscard]] constexpr auto operator<=(least, greatest) return_ctor_noexcept(tc::constant<true>, ())
	[[nodiscard]] constexpr auto operator<(greatest, least) return_ctor_noexcept(tc::constant<false>, ())
	[[nodiscard]] constexpr auto operator<=(greatest, least) return_ctor_noexcept(tc::constant<false>, ())
}
using unbounded_adl::least;
using unbounded_adl::greatest;

////////////////////////////
// comparison functors
// Unlike std::less<> et. al., they are special-made for comparisons, so they only use operator== and operator< with constant arguments and expect noexcept and a bool return
// This avoids types having to support operator> and operator>=, which we do not want to use in our code.

namespace equal_to_default {
	template<typename Lhs, typename Rhs> requires
		std::is_class<Lhs>::value ||
		std::is_class<Rhs>::value ||
		std::is_pointer<Lhs>::value ||
		std::is_pointer<Rhs>::value ||
		std::is_same<std::remove_volatile_t<Lhs>, std::remove_volatile_t<Rhs>>::value
	[[nodiscard]] constexpr auto equal_to_impl(Lhs const& lhs, Rhs const& rhs) return_decltype_MAYTHROW(
		tc::implicit_cast<bool>(lhs==rhs)
	)
	
	template<tc::actual_arithmetic Lhs, tc::actual_arithmetic Rhs> requires
		(!std::is_same<std::remove_volatile_t<Lhs>, std::remove_volatile_t<Rhs>>::value)
	[[nodiscard]] constexpr bool equal_to_impl(Lhs const& lhs, Rhs const& rhs) noexcept {
		return tc::explicit_cast<decltype(lhs+rhs)>(lhs)==tc::explicit_cast<decltype(lhs+rhs)>(rhs);
	}
}

DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(equal_to)

// 1. at least one of the operands is a pointer,
// 2. array-to-pointer conversions, derived-to-base pointer conversions, function pointer conversions, and qualification conversions are applied as necessary to convert both operands to the same pointer type, and the resulting pointer type is an object pointer type
// 3. std::nullptr_t is not ordered comparable to pointers
template<typename Lhs, typename Rhs>
concept comparable_pointers =
	(
		(std::is_pointer<Lhs>::value && (std::is_pointer<Rhs>::value || std::is_array<Rhs>::value))
		||
		((std::is_pointer<Lhs>::value || std::is_array<Lhs>::value) && std::is_pointer<Rhs>::value)
	) &&
	std::is_pointer<tc::common_type_t<Lhs, Rhs>>::value;

template<typename Lhs, typename Rhs>
[[nodiscard]] constexpr auto less(Lhs const& lhs, Rhs const& rhs) noexcept requires requires { lhs<rhs; } {
	if constexpr (tc::actual_arithmetic<Lhs> && tc::actual_arithmetic<Rhs>) {
		auto result = tc::explicit_cast<decltype(lhs+rhs)>(lhs)<tc::explicit_cast<decltype(lhs+rhs)>(rhs);
		static_assert(std::is_same<decltype(result), bool>::value || std::is_same<decltype(result), tc::constant<true>>::value || std::is_same<decltype(result), tc::constant<false>>::value);
		return result;
	} else if constexpr (tc::comparable_pointers<Lhs, Rhs>) {
		// A specialization of std::less for any pointer type yields the implementation-defined strict total order, even if the built-in < operator does not.
		return std::less<tc::common_type_t<Lhs, Rhs>>()(lhs, rhs);
	} else {
		static_assert(
			!(std::is_pointer<Lhs>::value && std::is_class<Rhs>::value && std::convertible_to<Rhs const&, Lhs>) &&
			!(std::is_class<Lhs>::value && std::is_pointer<Rhs>::value && std::convertible_to<Lhs const&, Rhs>), // use std::convertible_to instead of tc::safely_convertible_to avoid any pointer comparison with operator<
			"if a class type is implicitly convertible to a pointer type and you want to compare this class type with the pointer type,"
			" tc::implicit_cast class object to pointer when you want to make a pointer comparison between the two,"
			" otherwise use specific comparison function, e.g. tc::lexicographical_compare_3way, instead of tc::less."
		);
		auto result = lhs<rhs;
		static_assert(std::is_same<decltype(result), bool>::value || std::is_same<decltype(result), tc::constant<true>>::value  || std::is_same<decltype(result), tc::constant<false>>::value);
		return result;
	}
}

namespace no_adl
{
	DEFINE_FN2(!tc::equal_to, fn_not_equal_to)

	struct[[nodiscard]] fn_less{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr auto operator()(Lhs const& lhs, Rhs const& rhs) const& return_decltype_noexcept(
			tc::less(lhs,rhs)
		)
		using is_transparent = void;

		using supremum = tc::greatest;
		using infimum = tc::least;
	};

	DEFINE_FN2(!tc::less, fn_greater_equal)

	struct[[nodiscard]] fn_greater{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr auto operator()(Lhs const& lhs, Rhs const& rhs) const& return_decltype_noexcept(
			tc::less(rhs,lhs)
		)
		using is_transparent = void;

		using supremum = tc::least;
		using infimum = tc::greatest;
	};

	struct[[nodiscard]] fn_less_equal{
		template<typename Lhs, typename Rhs>
		[[nodiscard]] constexpr auto operator()(Lhs const& lhs, Rhs const& rhs) const& return_decltype_noexcept(
			!tc::less(rhs,lhs)
		)
		using is_transparent = void;
	};
}

using no_adl::fn_not_equal_to;
using no_adl::fn_less;
using no_adl::fn_greater_equal;
using no_adl::fn_greater;
using no_adl::fn_less_equal;

/////////////////////////////////////////////////////////////////////
// change

template< typename Better, typename Var, typename Val  >
constexpr auto assign_better( Better better, Var&& var, Val&& val ) noexcept {
	return tc::make_overload(
		[&](tc::constant<true> b) noexcept {
			static_assert( tc::safely_assignable_from<Var&&, Val&&> );
			tc_move_if_owned(var) = tc_move_if_owned(val);
			return b;
		},
		[&](tc::constant<false> b) noexcept {
			return b;
		},
		[&](bool const b) noexcept {
			if (b) {
				static_assert( tc::safely_assignable_from<Var&&, Val&&> );
				tc_move_if_owned(var) = tc_move_if_owned(val);
				return true;
			} else {
				return false;
			}
		}
	)( tc_invoke(better, tc::as_const(val), tc::as_const(var)) );
}

template< typename Var, typename Val >
constexpr bool change( Var&& var, Val&& val ) noexcept {
	return tc::assign_better( [](auto const& val_, auto const& var_) noexcept { return !tc::equal_to(var_, val_); }, tc_move_if_owned(var), tc_move_if_owned(val) ); // var==val, not val==var
}

#ifndef __EMSCRIPTEN__
template< typename Better, typename Var, typename Val >
bool assign_better( Better better, std::atomic<Var>& var, Val&& val ) noexcept {
	Var varOld=var;
	_ASSERTINITIALIZED( varOld );
	while( better(tc::as_const(VERIFYINITIALIZED(val)), tc::as_const(varOld)) ) {
		 if( var.compare_exchange_weak( varOld, val ) ) return true;
	}
	return false;
}

template< typename Better, typename Var, typename Val0, typename... Val >
bool assign_better( Better better, std::atomic<Var>&& var, Val0&& val0, Val&&... val ) noexcept =delete; // make passing rvalue ref an error

template< typename Var, typename Val >
bool change( std::atomic<Var> & var, Val&& val ) noexcept {
	_ASSERTINITIALIZED( val );
	return !tc::equal_to(var.exchange(val),val);
}

template< typename Var, typename Val >
bool change( std::atomic<Var>&& var, Val&& val ) noexcept; // make passing rvalue ref a linker error
#endif // __EMSCRIPTEN__

template< typename Better, typename Var, typename... Val> requires (1<sizeof...(Val))
constexpr bool assign_better( Better better, Var&& var, Val&&... val) noexcept {
	bool b=false;
	static_cast<void>(std::initializer_list<bool>{(b=(tc::assign_better(better, var, tc_move_if_owned(val)) || b))...});
	return b;
}

template< typename Var, typename Val0, typename... Val >
constexpr bool assign_max( Var&& var, Val0&& val0, Val&&... val ) noexcept {
	return tc::assign_better( tc::fn_greater(), tc_move_if_owned(var), tc_move_if_owned(val0), tc_move_if_owned(val)... ); // use operator< for comparison just like tc::min/max
}

template< typename Var, typename Val0, typename... Val>
constexpr bool assign_min( Var&& var, Val0&& val0, Val&&... val ) noexcept {
	return tc::assign_better( tc::fn_less(), tc_move_if_owned(var), tc_move_if_owned(val0), tc_move_if_owned(val)... );
}

template<typename Var, typename Val>
void change_with_or(Var&& var, Val&& val, bool& bChanged) noexcept {
	// accessing an uninitialized variable is undefined behavior, so don't compare for equality if var is uninitialized!
	if( VERIFYINITIALIZED(bChanged) ) {
		_ASSERTINITIALIZED( val );
		tc_move_if_owned(var) = tc_move_if_owned(val);
	} else {
		bChanged = tc::change( tc_move_if_owned(var), tc_move_if_owned(val) );
	}
}

tc_define_fn( assign_max );
tc_define_fn( assign_min );

////////////////////////////////////
// change for float/double

// Treat float/double assignment as bit pattern assignment, to avoid NaN problems.
// Assigning NaN to NaN should be !bChanged.
// Might not work because IEEE 754 does not specify a unique bit pattern for NaNs.
// RT#5691: Also, we must not rely on comparisons involving NaN to return false, as defined by the standard, because we are using /fp:fast:
// https://connect.microsoft.com/VisualStudio/feedback/ViewFeedback.aspx?FeedbackID=518015&ppud=0&wa=wsignin1.0

template< typename Lhs, typename Rhs >
[[nodiscard]] bool binary_equal( Lhs const& lhs, Rhs const& rhs ) noexcept {
	STATICASSERTEQUAL( sizeof(lhs), sizeof(rhs) );
	return 0==std::memcmp(std::addressof(lhs),std::addressof(rhs),sizeof(lhs));
}

template< typename Dst, typename Src >
bool binary_change( Dst& dst, Src const& src ) noexcept {
	STATICASSERTEQUAL( sizeof(Dst), sizeof(src) );
	if( !binary_equal(dst,src) ) {
		std::memcpy(std::addressof(dst),std::addressof(src),sizeof(dst));
		return true;
	} else {
		return false;
	}
}

template< typename Var, typename Value >
	requires std::floating_point<tc::decay_t<Var>>	
bool change( Var&& var, Value&& value ) noexcept {
	using float_type = tc::decay_t<Var>;
	auto const nan = std::numeric_limits<float_type>::quiet_NaN();
	_ASSERTINITIALIZED( var );
	_ASSERT( !std::isnan( var ) || binary_equal(var, nan) );
	_ASSERTINITIALIZED( value );
	auto float_value = tc::implicit_cast<float_type>(tc_move_if_owned(value));
	_ASSERT( !std::isnan( float_value ) || binary_equal(float_value, nan) );
	return binary_change( var, float_value );
}
} // namespace tc
