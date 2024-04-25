
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/return_decltype.h"
#include "../base/chained.h"
#include "../base/enum.h"
#include "../base/functors.h"
#include "../base/inplace.h"
#include "../base/template_func.h"
#include "../base/empty_chain.h"
#include "../range/transform_adaptor.h"

#include "../base/change.h"
#include "find.h"
#include "../interval_types.h"
#if defined(__clang__) && !defined(__cpp_lib_three_way_comparison) // three way comparison operators are not implemented for library types in Xcode13/14. TODO Xcode15
#include "../optional.h"
#include "../variant.h"
#endif
#include <boost/preprocessor/variadic/to_seq.hpp>
#include <compare>
#include <concepts>

namespace tc {
	// < involving NAN always returns false, so it is not even a partial order
	template<typename T>
	constexpr void assert_not_isnan(T const& t) noexcept {
		if constexpr( std::floating_point<T> ) {
			_ASSERTDEBUG( !std::isnan(t) );
		}
	}
}

namespace tc {
	// we always use tc::compare to perform 3 way comparison and tc::compare tc::explicit_cast std::strong_ordering/partial_ordering result to std::weak_ordering
	template<typename Cat>
	concept is_comparison_category = std::same_as<Cat, std::weak_ordering>;

	namespace no_adl {
		// class T derives from tc::equal_from_three_way if T has a user defined operator<=> and implements operator== with operator<=>
		template< typename T >
		struct TC_EMPTY_BASES equal_from_three_way {
			template<ENABLE_SFINAE> // ENABLE_SFINAE is needed because of possible clang/regression gcc bug https://godbolt.org/z/z7z3nf34f. It will be fixed in clang 16.
			friend constexpr bool operator==(SFINAE_TYPE(T) const& lhs, SFINAE_TYPE(T) const& rhs) noexcept(noexcept(lhs<=>rhs==0)) requires requires { {lhs<=>rhs==0} -> std::same_as<bool>; } {
				return lhs<=>rhs==0;
			}
		};

		// Default primary comparison operators
		//   1. operator== requires that each base class and member has associated operator==
		//   2. operator<=>:
		//     1) with auto return type: requires that each base class and member has associated operator<=>
		//     2) with comparison category return type: requires that each base class and member has either associated operator<=>, or both operator== and operator<.
		// Our chained empty base classes are not comparable by default. If we want to define primary comparison operators as default for some class with empty base(s),
		// we need to wrap tc::comparable around the empty base chain to make the chain comparable.
		template< typename EmptyBase=void >
		struct TC_EMPTY_BASES comparable: std::conditional_t<std::is_void<EmptyBase>::value, tc::empty_chain<comparable<void>>, EmptyBase> {
			static_assert(std::is_empty<std::conditional_t<std::is_void<EmptyBase>::value, tc::empty_chain<comparable<void>>, EmptyBase>>::value);
			// tc::comparable offers primary operators only for itself, not for derived types. Therefore we cannot accidently compare 2 types derived from tc::comparable.
			template<typename T> requires std::same_as<T, comparable>
			friend constexpr bool operator==(T const&, T const&) noexcept { return true; }
			template<typename T> requires std::same_as<T, comparable>
			friend constexpr auto operator<=>(T const&, T const&) noexcept { return std::weak_ordering::equivalent; }
		};
	}
	using no_adl::equal_from_three_way;
	using no_adl::comparable;

	namespace inplace_adl {
		template<tc::is_comparison_category Cat>
		constexpr auto operator-(inplace<Cat> inplaceorder) noexcept {
			inplaceorder.m_t = 0<=>inplaceorder.m_t;
		}
	}

#ifndef __clang__
	using std::is_eq;
	using std::is_neq;
#else // Xcode14 doesn't have std::is_eq & std::is_neq while Xcode13 does.
	constexpr bool is_eq(std::partial_ordering order) noexcept { return order==0; }
	constexpr bool is_neq(std::partial_ordering order) noexcept { return order!=0; }
#endif

	namespace explicit_convert_adl {
		template<std::same_as<std::weak_ordering> TTarget, std::same_as<std::partial_ordering> TSource>
		constexpr TTarget explicit_convert_impl(adl_tag_t, std::type_identity<TTarget>, TSource const& order) noexcept {
			if(std::is_lt(order)) {
				return std::weak_ordering::less;
			} else if(tc::is_eq(order)) {
				return std::weak_ordering::equivalent;
			} else if(std::is_gt(order)) {
				return std::weak_ordering::greater;
			} else {
				_ASSERTFALSE; // unordered is not supported
				return std::weak_ordering::less;
			}
		}
	}

	// clang workaround
	template<typename Lhs, typename Rhs>
	concept has_operator_3way_compare = requires(Lhs const& lhs, Rhs const& rhs) { lhs<=>rhs; };

	template<typename Lhs, typename Rhs>
	constexpr std::weak_ordering compare(Lhs const& lhs, Rhs const& rhs) noexcept(noexcept(lhs<=>rhs)) requires has_operator_3way_compare<Lhs, Rhs> && (!tc::comparable_pointers<Lhs, Rhs>) {
		// We tc::explicit_cast std::partial_ordering/std::strong_ordering result to std::weak_ordering.
		// Comparisons that return std::partial_ordering:
		// 1. floating point comparison
		// 2. library types comparison may return std::partial_ordering if they contain floating point values. e.g. std::vector, std::pair, std::variant, std::tuple
		tc_return_cast(lhs<=>rhs);
	}

	// similar to Synthesized three-way comparison except with tc::less instead of operator<
	//   1. boost::multiprecision numbers don't support three-way comparison
	//   2. clang library iterators don't support three-way comparison
	//   3. pointers: std::compare_three_way doesn't support function pointer comparison and is not implemented in Xcode13/14
	template<typename Lhs, typename Rhs>
	constexpr std::weak_ordering compare(Lhs const& lhs, Rhs const& rhs) noexcept(noexcept(tc::less(lhs,rhs)) && noexcept(tc::less(rhs,lhs))) requires
		(!has_operator_3way_compare<Lhs, Rhs> || tc::comparable_pointers<Lhs, Rhs>) &&
		requires {
			tc::less(lhs,rhs);
			tc::less(rhs,lhs);
		}
	{
		if(tc::less(lhs,rhs)) {
			return std::weak_ordering::less;
		} else if(tc::less(rhs, lhs)){
			return std::weak_ordering::greater;
		} else {
			return std::weak_ordering::equivalent;
		}
	}

	tc_define_fn(compare)

	//////////////////////////////////////////////////////////////
	// macros for implementing compare on compound types

	#define tc_return_if_not_equal( compare ) \
		if ( auto const order_ = compare; tc::is_neq(order_) ) return order_;

	// auto&& triggers internal error for VS2017
	// decltype((lhs)) triggers internal error for VS2019 16.8.0 preview 3.0
	// Not using return_decltype_MAYTHROW because noexcept(noexcept(expr)) not needed and triggering ICE in MSVC 19.28.
	#define tc_internal_compare_expr(fcompare, lhs, rhs, expr) \
		(fcompare)( \
			([&](auto&& _) MAYTHROW -> decltype(auto) { return tc::lvalue_or_decay(VERIFYINITIALIZED(expr)); })(VERIFYINITIALIZED(lhs)), \
			([&](auto&& _) MAYTHROW -> decltype(auto) { return tc::lvalue_or_decay(VERIFYINITIALIZED(expr)); })(VERIFYINITIALIZED(rhs)) \
		)

	#define tc_compare_expr( expr ) tc_return_if_not_equal( tc_internal_compare_expr(tc::compare, lhs, rhs, expr) )
	#define tc_compare_expr_reverse( expr ) tc_return_if_not_equal( tc_internal_compare_expr(tc::compare, rhs, lhs, expr) )
	#define tc_compare_expr_reverse_if( cond, expr ) tc_return_if_not_equal( tc::negate_if(cond, tc_internal_compare_expr(tc::compare, lhs, rhs, expr)) )
	#define tc_compare_expr_times_sign( expr, sign ) tc_return_if_not_equal( tc_internal_compare_expr(tc::compare, lhs, rhs, expr)*(sign) )

	#define tc_compare_expr_lex( expr ) tc_return_if_not_equal( tc_internal_compare_expr(tc::lexicographical_compare_3way, lhs, rhs, expr) )
	#define tc_compare_expr_lex_reverse( expr ) tc_return_if_not_equal( tc_internal_compare_expr(tc::lexicographical_compare_3way, rhs, lhs, expr) )
	#define tc_compare_expr_lex_reverse_if( cond, expr ) tc_return_if_not_equal( tc::negate_if(cond, tc_internal_compare_expr(tc::lexicographical_compare_3way, lhs, rhs, expr)) )
	#define tc_compare_expr_lex_times_sign( expr, sign ) tc_return_if_not_equal( tc_internal_compare_expr(tc::lexicographical_compare_3way, lhs, rhs, expr)*(sign) )

	#define tc_compare_base( basetype ) \
		tc_compare_expr( tc::base_cast<basetype>(_) );

	#define tc_compare_mask( maskmember, maskvalue ) \
		tc_compare_expr( tc::explicit_cast<bool>((_.m_ ## maskmember) & (maskmember ## maskvalue)) )

	#define tc_compare_aspect_if_var( tplbb,  expr ) \
		if (auto const tplbb = tc_internal_compare_expr(tc_fn(tc::make_tuple), lhs, rhs, tc::explicit_cast<bool>(expr)); tplbb != tc::make_tuple(true,true)) { \
			if (tplbb == tc::make_tuple(false, true)) return std::weak_ordering::less; \
			else if (tplbb == tc::make_tuple(true, false)) return std::weak_ordering::greater; \
		} else \

	#define tc_compare_aspect_if( expr ) tc_compare_aspect_if_var(UNIQUE_IDENTIFIER, expr)

	#define tc_compare_aspect( maskmember, maskvalue, member ) \
		tc_compare_aspect_if((_.m_ ## maskmember) & (maskmember ## maskvalue)) tc_compare_expr( _.member )

	#define tc_compare_aspect_lex( maskmember, maskvalue, member ) \
		tc_compare_aspect_if((_.m_ ## maskmember) & (maskmember ## maskvalue)) tc_compare_expr_lex( _.member )

	#define tc_hash_aspect( maskmember, maskvalue, member ) \
		if((t.m_ ## maskmember) & (maskmember ## maskvalue)) { \
			tc::hash_append(h, t.member ); \
			tc::hash_append(h, true); \
		} else { \
			tc::hash_append(h, false); \
		}

	///////////////////////////////////////////////////////////
	// compare on specific types

	template<typename Rng>
	[[nodiscard]] constexpr auto lexicographical_compare_3way(Rng const& rng) noexcept {
#if 0 // TODO Xcode14
		return tc::value_or(tc::find_first_if<tc::return_value_or_none>(rng, [](auto const order) {
			return tc::is_neq(order);
		}), std::weak_ordering::equal);
#else // constexpr workaround
		tc::range_value_t<Rng> order=std::weak_ordering::equivalent;
		tc::for_each(rng, [&](auto const& order2) noexcept {
			if(tc::is_neq(order2)) {
				order = order2;
				return tc::break_;
			}
			return tc::continue_;
		});
		return order;
#endif
	}

	namespace lexicographical_compare_3way_detail {
		TC_DEFINE_ENUM(EPrefix, eprefix, (FORBID)(ALLOW)(EQUIVALENT))
		template< EPrefix eprefix, typename Lhs, typename Rhs, typename FnCompare >
		constexpr auto lexicographical_compare_3way_impl( Lhs const& lhs, Rhs const& rhs, FnCompare fnCompare) noexcept ->
			decltype(fnCompare(*tc::begin(lhs), *tc::begin(rhs)))
		{
			auto itLhs=tc::begin( lhs );
			auto const itLhsEnd=tc::end( lhs );
			auto itRhs=tc::begin( rhs );
			auto const itRhsEnd=tc::end( rhs );

			for(;;) {
				if( itLhs==itLhsEnd ) {
					if constexpr(eprefixEQUIVALENT==eprefix) {
						return std::weak_ordering::equivalent; // if lhs is a prefix of rhs this is considered equivalent
					} else {
						if( itRhs==itRhsEnd ) {
							return std::weak_ordering::equivalent;
						} else {
							_ASSERTE(eprefixALLOW==eprefix);
							return std::weak_ordering::less; // lhs shorter than rhs, thus <
						}
					}
				}
				if( itRhs==itRhsEnd ) {
					_ASSERTE(eprefixFORBID!=eprefix);
					return std::weak_ordering::greater; // rhs shorter than lhs, thus >
				}
				tc_return_if_not_equal( fnCompare( *itLhs, *itRhs ) );
				++itLhs;
				++itRhs;
			}
		}
	}

	template< typename Lhs, typename Rhs, typename FnCompare = tc::fn_compare >
	[[nodiscard]] constexpr auto lexicographical_compare_3way(Lhs const& lhs, Rhs const& rhs, FnCompare&& fnCompare = FnCompare()) noexcept {
		return lexicographical_compare_3way_detail::lexicographical_compare_3way_impl<lexicographical_compare_3way_detail::eprefixALLOW>(lhs, rhs, tc_move_if_owned(fnCompare));
	}
	template< typename Lhs, typename Rhs, typename FnCompare = tc::fn_compare >
	[[nodiscard]] constexpr auto lexicographical_compare_3way_noprefix(Lhs const& lhs, Rhs const& rhs, FnCompare&& fnCompare = FnCompare()) noexcept {
		return lexicographical_compare_3way_detail::lexicographical_compare_3way_impl<lexicographical_compare_3way_detail::eprefixFORBID>(lhs, rhs, tc_move_if_owned(fnCompare));
	}
	template< typename Lhs, typename Rhs, typename FnCompare = tc::fn_compare >
	[[nodiscard]] constexpr auto lexicographical_compare_3way_prefixequivalence(Lhs const& lhs, Rhs const& rhs, FnCompare&& fnCompare = FnCompare()) noexcept {
		return lexicographical_compare_3way_detail::lexicographical_compare_3way_impl<lexicographical_compare_3way_detail::eprefixEQUIVALENT>(lhs, rhs, tc_move_if_owned(fnCompare));
	}

	tc_define_fn( lexicographical_compare_3way );
	tc_define_fn( lexicographical_compare_3way_noprefix );
	tc_define_fn( lexicographical_compare_3way_prefixequivalence );
}

#if defined(__clang__) && !defined(__cpp_lib_three_way_comparison) // three way comparison operators are not implemented for library types in Xcode13/14. TODO Xcode15
namespace std { // ADL
	template<typename Char, typename CharTraits, typename Alloc>
	[[nodiscard]] auto operator<=>(std::basic_string<Char, CharTraits, Alloc> const& lhs, std::basic_string<Char, CharTraits, Alloc> const& rhs) noexcept {
		return tc::lexicographical_compare_3way(lhs, rhs);
	}

	template<typename Char, typename CharTraits, typename Alloc>
	[[nodiscard]] auto operator<=>(std::basic_string<Char, CharTraits, Alloc> const& lhs, Char const* rhs) noexcept {
		return tc::lexicographical_compare_3way(lhs, rhs);
	}

	template< typename First, typename Second >
	[[nodiscard]] constexpr auto operator<=>(std::pair<First, Second> const& lhs, std::pair<First, Second> const& rhs) noexcept requires requires {
		tc::compare(lhs.first, rhs.first);
		tc::compare(lhs.second, rhs.second);
	} {
		tc_compare_expr( _.first );
		tc_compare_expr( _.second );
		return std::weak_ordering::equivalent;
	}
	
	template<typename T, typename Alloc>
	[[nodiscard]] auto operator<=>(std::vector<T, Alloc> const& lhs, std::vector<T, Alloc> const& rhs) noexcept {
		return tc::lexicographical_compare_3way(lhs, rhs);
	}
	
	template<typename T, std::size_t N>
	[[nodiscard]] auto operator<=>(std::array<T, N> const& lhs, std::array<T, N> const& rhs) noexcept {
		return tc::lexicographical_compare_3way(lhs, rhs);
	}
	
	template<typename... T>
	[[nodiscard]] constexpr auto operator<=>(std::variant<T...> const& lhs, std::variant<T...> const& rhs) noexcept
		requires requires { typename boost::mp11::mp_list<decltype(tc::compare(std::declval<T const&>(), std::declval<T const&>()))...>; }
	{
		if(lhs.valueless_by_exception() && rhs.valueless_by_exception()) {
			return std::weak_ordering::equivalent;
		} else if(lhs.valueless_by_exception()) {
			return std::weak_ordering::less;
		} else if(rhs.valueless_by_exception()) {
			return std::weak_ordering::greater;
		} else {
			tc_compare_expr(_.index());
			return tc::fn_visit(
				[]<typename TVal>(TVal const& valLhs, TVal const& valRhs) noexcept {
					return tc::compare(valLhs, valRhs);
				},
				tc::never_called<std::weak_ordering>()
			)(lhs, rhs);
		}
	}

	template<typename T, typename U>
	constexpr auto operator<=>(std::optional<T> const& lhs, std::optional<U> const& rhs ) noexcept -> decltype(tc::compare(*lhs, *rhs)) {
		return lhs && rhs ? tc::compare(*lhs, *rhs) : tc::compare(static_cast<bool>(lhs), static_cast<bool>(rhs));
	}

	template<typename T>
	constexpr std::weak_ordering operator<=>(std::optional<T> const& opt, std::nullopt_t) noexcept {
		return tc::compare(static_cast<bool>(opt), false);
	}

	template<typename T, typename U, std::enable_if_t<!tc::instance<U, std::optional>>* = nullptr>
	constexpr auto operator<=>(std::optional<T> const& opt, U const& value) noexcept -> decltype(tc::compare(*opt, value)) {
		return opt ? tc::compare(*opt, value) : std::weak_ordering::less;
	}
}
#endif

namespace tc {
	///////////////////////////////////
	// argument-wise transformation

	namespace no_adl {
		// cannot be implemented as a lambda because lambdas are not assignable
		template< typename Func, typename Transform>
		struct [[nodiscard]] projected_impl /*not final, containers may derive from predicates to benefit from EBCO*/
		{
			tc::verify_functor_t<tc::decay_t<Func>> m_func;
			tc::verify_functor_t<tc::decay_t<Transform>> m_transform;

			template <typename... Args>
			constexpr auto operator()(Args&&... args) const& return_decltype_allow_xvalue_slow_MAYTHROW(
				tc_invoke_pack(m_func, tc_invoke(m_transform, tc_move_if_owned(args)))
			)

			using is_transparent = void;
		};
	}

	template< typename Func, typename Transform >
	constexpr decltype(auto) projected(Func&& func, Transform&& transform) noexcept {
		if constexpr( std::is_same<tc::decay_t<Transform>, tc::identity>::value ) {
			return tc_move_if_owned(func);
		} else {
			return no_adl::projected_impl<Func, Transform>{ tc_move_if_owned(func), tc_move_if_owned(transform) };
		}
	}

	///////////////////////////////////
	// converse_relation

	template<typename Pred>
	auto reverse_binary_rel(Pred&& pred) noexcept {
		return [pred=tc::decay_copy(tc_move_if_owned(pred))](auto const& lhs, auto const& rhs) noexcept {
			return pred(rhs, lhs);
		};
	}

	////////////////////////////////
	// Provide adapter from 3-way compare to 2-way compare

	template< typename FCompare>
	constexpr auto lessfrom3way( FCompare&& fnCompare ) noexcept {
		return tc::chained(tc_fn(std::is_lt), tc_move_if_owned(fnCompare));
	}

	template< typename FCompare>
	constexpr auto greaterfrom3way( FCompare&& fnCompare ) noexcept {
		return tc::chained(tc_fn(std::is_gt), tc_move_if_owned(fnCompare));
	}

	template< typename FCompare>
	constexpr auto equalfrom3way( FCompare&& fnCompare ) noexcept {
		return tc::chained(tc_fn(tc::is_eq), tc_move_if_owned(fnCompare));
	}

	namespace tuple_adl {
		template<typename... T, typename... U> requires requires { typename boost::mp11::mp_list<decltype(tc::compare(std::declval<T const&>(), std::declval<U const&>()))...>; }
		constexpr auto operator<=>(tc::tuple<T...> const& lhs, tc::tuple<U...> const& rhs) noexcept {
			STATICASSERTEQUAL(sizeof...(T), sizeof...(U));
			return tc::lexicographical_compare_3way(
				tc::transform(std::index_sequence_for<T...>(), [&](auto const nconstIndex) noexcept -> std::weak_ordering {
					return tc::compare(tc::get<nconstIndex()>(lhs), tc::get<nconstIndex()>(rhs));
				})
			);
		}
	}
} // namespace tc

#define EQUAL_MEMBER_IMPL(member) (lhs.member == rhs.member)
#define EQUAL_MEMBERS(...) (TC_PP_DELIMIT_TRANSFORMED_SEQ(EQUAL_MEMBER_IMPL, &&, BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__)))
