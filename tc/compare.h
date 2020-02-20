
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assign.h"
#include "return_decltype.h"
#include "enum.h"
#include "functors.h"
#include "invoke.h"

namespace tc {
	DEFINE_SCOPED_ENUM(order,BOOST_PP_EMPTY(),(less)(equal)(greater))

	namespace order_adl {
		inline order operator-(order ord) noexcept {
			return order::less+(order::greater-ord);
		}
	}

	// < involving NAN always returns false, so it is not even a partial order
	// specialization for wchar_t in MSVC does not have has_quiet_NAN:
	// std::enable_if_t<std::numeric_limits<T>::has_quiet_NAN || std::numeric_limits<T>::has_signaling_NAN >
	template<typename T, std::enable_if_t<std::is_floating_point<T>::value>* = nullptr>
	void assert_not_isnan(T const& t) noexcept {
		_ASSERTDEBUG( !std::isnan(t) );
	}

	// specialization for wchar_t in MSVC does not have has_quiet_NAN:
	// std::enable_if_t<!(std::numeric_limits<T>::has_quiet_NAN || std::numeric_limits<T>::has_signaling_NAN) >
	template<typename T, std::enable_if_t<!std::is_floating_point<T>::value>* = nullptr>
	void assert_not_isnan(T const& t) noexcept {}
}

// not inside tc namespace, so that compare below has no chance seeing tc::compare
namespace tc_compare_impl {
	template< typename Lhs, typename Rhs >
	tc::order compare( Lhs const& lhs, Rhs const& rhs ) noexcept {
		tc::assert_not_isnan(lhs);
		tc::assert_not_isnan(rhs);
		if( lhs < rhs ) return tc::order::less;
		else if ( rhs < lhs ) return  tc::order::greater;
		else return tc::order::equal;
	}
	template< typename Lhs, typename Rhs >
	tc::order compare_impl( Lhs const& lhs, Rhs const& rhs ) noexcept {
		return compare( lhs, rhs );
	}
}

namespace tc {
	template< typename Lhs, typename Rhs >
	[[nodiscard]] tc::order compare( Lhs const& lhs, Rhs const& rhs ) noexcept {
		return ::tc_compare_impl::compare_impl(lhs,rhs);
	}

	//////////////////////////////////////////////////////////////
	// macros for implementing compare on compound types

	#define RETURN_IF_NOT_EQUAL( compare ) { \
		tc::order order = (compare); \
		if ( tc::order::equal!=order ) return order; \
	}

	#define COMPARE_EXPR_BASE(fcompare, lhs, rhs, expr) \
		(fcompare)( \
			[&](decltype((lhs)) _) return_decltype_xvalue_by_val_MAYTHROW( VERIFYINITIALIZED(expr) )(VERIFYINITIALIZED(lhs)), \
			[&](decltype((rhs)) _) return_decltype_xvalue_by_val_MAYTHROW( VERIFYINITIALIZED(expr) )(VERIFYINITIALIZED(rhs)) \
		)

	#define COMPARE_EXPR( expr ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(tc::compare, lhs, rhs, expr) )
	#define COMPARE_EXPR_REVERSE( expr ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(tc::compare, rhs, lhs, expr) )
	#define COMPARE_EXPR_REVERSE_IF( cond, expr ) RETURN_IF_NOT_EQUAL(tc::negate_if(cond, COMPARE_EXPR_BASE(tc::compare, lhs, rhs, expr)))
	#define COMPARE_EXPR_TIMES_SIGN( expr, sign ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(tc::compare, lhs, rhs, expr)*(sign) )

	#define COMPARE_EXPR_LEX( expr ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(tc::lexicographical_compare_3way, lhs, rhs, expr) )
	#define COMPARE_EXPR_LEX_REVERSE( expr ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(tc::lexicographical_compare_3way, rhs, lhs, expr) )
	#define COMPARE_EXPR_LEX_REVERSE_IF( cond, expr ) RETURN_IF_NOT_EQUAL(tc::negate_if(cond, COMPARE_EXPR_BASE(tc::lexicographical_compare_3way, lhs, rhs, expr)))
	#define COMPARE_EXPR_LEX_TIMES_SIGN( expr, sign ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(tc::lexicographical_compare_3way, lhs, rhs, expr)*(sign) )

	#define COMPARE_BASE( basetype ) \
		COMPARE_EXPR( tc::base_cast<basetype>(_) );

	#define COMPARE_MASK( maskmember, maskvalue ) \
		COMPARE_EXPR( tc::bool_cast((_.m_ ## maskmember) & (maskmember ## maskvalue)) )

	#define COMPARE_ASPECT( maskmember, maskvalue, member ) \
		COMPARE_MASK( maskmember, maskvalue ); \
		if( (lhs.m_ ## maskmember) & (maskmember ## maskvalue) ) COMPARE_EXPR( _.member );

	#define COMPARE_FUNCTION( func ) \
		RETURN_IF_NOT_EQUAL( tc::projected(tc::fn_compare(), func )( VERIFYINITIALIZED(lhs), VERIFYINITIALIZED(rhs) ) )

	#define COMPARE_FUNCTION_REVERSE( func ) \
		RETURN_IF_NOT_EQUAL( tc::projected(tc::fn_compare(), func )( VERIFYINITIALIZED(rhs), VERIFYINITIALIZED(lhs) ) )

	#define COMPARE_FUNCTION_REVERSE_IF( cond, func ) \
		RETURN_IF_NOT_EQUAL(tc::negate_if(cond,tc::projected(tc::fn_compare(), func )( VERIFYINITIALIZED(lhs), VERIFYINITIALIZED(rhs) )))

	///////////////////////////////////////////////////////////
	// compare on specific types

	namespace lexicographical_compare_3way_detail {
		template< bool bNoPrefix, typename Lhs, typename Rhs >
		tc::order lexicographical_compare_3way_impl( Lhs const& lhs, Rhs const& rhs ) noexcept {
			auto itLhs=tc::begin( lhs );
			auto const itLhsEnd=tc::end( lhs );
			auto itRhs=tc::begin( rhs );
			auto const itRhsEnd=tc::end( rhs );

			// same as std::lexicographical_compare_3way(itLhs, itLhsEnd, itRhs, itRhsEnd), except for using compare instead of operator<
			for(;;) {
				if( itLhs==itLhsEnd ) {
					_ASSERT(!bNoPrefix || itRhs==itRhsEnd);
					if( itRhs==itRhsEnd ) { // lhs shorter than rhs, thus <
						return tc::order::equal;
					} else {
						return tc::order::less;
					}
				}
				if( itRhs==itRhsEnd ) {
					_ASSERT(!bNoPrefix);
					return tc::order::greater; // rhs shorter than lhs, thus >
				}
				RETURN_IF_NOT_EQUAL( tc::compare( *itLhs, *itRhs ) );
				++itLhs;
				++itRhs;
			}
		}
	}

	template< typename Lhs, typename Rhs >
	[[nodiscard]] tc::order lexicographical_compare_3way(Lhs const& lhs, Rhs const& rhs) noexcept {
		return lexicographical_compare_3way_detail::lexicographical_compare_3way_impl</*bNoPrefix*/false>(lhs, rhs);
	}
	template< typename Lhs, typename Rhs >
	[[nodiscard]] tc::order lexicographical_compare_3way_noprefix(Lhs const& lhs, Rhs const& rhs) noexcept {
		return lexicographical_compare_3way_detail::lexicographical_compare_3way_impl</*bNoPrefix*/true>(lhs, rhs);
	}

	DEFINE_FN( lexicographical_compare_3way );
	DEFINE_FN( lexicographical_compare_3way_noprefix );

	template< typename LFirst, typename LSecond, typename RFirst, typename RSecond >
	[[nodiscard]] tc::order compare( std::pair<LFirst, LSecond> const& lhs, std::pair<RFirst, RSecond> const& rhs ) noexcept {
		COMPARE_EXPR( _.first );
		COMPARE_EXPR( _.second );
		return tc::order::equal;
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename Elem, typename Alloc, typename Rhs >
	[[nodiscard]] tc::order compare( std::vector< Elem, Alloc > const& lhs, Rhs const& rhs ) noexcept {
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename Elem, typename Alloc, typename Rhs >
	[[nodiscard]] tc::order compare( std::basic_string< Elem, std::char_traits<Elem>, Alloc > const& lhs, Rhs const& rhs ) noexcept {
		// should be the same as lhs.compare(rhs) which is only implemented for decltype(lhs)==Rhs
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename T, std::size_t N, typename Rhs >
	[[nodiscard]] tc::order compare( std::array< T, N > const& lhs, Rhs const& rhs ) noexcept {
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	DEFINE_FN( compare );

	namespace no_adl {
		// Function pointers have disadvantages over function objects, in particular when being aggregated in wrapper objects:
		// - Invokation through a function pointer usually results in an extra indirection (like a virtual function call),
		//   unless the compiler can figure out the address is constant over the entire lifetime of the wrapper. Note that
		//   this is hard to prove for the compiler: The type of the wrapper is independent of the function address, so they
		//   might be assigned from wrappers of the same type, storing pointers to different addresses.
		// - Many wrappers in our library support default construction (which is useful when used with STL or Boost containers).
		//   Default construction would leave a function pointer unintitialized!
		template<typename T>
		struct verify_functor final{
			// TODO: We would like to verify specifically that T is a class or lambda, but unfortunately
			// there is no trait to test for the latter. For now, just blacklist (function-)pointers here,
			// as other types are not callable, anyway.
			static_assert(!std::is_pointer<T>::value, "Do not pass raw function pointer as functor. Use TC_FN instead.");
			using type=T;
		};

		template<typename T>
		using verify_functor_t=typename verify_functor<T>::type;
	}
	using no_adl::verify_functor_t;

	///////////////////////////////////
	// argument-wise transformation

	namespace no_adl {
		// cannot be implemented as a lambda because lambdas are not assignable
		template< typename Func, typename Transform>
		struct [[nodiscard]] projected_impl /*not final, containers may derive from predicates to benefit from EBCO*/
		{
			tc::verify_functor_t<tc::decay_t<Func>> m_func;
			tc::verify_functor_t<tc::decay_t<Transform>> m_transform;

			template< typename ...Args >
			constexpr auto operator()(Args&& ... args) const& MAYTHROW -> tc::transform_return_t<
				Func,
				decltype(m_func(tc::invoke(m_transform, std::forward<Args>(args))...)),
				decltype(tc::invoke(m_transform, std::forward<Args>(args)))...
			> {
				return m_func(tc::invoke(m_transform, std::forward<Args>(args))...);
			}
		};

		// special case: tc_front is a macro
		template< typename Func>
		struct [[nodiscard]] projected_front_impl final
		{
		public:
			tc::verify_functor_t<tc::decay_t<Func>> m_func;

		public:
			template< typename ...Args >
			auto operator()(Args&& ... args) const& return_decltype_MAYTHROW(
				m_func( tc_front( std::forward<Args>(args) )... )
			)
		};

	}

	template< typename Func, typename Transform >
	constexpr auto projected(Func&& func, Transform&& transform) noexcept -> no_adl::projected_impl<Func, Transform> {
		// Using return_ctor_noexcept here causes an internal compiler error in MSVC 2017 when compiling TCCrmPortal
		return no_adl::projected_impl<Func, Transform>{ std::forward<Func>(func), std::forward<Transform>(transform) };
	}

	template< typename Func>
	auto projected_front(Func&& func) return_ctor_noexcept(
		no_adl::projected_front_impl<Func>,{ std::forward<Func>(func) }
	)

	///////////////////////////////////
	// not_fn, will be available in c++17

	namespace no_adl {
		template<class F>
		struct [[nodiscard]] not_fn_t {
			F f;
			template<class... Args>
			constexpr auto operator()(Args&&... args) &
				return_decltype_MAYTHROW(!f(std::forward<Args>(args)...))
 
			// cv-qualified overload for QoI
			template<class... Args>
			constexpr auto operator()(Args&&... args) const&
				return_decltype_MAYTHROW(!f(std::forward<Args>(args)...))
 
			template<class... Args>
			constexpr auto operator()(Args&&... args) &&
				return_decltype_MAYTHROW(!tc_move(f)(std::forward<Args>(args)...))

			template<class... Args>
			constexpr auto operator()(Args&&... args) const&&
				return_decltype_MAYTHROW(!static_cast<F const&&>(f)(std::forward<Args>(args)...))
		};
	}
 
	template<class F>
	constexpr auto not_fn(F&& f) return_ctor_noexcept(no_adl::not_fn_t<tc::decay_t<F>>, { std::forward<F>(f) })

	////////////////////////////////
	// Provide adapter from 3-way compare to 2-way compare

	namespace no_adl {
		template<typename FCompare, typename Base>
		struct [[nodiscard]] F2wayFrom3way /* final */ : private Base {
		private:
			tc::verify_functor_t<tc::decay_t<FCompare>> m_fnCompare;
		public:
			F2wayFrom3way() noexcept = default; // default-constructible if m_fnCompare is default-constructible, practical for using as STL container comparator template parameter

			explicit F2wayFrom3way( FCompare&& fnCompare ) noexcept : m_fnCompare(std::forward<FCompare>(fnCompare)) {}

			template< typename Lhs, typename Rhs > bool operator()( Lhs&& lhs, Rhs&& rhs ) const& noexcept {
				return tc::base_cast<Base>(*this)(m_fnCompare(std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)), tc::order::equal);
			}
			using is_transparent = void;
		};
	}
	using no_adl::F2wayFrom3way;

	template< typename FCompare>
	F2wayFrom3way<FCompare, tc::fn_less> lessfrom3way( FCompare&& fnCompare ) noexcept {
		return F2wayFrom3way<FCompare, tc::fn_less>( std::forward<FCompare>(fnCompare) );
	}

	template< typename FCompare>
	F2wayFrom3way<FCompare, tc::fn_greater> greaterfrom3way( FCompare&& fnCompare ) noexcept {
		return F2wayFrom3way<FCompare, tc::fn_greater>( std::forward<FCompare>(fnCompare) );
	}

	template< typename FCompare>
	F2wayFrom3way<FCompare, tc::fn_equal_to> equalfrom3way( FCompare&& fnCompare ) noexcept {
		return F2wayFrom3way<FCompare, tc::fn_equal_to>( std::forward<FCompare>(fnCompare) );
	}
} // namespace tc
