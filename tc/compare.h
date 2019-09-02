
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assign.h"
#include "return_decltype.h"
#include "enum.h"
#include "functors.h"

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
	tc::order compare( Lhs const& lhs, Rhs const& rhs ) noexcept {
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
			[&](decltype((lhs)) _) return_decltype_xvalue_by_val( VERIFYINITIALIZED(expr) )(VERIFYINITIALIZED(lhs)), \
			[&](decltype((rhs)) _) return_decltype_xvalue_by_val( VERIFYINITIALIZED(expr) )(VERIFYINITIALIZED(rhs)) \
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
	tc::order lexicographical_compare_3way(Lhs const& lhs, Rhs const& rhs) noexcept {
		return lexicographical_compare_3way_detail::lexicographical_compare_3way_impl</*bNoPrefix*/false>(lhs, rhs);
	}
	template< typename Lhs, typename Rhs >
	tc::order lexicographical_compare_3way_noprefix(Lhs const& lhs, Rhs const& rhs) noexcept {
		return lexicographical_compare_3way_detail::lexicographical_compare_3way_impl</*bNoPrefix*/true>(lhs, rhs);
	}

	DEFINE_FN( lexicographical_compare_3way );
	DEFINE_FN( lexicographical_compare_3way_noprefix );

	template< typename LFirst, typename LSecond, typename RFirst, typename RSecond >
	tc::order compare( std::pair<LFirst, LSecond> const& lhs, std::pair<RFirst, RSecond> const& rhs ) noexcept {
		COMPARE_EXPR( _.first );
		COMPARE_EXPR( _.second );
		return tc::order::equal;
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename Elem, typename Alloc, typename Rhs >
	tc::order compare( std::vector< Elem, Alloc > const& lhs, Rhs const& rhs ) noexcept {
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename Elem, typename Alloc, typename Rhs >
	tc::order compare( std::basic_string< Elem, std::char_traits<Elem>, Alloc > const& lhs, Rhs const& rhs ) noexcept {
		// should be the same as lhs.compare(rhs) which is only implemented for decltype(lhs)==Rhs
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename T, std::size_t N, typename Rhs >
	tc::order compare( std::array< T, N > const& lhs, Rhs const& rhs ) noexcept {
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	DEFINE_FN( compare );

	///////////////////////////////////
	// argument-wise transformation

	namespace no_adl {
		// cannot be implemented as a lambda because lambdas are not assignable
		template< typename Func, typename Transform>
		struct projected_impl final
		{
		public:
			tc::decay_t<Func> m_func;
			tc::decay_t<Transform> m_transform;

		public:
			template< typename ...Args >
			auto operator()(Args&& ... args) const& MAYTHROW -> tc::transform_return_t<
				Func,
				decltype(m_func(m_transform( std::forward<Args>(args) )...)),
				decltype(m_transform(std::forward<Args>(args)))...
			> {
				return m_func( m_transform( std::forward<Args>(args) )... );
			}
		};

		// special case: tc_front is a macro
		template< typename Func>
		struct projected_front_impl final
		{
		public:
			tc::decay_t<Func> m_func;

		public:
			template< typename ...Args >
			auto operator()(Args&& ... args) const& MAYTHROW return_decltype(
				m_func( tc_front( std::forward<Args>(args) )... )
			)
		};

	}

	template< typename Func, typename Transform >
	auto projected(Func&& func, Transform&& transform) noexcept return_ctor(
		no_adl::projected_impl<Func BOOST_PP_COMMA() Transform>,{ std::forward<Func>(func), std::forward<Transform>(transform) }
	)

	template< typename Func>
	auto projected_front(Func&& func) noexcept return_ctor(
		no_adl::projected_front_impl<Func>,{ std::forward<Func>(func) }
	)

	///////////////////////////////////
	// not_fn, will be available in c++17

	namespace no_adl {
		template<class F>
		struct not_fn_t {
			F f;
			template<class... Args>
			auto operator()(Args&&... args) &
				noexcept(noexcept(!f(std::forward<Args>(args)...)))
				-> decltype(!f(std::forward<Args>(args)...)) {
				return !f(std::forward<Args>(args)...);
			}
 
			// cv-qualified overload for QoI
			template<class... Args>
			auto operator()(Args&&... args) const&
				noexcept(noexcept(!f(std::forward<Args>(args)...)))
				-> decltype(!f(std::forward<Args>(args)...)) {
				return !f(std::forward<Args>(args)...);
			}
 
			template<class... Args>
			auto operator()(Args&&... args) volatile &
				noexcept(noexcept(!f(std::forward<Args>(args)...)))
				-> decltype(!f(std::forward<Args>(args)...)) {
				return !f(std::forward<Args>(args)...);
			}
			template<class... Args>
			auto operator()(Args&&... args) const volatile &
				noexcept(noexcept(!f(std::forward<Args>(args)...)))
				-> decltype(!f(std::forward<Args>(args)...)) {
				return !f(std::forward<Args>(args)...);
			}
		};
	}
 
	template<class F>
	no_adl::not_fn_t<tc::decay_t<F>> not_fn(F&& f) { return { std::forward<F>(f) }; } 

	////////////////////////////////
	// Provide adapter from 3-way compare to 2-way compare

	template<typename FCompare, typename Base>
	struct F2wayFrom3way /* final */ : private Base {
	private:
		tc::decay_t<FCompare> m_fnCompare;
	public:
		F2wayFrom3way() noexcept {} // default-constructible if m_fnCompare is default-constructible, practical for using as STL container comparator template parameter

		explicit F2wayFrom3way( FCompare&& fnCompare ) noexcept : m_fnCompare(std::forward<FCompare>(fnCompare)) {}

		template< typename Lhs, typename Rhs > bool operator()( Lhs&& lhs, Rhs&& rhs ) const& noexcept {
			return tc::base_cast<Base>(*this)(m_fnCompare(std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)), tc::order::equal);
		}
		using is_transparent = void;
	};

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
