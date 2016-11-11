//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "assign.h"
#include "return_decltype.h"
#ifdef TC_PRIVATE
#include "Library/HeaderOnly/enum.h"
#endif
#include "quantifier.h"
#include "container.h" // tc::vector
#include "functors.h"

namespace tc {
	namespace order_t_adl_barrier {
		enum class order {
			less, equal, greater, end__
		};
#ifdef TC_PRIVATE
		DEFINE_CONTIGUOUS_ENUM(order,order::less,order::end__)
#endif

		inline order operator-(order ord) noexcept {
#ifdef TC_PRIVATE
			return order::less+(order::greater-ord);
#else
			using TUnderlying = std::underlying_type_t<order>;
			return static_cast<order>(
				static_cast<TUnderlying>(order::less) + (static_cast<TUnderlying>(order::greater) - static_cast<TUnderlying>(ord))
			);
#endif
		}
	}
	using order_t_adl_barrier::order;

	// < involving NAN always returns false, so it is not even a partial order
	// specialization for wchar_t in MSVC does not have has_quiet_NAN:
	// std::enable_if_t<std::numeric_limits<T>::has_quiet_NAN || std::numeric_limits<T>::has_signaling_NAN >
	template<typename T, std::enable_if_t<std::is_floating_point<T>::value>* = nullptr>
	void assert_not_isnan(T const& t) noexcept {
		_ASSERT( !std::isnan(t) );
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

	#define COMPARE_EXPR_BASE(lhs, rhs, expr) \
		tc::compare( \
			[&](decltype((lhs)) _) return_decltype_rvalue_by_val_variable_by_ref( VERIFYINITIALIZED(expr) )(VERIFYINITIALIZED(lhs)), \
			[&](decltype((rhs)) _) return_decltype_rvalue_by_val_variable_by_ref( VERIFYINITIALIZED(expr) )(VERIFYINITIALIZED(rhs)) \
		)

	#define COMPARE_EXPR( expr ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(lhs, rhs, expr) )
	#define COMPARE_EXPR_REVERSE( expr ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(rhs, lhs, expr) )
	#define COMPARE_EXPR_REVERSE_IF( cond, expr ) RETURN_IF_NOT_EQUAL(tc::negate_if(cond, COMPARE_EXPR_BASE(lhs, rhs, expr)))
	#define COMPARE_EXPR_TIMES_SIGN( expr, sign ) RETURN_IF_NOT_EQUAL( COMPARE_EXPR_BASE(lhs, rhs, expr)*(sign) )

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

	template< typename Lhs, typename Rhs >
	tc::order lexicographical_compare_3way( Lhs const& lhs, Rhs const& rhs ) noexcept {
		auto itLhs=boost::begin( lhs );
		auto const itLhsEnd=boost::end( lhs );
		auto itRhs=boost::begin( rhs );
		auto const itRhsEnd=boost::end( rhs );

		// same as std::lexicographical_compare_3way(itLhs, itLhsEnd, itRhs, itRhsEnd), except for using compare instead of operator<
		for(;;) {
			if( itLhs==itLhsEnd ) {
				return itRhs==itRhsEnd ? tc::order::equal : tc::order::less; // lhs shorter than rhs, thus <
			}
			if( itRhs==itRhsEnd ) {
				return tc::order::greater; // rhs shorter than lhs, thus >
			}
			RETURN_IF_NOT_EQUAL( tc::compare( *itLhs, *itRhs ) );
			++itLhs;
			++itRhs;
		}
	}

	DEFINE_FN( lexicographical_compare_3way );

	template< typename LFirst, typename LSecond, typename RFirst, typename RSecond >
	tc::order compare( std::pair<LFirst, LSecond> const& lhs, std::pair<RFirst, RSecond> const& rhs ) noexcept {
		COMPARE_EXPR( _.first );
		COMPARE_EXPR( _.second );
		return tc::order::equal;
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename Elem, typename Alloc, typename Rhs >
	tc::order compare( tc::vector< Elem, Alloc > const& lhs, Rhs const& rhs ) noexcept {
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

	namespace projected_impl_adl_barrier {
		// cannot be implemted as a lambda because lambdas are not assignable
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
		projected_impl_adl_barrier::projected_impl<Func BOOST_PP_COMMA() Transform>,{ std::forward<Func>(func), std::forward<Transform>(transform) }
	)

	template< typename Func>
	auto projected_front(Func&& func) noexcept return_ctor(
		projected_impl_adl_barrier::projected_front_impl<Func>,{ std::forward<Func>(func) }
	)

	///////////////////////////////////
	// not_fn, will be available in c++17

	namespace not_fn_impl_adl_barrier {
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
	not_fn_impl_adl_barrier::not_fn_t<tc::decay_t<F>> not_fn(F&& f) { return { std::forward<F>(f) }; } 

	////////////////////////////////
	// Provide adapter from 3-way compare to 2-way compare

	template<typename FCompare, typename Base>
	struct F2wayFrom3way /* final */ : private Base {
	private:
		FCompare m_fnCompare;
	public:
		F2wayFrom3way() noexcept {} // default-constructible if m_fnCompare is default-constructible, practical for using as STL container comparator template parameter
		F2wayFrom3way( FCompare fnCompare ) noexcept : m_fnCompare(fnCompare) {}
		template< typename Lhs, typename Rhs > bool operator()( Lhs&& lhs, Rhs&& rhs ) const& noexcept {
			return tc::base_cast<Base>(*this)(m_fnCompare(std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)), tc::order::equal);
		}
		using is_transparent = void;
	};

	template< typename FCompare> F2wayFrom3way<FCompare, tc::fn_less> lessfrom3way( FCompare fnCompare ) noexcept {
		return F2wayFrom3way<FCompare, tc::fn_less>( fnCompare );
	}

	template< typename FCompare> F2wayFrom3way<FCompare, tc::fn_greater> greaterfrom3way( FCompare fnCompare ) noexcept {
		return F2wayFrom3way<FCompare, tc::fn_greater>( fnCompare );
	}

	template< typename FCompare> F2wayFrom3way<FCompare, tc::fn_equal_to> equalfrom3way( FCompare fnCompare ) noexcept {
		return F2wayFrom3way<FCompare, tc::fn_equal_to>( fnCompare );
	}
} // namespace tc
