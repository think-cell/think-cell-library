#pragma once

#include "assign.h"
#include "return_decltype.h"
#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
#include "enum.h"
#endif
#include "Range/quantifier.h"
#include "Library/ErrorReporting/functors.h"
#include <vector>

namespace tc {
	namespace order_t_adl_barrier {
		enum class order {
			less, equal, greater, end__
		};
#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
		DEFINE_CONTIGUOUS_ENUM(order,order::less,order::end__)
#endif

		inline order operator-(order ord) {
#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
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
	template<typename T>
	typename std::enable_if<std::is_floating_point<T>::value >::type
	// specialization for wchar_t in MSVC does not have has_quiet_NAN:
	// typename std::enable_if<std::numeric_limits<T>::has_quiet_NAN || std::numeric_limits<T>::has_signaling_NAN >::type
	assert_isnan(T const& t) {
		_ASSERT( !std::isnan(t) );
	}

	template<typename T>
	typename std::enable_if<!std::is_floating_point<T>::value >::type
	// specialization for wchar_t in MSVC does not have has_quiet_NAN:
	// typename std::enable_if<!(std::numeric_limits<T>::has_quiet_NAN || std::numeric_limits<T>::has_signaling_NAN) >::type
	assert_isnan(T const& t) {}
}

// not inside tc namespace, so that compare below has no chance seeing tc::compare
namespace tc_compare_impl {
	template< typename Lhs, typename Rhs >
	tc::order compare( Lhs const& lhs, Rhs const& rhs ) {
		tc::assert_isnan(lhs);
		tc::assert_isnan(rhs);
		if( lhs < rhs ) return tc::order::less;
		else if ( rhs < lhs ) return  tc::order::greater;
		else return tc::order::equal;
	}
	template< typename Lhs, typename Rhs >
	tc::order compare_impl( Lhs const& lhs, Rhs const& rhs ) {
		return compare( lhs, rhs );
	}
}

namespace tc {
	template< typename Lhs, typename Rhs >
	tc::order compare( Lhs const& lhs, Rhs const& rhs ) {
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

	#define COMPARE_BASE( basetype ) \
		COMPARE_EXPR( tc::base_cast<basetype>(_) );

	#define COMPARE_MASK( maskmember, maskvalue ) \
		COMPARE_EXPR( tc::bool_cast((_.m_ ## maskmember) & (maskmember ## maskvalue)) )

	#define COMPARE_ASPECT( maskmember, maskvalue, member ) \
		COMPARE_MASK( maskmember, maskvalue ); \
		if( (lhs.m_ ## maskmember) & (maskmember ## maskvalue) ) COMPARE_EXPR( _.member );

	#define COMPARE_FUNCTION( func ) \
		RETURN_IF_NOT_EQUAL( tc::compare_less_3way( func )( VERIFYINITIALIZED(lhs), VERIFYINITIALIZED(rhs) ) )

	#define COMPARE_FUNCTION_REVERSE( func ) \
		RETURN_IF_NOT_EQUAL( tc::compare_less_3way( func )( VERIFYINITIALIZED(rhs), VERIFYINITIALIZED(lhs) ) )

	#define COMPARE_FUNCTION_REVERSE_IF( cond, func ) \
		RETURN_IF_NOT_EQUAL(tc::negate_if(cond,tc::compare_less_3way( func )( VERIFYINITIALIZED(lhs), VERIFYINITIALIZED(rhs) )))

	///////////////////////////////////////////////////////////
	// compare on specific types

	template< typename Lhs, typename Rhs >
	tc::order lexicographical_compare_3way( Lhs const& lhs, Rhs const& rhs ) {
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
	tc::order compare( std::pair<LFirst, LSecond> const& lhs, std::pair<RFirst, RSecond> const& rhs ) {
		COMPARE_EXPR( _.first );
		COMPARE_EXPR( _.second );
		return tc::order::equal;
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename Elem, typename Alloc, typename Rhs >
	tc::order compare( std::vector< Elem, Alloc > const& lhs, Rhs const& rhs ) {
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename Elem, typename Alloc, typename Rhs >
	tc::order compare( std::basic_string< Elem, std::char_traits<Elem>, Alloc > const& lhs, Rhs const& rhs ) {
		// should be the same as lhs.compare(rhs) which is only implemented for decltype(lhs)==Rhs
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	// keep in tc namespace to prevent overloading of this case of compare
	template< typename T, std::size_t N, typename Rhs >
	tc::order compare( std::array< T, N > const& lhs, Rhs const& rhs ) {
		return tc::lexicographical_compare_3way( lhs, rhs );
	}

	DEFINE_FN( compare );

	
}

///////////////////////////////////
// argument-wise transformation,
// add operator() with more arguments if you need it

template<typename result_type, typename Func, typename Transform>
struct transform_args_impl
{
private:
	using function_type = std::decay_t<Func>;
	using transform_type = std::decay_t<Transform>;
public:
	function_type m_func;
	transform_type m_transform;

	template< typename ...Args >
	auto operator()(Args&& ... args) const
		->result_type // TODO: replace by return_decltype as soon as compiler supports it
	{
		return m_func( m_transform( std::forward<Args>(args) )... );
	}
};

template< typename result_type, typename Func, typename Transform >
transform_args_impl<result_type, Func, Transform> transform_args(Func&& func, Transform&& transform) {
	return transform_args_impl<result_type, Func, Transform>{ std::forward<Func>(func), std::forward<Transform>(transform) };
}

///////////////////////////////////
// binary predicates

namespace tc {

template< typename Func >
auto compare_less(Func&& func)
	return_decltype( transform_args<bool>( boost::is_less(), std::forward<Func>(func) ) )

template< typename Func >
auto compare_less_3way(Func&& func)
	return_decltype( transform_args<tc::order>( tc::fn_compare(), std::forward<Func>(func) ) )

template< typename Func >
auto compare_greater(Func&& func)
	return_decltype( boost::bind<bool>( transform_args<bool>( boost::is_less(), std::forward<Func>(func) ), _2, _1 ) )

template< typename Func >
auto compare_greater_3way(Func&& func)
	return_decltype( boost::bind<tc::order>( transform_args<tc::order>( tc::fn_compare(), std::forward<Func>(func) ), _2, _1 ) )

template< typename Func >
auto compare_equal(Func&& func)
	return_decltype( transform_args<bool>( boost::is_equal(), std::forward<Func>(func) ) )

////////////////////////////////
// Provide adapter from 3-way compare to 2-way compare

template<typename FCompare, typename Base>
struct F2wayFrom3way : private Base {
private:
	FCompare m_fnCompare;
public:
	using result_type = bool;
	F2wayFrom3way() {}; // default-constructible if m_fnCompare is default-constructible, practical for using as STL container comparator template parameter
	F2wayFrom3way( FCompare fnCompare ) : m_fnCompare(fnCompare) {};
	template< typename Lhs, typename Rhs > bool operator()( Lhs&& lhs, Rhs&& rhs ) const {
		return tc::base_cast<Base>(*this)(m_fnCompare(std::forward<Lhs>(lhs), std::forward<Rhs>(rhs)), tc::order::equal);
	};
};

template< typename FCompare> F2wayFrom3way<FCompare, tc::fn_less> lessfrom3way( FCompare fnCompare ) {
	return F2wayFrom3way<FCompare, tc::fn_less>( fnCompare );
}

template< typename FCompare> F2wayFrom3way<FCompare, tc::fn_greater> greaterfrom3way( FCompare fnCompare ) {
	return F2wayFrom3way<FCompare, tc::fn_greater>( fnCompare );
}

} // namespace tc