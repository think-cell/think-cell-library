#pragma once

#include "range_defines.h"
#include "Library/Utilities/perfect_forward.h"
#include "Library/Utilities/result_of.h"
#include "Library/Utilities/conversion_traits.h"
#include "Library/Utilities/accumulator.h"

#include <boost/mpl/has_xxx.hpp>
#include <boost/mpl/logical.hpp>
#include <boost/preprocessor.hpp>
#include <boost/function.hpp>

#include <type_traits>

#ifdef CLANG
	#define HAS_VARIADIC_TEMPLATES
#endif

namespace RANGE_PROPOSAL_NAMESPACE {

	//////////////////////////
	// break_or_continue + helper
	enum break_or_continue {
		break_,
		continue_
	};

	inline break_or_continue continue_if(bool bCondition) {
		return bCondition ? continue_ : break_;
	}

#define RETURN_IF_BREAK(...) if( tc::break_==(__VA_ARGS__) ) { return tc::break_; }

#ifdef HAS_VARIADIC_TEMPLATES

	//// continue_if_void ///////////////////////////////////////////////////////////////////////////
	// Func returns break_or_continue
	template <typename Func, typename ...Ps>
	typename std::enable_if<
		std::is_same< break_or_continue,
					  typename tc::result_of< Func( Ps... ) >::type
					>::value,
	break_or_continue >::type continue_if_void( Func&& func, Ps&&... args ) {
		return std::forward<Func>(func)(std::forward<Ps>(args)...);
	}

	// Func does not return break_or_continue
	template <typename Func, typename ...Ps>
	typename std::enable_if< !
		std::is_same< break_or_continue,
					  typename tc::result_of< Func( Ps... ) >::type
					>::value,
	break_or_continue >::type continue_if_void( Func&& func, Ps&&... args ) {
		std::forward<Func>(func)(std::forward<Ps>(args)...);
		return continue_;
	}

	//// returns_void ///////////////////////////////////////////////////////////////////////////////
	// returns_void helps when changing existing for_each implementations to support continue_if_void
	template <typename Func, typename ...Ps>
	typename std::enable_if< std::is_void< typename tc::result_of< Func( Ps... ) >::type >::value,
	void >::type returns_void( Func&& func, Ps&&... args ) {
		std::forward<Func>(func)(std::forward<Ps>(args)...);
	}

	//// make_return_continue /////////////////////////////////////////////////////////////////////////
	template<typename Func>
	struct make_return_continue : public fundamental_base< typename std::decay<Func>::type > {
		typedef fundamental_base< typename std::decay<Func>::type > fundamental_base;
		typedef break_or_continue result_type;
		make_return_continue(Func&& func) : fundamental_base(std::forward<Func>(func)) {}

		template <typename ...Ps>
		result_type operator()(Ps&& ... args) {
			this->_get()( std::forward<Ps>(args)...);
			return tc::continue_;
		}

		template <typename ...Ps>
		result_type operator()(Ps&& ... args) const {
			this->_get()( std::forward<Ps>(args)...);
			return tc::continue_;
		}
	};

#else
	//// No Variadic templates ///////////////////////////////////////////////////////////////////////////////////////////////////
	// Workaround gives the same result as above using the preprocessor
	// Note: PP code was written on MSVC so it will most likely not work on other compilers with more conformant PPs

	#define CONTINUE_IF_VOID_CALL_(z,n,_) typename tc::result_of< Func(BOOST_PP_CAT(A, n)) >::type

	#define CONTINUE_IF_VOID_(z, n, _)                                                                                           \
		template< typename Func, BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename A) >                                             \
		typename boost::enable_if< std::is_same< break_or_continue,                                                              \
								   typename std::result_of< Func( BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), A) ) >::type >,			 \
		break_or_continue >::type continue_if_void( Func&& func, BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), A, && a) ) {       \
			return std::forward<Func>(func)( BOOST_PP_ENUM(BOOST_PP_INC(n), PERFECT_FORWARD_CALL_, _ ) );                        \
		}                                                                                                                        \
		template< typename Func, BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename A) >                                             \
		typename boost::disable_if< std::is_same< break_or_continue,															 \
									typename std::result_of< Func( BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), A) ) >::type >,			 \
		break_or_continue >::type continue_if_void( Func&& func, BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), A, && a) ) {       \
			std::forward<Func>(func)( BOOST_PP_ENUM(BOOST_PP_INC(n), PERFECT_FORWARD_CALL_, _ ) );                               \
			return continue_;                                                                                                    \
		}

	BOOST_PP_REPEAT( 4, CONTINUE_IF_VOID_, _ )

	#undef CONTINUE_IF_VOID_
	#undef CONTINUE_IF_VOID_CALL_

	// returns_void helps when changing existing for_each implementations to support continue_if_void
	#define RETURNS_VOID_(z, n, _)                                                                                               \
		template< typename Func, BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), typename A) >                                             \
		typename boost::enable_if< std::is_void<																				 \
			typename std::result_of< Func( BOOST_PP_ENUM_PARAMS(BOOST_PP_INC(n), A) ) >::type >,                               \
		void >::type returns_void( Func&& func, BOOST_PP_ENUM_BINARY_PARAMS(BOOST_PP_INC(n), A, && a) ) {                        \
			std::forward<Func>(func)( BOOST_PP_ENUM(BOOST_PP_INC(n), PERFECT_FORWARD_CALL_, _ ) );                               \
		}

		BOOST_PP_REPEAT( 4, RETURNS_VOID_, _ )

	#undef RETURNS_VOID_

	template<typename Func>
	struct make_return_continue : fundamental_base< typename std::decay<Func>::type > {
		typedef break_or_continue result_type;
		make_return_continue(Func&& func) : fundamental_base< typename std::decay<Func>::type >(std::forward<Func>(func)) {}

		#define PART1() \
			template<
		#define PART2() \
			> break_or_continue operator()(
		#define PART3() ) \
			{ \
				this->_get()(
		#define PART4() ); \
				return tc::continue_; \
			}
		PERFECT_FORWARD
		#undef PART1
		#undef PART2
		#undef PART3
		#undef PART4

		#define PART1() \
			template<
		#define PART2() \
			> break_or_continue operator()(
		#define PART3() ) const \
			{ \
				this->_get()(
		#define PART4() ); \
				return tc::continue_; \
			}
		PERFECT_FORWARD
		#undef PART1
		#undef PART2
		#undef PART3
		#undef PART4
	};

#endif

	///////////////////////////////////////////////////
	// std::function should support tc::continue_;
	//	struct Fn {
	//		int operator()(int) const {
	//			return 3;
	//		}
	//	} fn;
	//	std::function< void(int) > erased=fn; // erased(x) runs fn(x), discards the return value and returns void
	//	tc::function< tc::break_or_continue(int) > erased2=fn; // erased2(x) runs fn(x), discards the return value and returns tc::continue_

	template< typename Signature >
	class function;

	template< typename A0 >
	class function< break_or_continue(A0) > : public std::function< break_or_continue(A0) > {
		typedef std::function< break_or_continue(A0) > base_;
	public:
		typedef break_or_continue result_type;
		template< typename Func >
		function( Func && func, typename std::enable_if< is_base_of< base_, typename remove_cvref< Func >::type >::value, unused_arg>::type=unused_arg() )
			: base_( base_cast< base_ >( std::forward<Func>(func) ) )
		{}
		template< typename Func >
		function( Func && func, typename std::enable_if< !is_base_of< base_, typename remove_cvref< Func >::type >::value && std::is_same<break_or_continue,typename std::result_of< Func( A0 ) >::type>::value, unused_arg>::type=unused_arg() )
			: base_( std::forward<Func>(func) )
		{}
		template< typename Func >
		function( Func && func, typename std::enable_if< !std::is_same<break_or_continue,typename std::result_of< Func( A0 ) >::type>::value, unused_arg>::type=unused_arg() )
			: base_( make_return_continue<Func>( std::forward<Func>(func) ) )
		{}
	};

	template< typename A0, typename A1, typename A2 >
	class function< break_or_continue(A0,A1,A2) > : public std::function< break_or_continue(A0,A1,A2) > {
		typedef std::function< break_or_continue(A0,A1,A2) > base_;
	public:
		typedef break_or_continue result_type;
		template< typename Func >
		function( Func && func, typename std::enable_if< is_base_of< base_, typename remove_cvref< Func >::type >::value, unused_arg>::type=unused_arg() )
			: base_( base_cast< base_ >( std::forward<Func>(func) ) )
		{}
		template< typename Func >
		function( Func && func, typename std::enable_if< !is_base_of< base_, typename remove_cvref< Func >::type >::value && std::is_same<break_or_continue,typename std::result_of< Func( A0,A1,A2 ) >::type>::value, unused_arg>::type=unused_arg() )
			: base_( std::forward<Func>(func) )
		{}
		template< typename Func >
		function( Func && func, typename std::enable_if< !std::is_same<break_or_continue,typename std::result_of< Func( A0,A1,A2 ) >::type>::value, unused_arg>::type=unused_arg() )
			: base_( make_return_continue<Func>( std::forward<Func>(func) ) )
		{}
	};
}