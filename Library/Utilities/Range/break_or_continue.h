#pragma once

#include "range_defines.h"
#include "Library/Utilities/result_of.h"
#include "Library/Utilities/conversion_traits.h"
#include "Library/Utilities/accumulator.h"

#include <boost/mpl/has_xxx.hpp>
#include <boost/preprocessor.hpp>

#include <type_traits>
#include <functional>

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

	//// continue_if_void ///////////////////////////////////////////////////////////////////////////
	// Func returns break_or_continue
	template <typename Func, typename ...Args>
	typename std::enable_if<
		std::is_same< break_or_continue,
					  typename tc::result_of< Func( Args... ) >::type
					>::value,
	break_or_continue >::type continue_if_void( Func&& func, Args&& ... args ) {
		return std::forward<Func>(func)(std::forward<Args>(args)...);
	}

	// Func does not return break_or_continue
	template <typename Func, typename ...Args>
	typename std::enable_if< !
		std::is_same< break_or_continue,
					  typename tc::result_of< Func( Args... ) >::type
					>::value,
	break_or_continue >::type continue_if_void( Func&& func, Args&& ... args ) {
		std::forward<Func>(func)(std::forward<Args>(args)...);
		return continue_;
	}

	//// returns_void ///////////////////////////////////////////////////////////////////////////////
	// returns_void helps when changing existing for_each implementations to support continue_if_void
	template <typename Func, typename ...Args>
		typename std::enable_if< std::is_void< typename tc::result_of< Func(Args...) >::type >::value,
	void >::type returns_void(Func&& func, Args && ... args) {
		std::forward<Func>(func)(std::forward<Args>(args)...);
	}

	//// make_return_continue /////////////////////////////////////////////////////////////////////////
	template<typename Func>
	struct make_return_continue : fundamental_base< typename std::decay<Func>::type > {
		typedef break_or_continue result_type;
		make_return_continue(Func&& func) : fundamental_base< typename std::decay<Func>::type >(std::forward<Func>(func)) {}

		template<typename... Args>
		break_or_continue operator()(Args&& ... args) {
			this->_get()(std::forward<Args>(args)...);
			return tc::continue_;
		}

		template<typename... Args>
		break_or_continue operator()(Args&& ... args) const {
			this->_get()(std::forward<Args>(args)...);
			return tc::continue_;
		}
	};

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

	template< typename ...Args >
	class function< break_or_continue(Args...) > : public std::function< break_or_continue(Args...) >{
		typedef std::function< break_or_continue(Args...) > base_;
	public:
		typedef break_or_continue result_type;
		template< typename Func >
		function( Func && func, typename std::enable_if< std::is_base_of< base_, typename std::decay< Func >::type >::value, unused_arg>::type=unused_arg() )
			: base_( base_cast< base_ >( std::forward<Func>(func) ) )
		{}
		template< typename Func >
		function( Func && func, typename std::enable_if< !std::is_base_of< base_, typename std::decay< Func >::type >::value && std::is_same<break_or_continue,typename std::result_of< Func( Args... ) >::type>::value, unused_arg>::type=unused_arg() )
			: base_( std::forward<Func>(func) )
		{}
		template< typename Func >
		function( Func && func, typename std::enable_if< !std::is_same<break_or_continue,typename std::result_of< Func( Args... ) >::type>::value, unused_arg>::type=unused_arg() )
			: base_( make_return_continue<Func>( std::forward<Func>(func) ) )
		{}
	};
}
