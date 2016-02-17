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

#include "range_defines.h"
#include "result_of.h"
#include "accumulator.h"

#include <boost/mpl/has_xxx.hpp>
#include <boost/preprocessor.hpp>

#include <type_traits>
#include <functional>

namespace tc {

	//////////////////////////
	// break_or_continue + helper
	enum break_or_continue {
		break_,
		continue_
	};

	inline tc::break_or_continue continue_if(bool bCondition) noexcept {
		return bCondition ? tc::continue_ : tc::break_;
	}

	#define RETURN_IF_BREAK(...) if( tc::break_==(__VA_ARGS__) ) { return tc::break_; }

	//// continue_if_not_break ///////////////////////////////////////////////////////////////////////////
	// Func returns break_or_continue
	template <typename Func, typename ...Args>
	std::enable_if_t<
		std::is_same< tc::break_or_continue,
					  std::result_of_t< Func( Args... ) >
					>::value,
	tc::break_or_continue > continue_if_not_break(Func&& func, Args&& ... args) MAYTHROW {
		return std::forward<Func>(func)(std::forward<Args>(args)...);
	}

	// Func does not return break_or_continue
	template <typename Func, typename ...Args>
	std::enable_if_t< !
		std::is_same< tc::break_or_continue,
					  std::result_of_t< Func( Args... ) >
					>::value,
	tc::break_or_continue > continue_if_not_break(Func&& func, Args&& ... args) MAYTHROW {
		std::forward<Func>(func)(std::forward<Args>(args)...);
		return tc::continue_;
	}

	//// returns_void ///////////////////////////////////////////////////////////////////////////////
	// returns_void helps when changing existing for_each implementations to support continue_if_not_break
	template <typename Func, typename ...Args>
		std::enable_if_t< std::is_void< std::result_of_t< Func(Args...) > >::value,
	void > returns_void(Func&& func, Args && ... args) MAYTHROW {
		std::forward<Func>(func)(std::forward<Args>(args)...);
	}

	//// make_return_continue /////////////////////////////////////////////////////////////////////////
	template<typename Func>
	struct make_return_continue final : tc::fundamental_base< std::decay_t<Func> > {
		using result_type = tc::break_or_continue;
		make_return_continue(Func&& func) noexcept : tc::fundamental_base< std::decay_t<Func> >(std::forward<Func>(func)) {}

		template<typename... Args>
		tc::break_or_continue operator()(Args&& ... args) MAYTHROW {
			this->_get()(std::forward<Args>(args)...);
			return tc::continue_;
		}

		template<typename... Args>
		tc::break_or_continue operator()(Args&& ... args) const MAYTHROW {
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
	struct function;

	template< typename ...Args >
	struct function< tc::break_or_continue(Args...) > final : std::function< tc::break_or_continue(Args...) >{
    private:
		using base_ = std::function< tc::break_or_continue(Args...) >;
	public:
		using result_type = tc::break_or_continue;
		template< typename Func >
		function(Func&& func, std::enable_if_t< tc::is_base_of_decayed< base_, Func >::value, unused_arg> =unused_arg()) MAYTHROW
			: base_( base_cast< base_ >( std::forward<Func>(func) ) )
		{}
		template< typename Func >
		function(Func&& func, std::enable_if_t< !tc::is_base_of_decayed< base_, Func >::value && std::is_same<tc::break_or_continue, std::result_of_t< Func( Args... ) > >::value, unused_arg> =unused_arg()) MAYTHROW
			: base_( std::forward<Func>(func) )
		{}
		template< typename Func >
		function(Func&& func, std::enable_if_t< !std::is_same<tc::break_or_continue, std::result_of_t< Func( Args... ) > >::value, unused_arg> =unused_arg()) MAYTHROW
			: base_( make_return_continue<Func>( std::forward<Func>(func) ) )
		{}
	};

	// TODO: extend to more functions
	template< typename F0, typename F1 >
	void cyclic_improve(F0 f0, F1 f1) noexcept {
		int nBreakAfter=1;
		for (;;) {

			if (f0()) {
				nBreakAfter=1;
			} else if( 0==nBreakAfter ) {
				break;
			}

			if (f1()) {
				nBreakAfter=0;
			} else if( 1==nBreakAfter ) {
				break;
			}

		}
	}
}
