
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/enum.h"
#include "../base/utility.h"
#include "../base/noncopyable.h"
#include "../base/derivable.h"
#include "../base/invoke.h"

#include "accumulator.h"

#include <type_traits>
#include <functional>

namespace tc {

	DEFINE_ENUM(break_or_continue, BOOST_PP_EMPTY(), (break_)(continue_))

	[[nodiscard]] inline constexpr tc::break_or_continue continue_if(tc::bool_context bCondition) noexcept {
		if( bCondition ) {
			return tc::continue_;
		} else {
			return tc::break_;
		}
	}

	#define RETURN_IF_BREAK(...) \
	{ \
		auto breakorcontinue__ = (__VA_ARGS__); \
		/* Inline constexpr bools as soon as "constexpr if" works properly in MSVC */ \
		MODIFY_WARNINGS_BEGIN(((disable)(4189))) /* diable warning C4189 to workaround VS2022 17.0 compiler bug: https://developercommunity.visualstudio.com/t/unexpected-warning-c4189-with-vs170-preview-21/1489426 */ \
		constexpr bool bAlwaysBreaks = std::is_same<decltype(breakorcontinue__), tc::constant<tc::break_>>::value; \
		constexpr bool bNeverBreaks = std::is_same<decltype(breakorcontinue__), tc::constant<tc::continue_>>::value; \
		MODIFY_WARNINGS_END \
		if constexpr (bAlwaysBreaks) { \
			return tc::constant<tc::break_>(); \
		} \
		else if constexpr (!bNeverBreaks) { \
			if( tc::break_ == breakorcontinue__ ) { \
				return tc::break_; \
			} \
		} \
	}
	
	namespace continue_if_not_break_adl {
		template<typename BreakOrContinueDefault = tc::constant<tc::continue_>>
		struct impl_t final {
			BreakOrContinueDefault m_breakorcontinue;
		};
		inline constexpr impl_t<> impl = {tc::constant<tc::continue_>()};
		template<typename BreakOrContinue>
		using is_break_or_continue = tc::type::find_unique<tc::type::list<tc::break_or_continue, tc::constant<tc::break_>, tc::constant<tc::continue_>>, BreakOrContinue>;

		template<typename BreakOrContinue> requires is_break_or_continue<BreakOrContinue>::found
		constexpr impl_t<BreakOrContinue> operator,(BreakOrContinue breakorcontinue, impl_t<> const&) noexcept {
			return {breakorcontinue};
		}
		// The built-in "operator," would be fine, but this is needed to protect against other libraries that overload "operator," (e.g. boost::proto).
		template<typename NotBreakOrContinue> requires (!is_break_or_continue<std::remove_cvref_t<NotBreakOrContinue>>::found)
		constexpr impl_t<> const& operator,(NotBreakOrContinue&& /*notbreakorcontinue*/, impl_t<> const& _) noexcept {
			return _;
		}
		// Built-in:
		//template<typename >
		//impl_t<> const& operator,(void, impl_t<> const& _) noexcept {
		//	return _;
		//}
#define tc_internal_continue_if_not_break(...) tc::decay_copy(((__VA_ARGS__), tc::continue_if_not_break_adl::impl).m_breakorcontinue)
	}

	template<typename Void> requires std::is_void<Void>::value
	constexpr Void implicit_cast(tc::constant<tc::continue_>) noexcept {}


	//////////////////////////////////////////////////////////////////////////

	//// continue_if_not_break ///////////////////////////////////////////////////////////////////////////
	// Func returns break_or_continue

	template<typename Sink, typename... Args>
	constexpr auto continue_if_not_break(Sink const& sink, Args&&... args) return_decltype_MAYTHROW(
		tc_internal_continue_if_not_break(tc::invoke(sink, std::forward<Args>(args)...))
	)

	///////////////////////////////////////////////////
	// std::function should support tc::continue_;
	//	struct Fn {
	//		int operator()(int) const {
	//			return 3;
	//		}
	//	} fn;
	//	std::function< void(int) > erased=fn; // erased(x) runs fn(x), discards the return value and returns void
	//	tc::function{_view}< tc::break_or_continue(int) > erased2=fn; // erased2(x) runs fn(x), discards the return value and returns tc::continue_
	
	namespace no_adl {

		///////////////////////////////////////////////////
		// tc::function

		// tc::function is noncopyable, but implemented on top of std::function, which requires the functor to be copyable.
		template<typename Func>
		struct movable_functor_adaptor_base : tc::derivable_t<tc::decay_t<Func>> {
		private:
			using base_t = tc::derivable_t<tc::decay_t<Func>>;
		public:
			movable_functor_adaptor_base(Func&& func) noexcept : base_t(std::forward<Func>(func)) {}
			movable_functor_adaptor_base(movable_functor_adaptor_base&&) = default; // not noexcept to "inherit" exception-specifier from base class
			movable_functor_adaptor_base(movable_functor_adaptor_base const& mfa) noexcept
				: base_t(tc_move_always(tc::as_mutable(tc::base_cast<base_t>(mfa))))
			{
				// On the Mac, the std::function move ctor may actually copy our movable_functor_adaptor.
				// Since tc::function is noncopyable, we always move here.
			}
		};

		template<typename Ret, typename Func>
		struct movable_functor_adaptor final : movable_functor_adaptor_base<Func> {
			using movable_functor_adaptor_base<Func>::movable_functor_adaptor_base;

			template<typename... Args>
			Ret operator()(Args&& ... args) & MAYTHROW {
				return tc::base_cast<tc::decay_t<Func>>(*this)(std::forward<Args>(args)...);
			}

			template<typename... Args>
			Ret operator()(Args&& ... args) const& MAYTHROW {
				return tc::base_cast<tc::decay_t<Func>>(*this)(std::forward<Args>(args)...);
			}
		};

		template<typename Func>
		struct movable_functor_adaptor<tc::break_or_continue, Func> final : movable_functor_adaptor_base<Func> {
			using movable_functor_adaptor_base<Func>::movable_functor_adaptor_base;

			template<typename... Args>
			tc::break_or_continue operator()(Args&& ... args) & MAYTHROW {
				return tc::continue_if_not_break(tc::base_cast<tc::decay_t<Func>>(*this), std::forward<Args>(args)...);
			}

			template<typename... Args>
			tc::break_or_continue operator()(Args&& ... args) const& MAYTHROW {
				return tc::continue_if_not_break(tc::base_cast<tc::decay_t<Func>>(*this), std::forward<Args>(args)...);
			}
		};

		template< bool bNoExcept, typename Ret, typename... Args >
		struct function_base: tc::noncopyable {
			function_base() noexcept {} // creates an empty function
			function_base(std::nullptr_t) noexcept {} // creates an empty function

			function_base(function_base&& func) noexcept : m_func(tc_move(func).m_func) {}
			function_base& operator=(function_base&& func) noexcept {
				m_func=tc_move(func).m_func;
				return *this;
			}

			template< typename Func > requires (!tc::is_base_of_decayed< function_base, Func >::value)
			function_base(Func&& func) MAYTHROW
				: m_func( tc::no_adl::movable_functor_adaptor<Ret, Func>( std::forward<Func>(func) ) )
			{
				static_assert(!tc::is_base_of_decayed< std::function< Ret(Args...) >, Func >::value);
				// Checking the noexcept value of the function call is commented out because
				// 1. std::ref(func)'s operator() is not noexcept
				// 2. tc::unordered_set's move ctor is not noexcept
				// 3. boost::bind overloaded operators are not noexcept
				//static_assert(!bNoExcept || noexcept(std::declval<tc::decay_t<Func>&>()(std::declval<Args>()...)));
			}

			explicit operator bool() const& noexcept { return tc::explicit_cast<bool>(m_func); }

			Ret operator()(Args ... args) const& noexcept(bNoExcept) {
				return m_func(tc_move_if_owned(args)...);
			}

		private:
			std::function< Ret(Args...) > m_func;
		};

		template< typename Signature >
		struct function;
	
		template< typename Ret, typename... Args >
		struct function< Ret(Args...) > : tc::no_adl::function_base</*bNoExcept*/false, Ret, Args...>
		{
			using base_ = typename function::function_base;
			using base_::base_;
		};

		template< typename Ret, typename... Args >
		struct function< Ret(Args...) noexcept > : tc::no_adl::function_base</*bNoExcept*/true, Ret, Args...>
		{
			using base_ = typename function::function_base;
			using base_::base_;
		};
	} // no_adl

	using no_adl::function;

	inline bool cyclic_improve_impl(int n, int& nSkipRule) noexcept {
		return false;
	}

	template<typename F0, typename... F>
	bool cyclic_improve_impl(int n, int& nSkipRule, F0& f0, F&... f) {
		if (n != nSkipRule && f0()) {
			nSkipRule = n;
			return true;
		} else {
			return cyclic_improve_impl(n+1, nSkipRule, f...);
		}
	}

	template< typename... F >
	bool cyclic_improve(F... f) noexcept {
		int nSkipRule=-1;
		while(cyclic_improve_impl(0, nSkipRule, f...)) {}
		return -1 != nSkipRule;
	}
}
