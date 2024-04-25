
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/enum.h"
#include "../base/noncopyable.h"
#include "../base/derivable.h"

#include <type_traits>
#include <functional>

namespace tc {

	TC_DEFINE_ENUM(break_or_continue, BOOST_PP_EMPTY(), (break_)(continue_))

	[[nodiscard]] inline constexpr tc::break_or_continue continue_if(tc::bool_context bCondition) noexcept {
		if( bCondition ) {
			return tc::continue_;
		} else {
			return tc::break_;
		}
	}

	#define tc_return_if_break_impl(sometimes_break_value, ...) \
	{ \
		auto boc__ = (__VA_ARGS__); \
		/* Inline constexpr bools as soon as "constexpr if" works properly in MSVC */ \
		MODIFY_WARNINGS_BEGIN(((disable)(4189))) /* diable warning C4189 to workaround VS2022 17.0 compiler bug: https://developercommunity.visualstudio.com/t/unexpected-warning-c4189-with-vs170-preview-21/1489426 */ \
		constexpr bool bAlwaysBreaks = std::is_same<decltype(boc__), tc::constant<tc::break_>>::value; \
		constexpr bool bNeverBreaks = std::is_same<decltype(boc__), tc::constant<tc::continue_>>::value; \
		MODIFY_WARNINGS_END \
		if constexpr (bAlwaysBreaks) { \
			return tc::constant<tc::break_>(); \
		} \
		else if constexpr (!bNeverBreaks) { \
			if( tc::break_ == boc__ ) { \
				return sometimes_break_value; \
			} \
		} \
	}
	#define tc_return_if_break(...) \
		tc_return_if_break_impl(tc::break_, __VA_ARGS__)
	
	namespace continue_if_not_break_adl {
		template<typename BreakOrContinueDefault = tc::constant<tc::continue_>>
		struct impl_t final {
			BreakOrContinueDefault m_boc;
		};
		inline constexpr impl_t<> impl = {tc::constant<tc::continue_>()};
		template<typename BreakOrContinue>
		using is_break_or_continue = boost::mp11::mp_set_contains<boost::mp11::mp_list<tc::break_or_continue, tc::constant<tc::break_>, tc::constant<tc::continue_>>, BreakOrContinue>;

		template<typename BreakOrContinue> requires is_break_or_continue<BreakOrContinue>::value
		constexpr impl_t<BreakOrContinue> operator,(BreakOrContinue boc, impl_t<> const&) noexcept {
			return {boc};
		}
		// The built-in "operator," would be fine, but this is needed to protect against other libraries that overload "operator," (e.g. boost::proto).
		template<typename NotBreakOrContinue> requires (!is_break_or_continue<std::remove_cvref_t<NotBreakOrContinue>>::value)
		constexpr impl_t<> const& operator,(NotBreakOrContinue&& /*notboc*/, impl_t<> const& _) noexcept {
			return _;
		}
		// Built-in:
		//template<typename >
		//impl_t<> const& operator,(void, impl_t<> const& _) noexcept {
		//	return _;
		//}
#define tc_internal_continue_if_not_break(...) tc::decay_copy(((__VA_ARGS__), tc::continue_if_not_break_adl::impl).m_boc)
	}

	template<typename Void> requires std::is_void<Void>::value
	constexpr Void implicit_cast(tc::constant<tc::continue_>) noexcept {}


	//////////////////////////////////////////////////////////////////////////

	//// continue_if_not_break ///////////////////////////////////////////////////////////////////////////
	// Func returns break_or_continue

	template <typename Sink, typename ... Args>
	concept sinkable = tc::invocable<std::remove_cvref_t<Sink> const&, Args...>;
	template <typename Sink, typename ... Args>
	concept nothrow_sinkable = tc::sinkable<Sink, Args...> && tc::nothrow_invocable<std::remove_cvref_t<Sink> const&, Args...>;

	template<typename Sink, typename... Args>
	constexpr auto continue_if_not_break(Sink const& sink, Args&&... args) return_decltype_MAYTHROW(
		tc_internal_continue_if_not_break(tc_invoke_pack(sink, tc_move_if_owned(args)))
	)

	namespace no_adl {

		///////////////////////////////////////////////////
		// tc::move_only_function

		// Supports constructing tc::move_only_function<tc::break_or_continue_(...)> from a callable with return type other than break_or_continue_.
		// TODO: Implement on top of std::move_only_function once it becomes available
		template<typename Func> requires (!std::is_final_v<tc::decay_t<Func>>) && std::move_constructible<tc::decay_t<Func>>
		struct movable_functor_adaptor_base : tc::derivable_t<tc::decay_t<Func>> {
		private:
			using base_t = tc::derivable_t<tc::decay_t<Func>>;
		public:
			movable_functor_adaptor_base(Func&& func) noexcept requires tc::safely_constructible_from<base_t, Func&&>
				: base_t(tc_move_if_owned(func)) {}
			movable_functor_adaptor_base(movable_functor_adaptor_base&&) = default; // not noexcept to "inherit" exception-specifier from base class
			movable_functor_adaptor_base(movable_functor_adaptor_base const& mfa) noexcept
				: base_t(tc_move_always(tc::as_mutable(tc::base_cast<base_t>(mfa))))
			{
				// On the Mac, the std::function move ctor may actually copy our movable_functor_adaptor.
				// Since tc::move_only_function is noncopyable, we always move here.
			}
		};

		template<typename /*Ret*/, typename Func> requires requires { typename movable_functor_adaptor_base<Func>; }
		struct movable_functor_adaptor final : movable_functor_adaptor_base<Func> {
			using movable_functor_adaptor_base<Func>::movable_functor_adaptor_base;

			// no return_decltype_..._MAYTHROW: type is still incomplete in function signature - tc::base_cast does not compile on clang
			template<typename... Args> requires requires { std::declval<tc::decay_t<Func>&>()(std::declval<Args>()...); }
			decltype(auto) operator()(Args&& ... args) & noexcept(noexcept(std::declval<tc::decay_t<Func>&>()(std::declval<Args>()...))) {
				return tc::base_cast<tc::decay_t<Func>>(*this)(tc_move_if_owned(args)...);
			}

			// no return_decltype_..._MAYTHROW: type is still incomplete in function signature - tc::base_cast does not compile on clang
			template<typename... Args> requires requires { std::declval<tc::decay_t<Func> const&>()(std::declval<Args>()...); }
			decltype(auto) operator()(Args&& ... args) const& noexcept(noexcept(std::declval<tc::decay_t<Func> const&>()(std::declval<Args>()...))) {
				return tc::base_cast<tc::decay_t<Func>>(*this)(tc_move_if_owned(args)...);
			}
		};

		template<typename Func> requires requires { typename movable_functor_adaptor_base<Func>; }
		struct movable_functor_adaptor<tc::break_or_continue, Func> final : movable_functor_adaptor_base<Func> {
			using movable_functor_adaptor_base<Func>::movable_functor_adaptor_base;

			// no return_decltype_..._MAYTHROW: type is still incomplete in function signature - tc::base_cast does not compile on clang
			template<typename... Args> requires requires { tc::continue_if_not_break(std::declval<tc::decay_t<Func>&>(), std::declval<Args>()...); }
			tc::break_or_continue operator()(Args&& ... args) & noexcept(noexcept(
				tc::continue_if_not_break(std::declval<tc::decay_t<Func>&>(), std::declval<Args>()...)
			)) {
				return tc::continue_if_not_break(tc::base_cast<tc::decay_t<Func>>(*this), tc_move_if_owned(args)...);
			}

			// no return_decltype_..._MAYTHROW: type is still incomplete in function signature - tc::base_cast does not compile on clang
			template<typename... Args> requires requires { tc::continue_if_not_break(std::declval<tc::decay_t<Func> const&>(), std::declval<Args>()...); }
			tc::break_or_continue operator()(Args&& ... args) const& noexcept(noexcept(
				tc::continue_if_not_break(std::declval<tc::decay_t<Func> const&>(), std::declval<Args>()...)
			)) {
				return tc::continue_if_not_break(tc::base_cast<tc::decay_t<Func>>(*this), tc_move_if_owned(args)...);
			}
		};

		template< bool bNoExcept, typename Ret, typename... Args >
		struct move_only_function_base: tc::noncopyable {
		private:
			std::function< Ret(Args...) > m_func;
		public:
			move_only_function_base() noexcept {} // creates an empty function
			move_only_function_base(std::nullptr_t) noexcept {} // creates an empty function

			move_only_function_base(move_only_function_base&& func) noexcept : m_func(tc_move(func).m_func) {}
			move_only_function_base& operator=(move_only_function_base&& func) noexcept {
				m_func=tc_move(func).m_func;
				return *this;
			}

			template< typename Func > requires
				(!tc::decayed_derived_from<Func, move_only_function_base>) &&
				tc::safely_constructible_from<movable_functor_adaptor<Ret, Func>, Func&&> &&
				tc::safely_constructible_from<decltype(m_func), movable_functor_adaptor<Ret, Func>>
			move_only_function_base(Func&& func) noexcept 
				: m_func(movable_functor_adaptor<Ret, Func>(tc_move_if_owned(func)))
			{
				static_assert(!tc::decayed_derived_from<Func, std::function< Ret(Args...) >>);
				// TODO: static_assert(!tc::decayed_derived_from<Func, std::move_only_function< Ret(Args...) >>);
				// Checking the noexcept value of the function call is commented out because
				// 1. std::ref(func)'s operator() is not noexcept
				// 2. tc::unordered_set's move ctor is not noexcept
				// 3. boost::bind overloaded operators are not noexcept
				//static_assert(!bNoExcept || noexcept(std::declval<tc::decay_t<Func>&>()(std::declval<Args>()...)));
			}

			explicit operator bool() const& noexcept { tc_return_cast(m_func); }

			Ret operator()(Args ... args) const& noexcept(bNoExcept) {
				return m_func(tc_move_if_owned(args)...);
			}

			friend decltype(auto) debug_output_impl(move_only_function_base const& self) noexcept {
				return self.m_func.target_type().name();
			}
		};

		template< typename Signature >
		struct move_only_function;
	
		template< typename Ret, typename... Args >
		struct move_only_function< Ret(Args...) > : tc::no_adl::move_only_function_base</*bNoExcept*/false, Ret, Args...>
		{
			using base_ = typename move_only_function::move_only_function_base;
			using base_::base_;
		};

		template< typename Ret, typename... Args >
		struct move_only_function< Ret(Args...) noexcept > : tc::no_adl::move_only_function_base</*bNoExcept*/true, Ret, Args...>
		{
			using base_ = typename move_only_function::move_only_function_base;
			using base_::base_;
		};
	} // no_adl

	using no_adl::move_only_function;

	inline bool cyclic_improve_impl(int const n, int& nSkipRule) noexcept {
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
