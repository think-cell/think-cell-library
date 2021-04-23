
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "range_fwd.h"
#include "for_each.h"
#include "as_lvalue.h"

namespace tc {
	namespace no_adl {
		///////////////////////////////////////////////////
		// tc::function_ref

		union any_ref {
			// Any pointer to function can be converted to a pointer to a different function type. Calling the
			// function through a pointer to a different function type is undefined, but converting such pointer
			// back to pointer to the original function type yields the pointer to the original function.
			// https://en.cppreference.com/w/cpp/language/reinterpret_cast

			template <typename RefT, std::enable_if_t<!std::is_function<RefT>::value>* = nullptr>
			explicit any_ref(RefT& ref) noexcept
				: m_pvRef(std::addressof(ref))
			{}

			template <typename RefT, std::enable_if_t<std::is_function<RefT>::value>* = nullptr>
			explicit any_ref(RefT& ref) noexcept
				: m_fpvRef(reinterpret_cast<void(*)()>(std::addressof(ref)))
			{}

			template <typename RefT> // may be const and/or volatile
			RefT& get_ref() const& noexcept {
				static_assert(!std::is_reference<RefT>::value); 
				if constexpr (std::is_function<RefT>::value) {
					return *reinterpret_cast<RefT*>(m_fpvRef);
				} else {
					return *static_cast<RefT*>(tc::make_mutable_ptr(m_pvRef));
				}
			}

			void const* m_pvRef;
			void (*m_fpvRef)();
		};

#ifndef _MSC_VER
		template <bool bNoExcept, typename Ret, typename ...Args>
		using type_erased_function_ptr = Ret(*)(tc::no_adl::any_ref, Args...) noexcept(bNoExcept);
#else
		// Workaround C1001: An internal error has occurred in the compiler
		template <bool bNoExcept, typename Ret, typename ...Args>
		struct get_type_erased_function_ptr;

		template <typename Ret, typename ...Args>
		struct get_type_erased_function_ptr</*bNoExcept*/true, Ret, Args...> {
			using type=Ret(*)(tc::no_adl::any_ref, Args...) noexcept;
		};

		template <typename Ret, typename ...Args>
		struct get_type_erased_function_ptr</*bNoExcept*/false, Ret, Args...> {
			using type=Ret(*)(tc::no_adl::any_ref, Args...) noexcept(false);
		};
		
		template <bool bNoExcept, typename Ret, typename ...Args>
		using type_erased_function_ptr = typename tc::no_adl::get_type_erased_function_ptr<bNoExcept, Ret, Args...>::type;
#endif

		template <bool bNoExcept, typename Func, typename Ret, typename ...Args>
		struct make_type_erased_function_ptr final {
			tc::no_adl::type_erased_function_ptr<bNoExcept, Ret, Args...> operator()() const& noexcept {
				return [](tc::no_adl::any_ref anyref, Args... args) noexcept(bNoExcept) -> Ret { // implicit cast of stateless lambda to function pointer
					return tc::invoke(anyref.get_ref<Func>(), std::forward<Args>(args)...); // MAYTHROW unless bNoExcept
				};
			}
		};

		template <bool bNoExcept, typename Func, typename ...Args>
		struct make_type_erased_function_ptr<bNoExcept, Func, void, Args...> final {
			tc::no_adl::type_erased_function_ptr<bNoExcept, void, Args...> operator()() const& noexcept {
				return [](tc::no_adl::any_ref anyref, Args... args) noexcept(bNoExcept) { // implicit cast of stateless lambda to function pointer
					tc::invoke(anyref.get_ref<Func>(), std::forward<Args>(args)...); // MAYTHROW unless bNoExcept
				};
			}
		};

		template <bool bNoExcept, typename Func, typename ...Args>
		struct make_type_erased_function_ptr<bNoExcept, Func, tc::break_or_continue, Args...> final {
			tc::no_adl::type_erased_function_ptr<bNoExcept, tc::break_or_continue, Args...> operator()() const& noexcept {
				return [](tc::no_adl::any_ref anyref, Args... args) noexcept(bNoExcept) -> tc::break_or_continue { // implicit cast of stateless lambda to function pointer
					// Not tc::continue_if_not_break because it casts sink to const&.
					return tc_internal_continue_if_not_break(tc::invoke(anyref.get_ref<Func>(), std::forward<Args>(args)...)); // MAYTHROW unless bNoExcept
				};
			}
		};

		template <bool bNoExcept, typename Ret, typename ...Args>
		struct function_ref_base {
			function_ref_base(function_ref_base const&) noexcept = default;

			Ret operator()(Args... args) const& noexcept(bNoExcept) {
				return m_pfuncTypeErased(m_anyref, std::forward<Args>(args)...); // MAYTHROW unless bNoExcept
			}
				
			template <typename Func, std::enable_if_t<
				!tc::is_base_of_decayed< function_ref_base, Func >::value
				&& tc::is_invocable<std::remove_reference_t<Func>&, Args...>::value
				&& (
					std::is_convertible<decltype(tc::invoke(std::declval<std::remove_reference_t<Func>&>(), std::declval<Args>()...)), Ret>::value
					|| std::is_same<Ret, tc::break_or_continue>::value
					|| std::is_same<Ret, void>::value
				)
			>* =nullptr>
			function_ref_base(Func&& func) noexcept
				: m_pfuncTypeErased(tc::no_adl::make_type_erased_function_ptr<bNoExcept, std::remove_reference_t<Func>, Ret, Args...>{}())
				, m_anyref(tc::as_lvalue(func))
			{
				static_assert(!std::is_member_function_pointer<Func>::value, "Raw member functions are not supported (how would you call them?).  Pass in a lambda or use std::mem_fn/TC_MEM_FN instead.");
				static_assert(!std::is_pointer<Func>::value, "Pass in functions rather than function pointers.");
				// Checking the noexcept value of the function call is commented out because MAYTHROW is widely used in generic code such as for_each, range_adaptor...
				// static_assert(!bNoExcept || noexcept(std::declval<tc::decay_t<Func>&>()(std::declval<Args>()...)));
			}

		private:
			tc::no_adl::type_erased_function_ptr<bNoExcept, Ret, Args...> m_pfuncTypeErased;
			tc::no_adl::any_ref m_anyref;
		};

		template <typename Sig>
		struct function_ref;

		template <typename Ret, typename ...Args>
		struct function_ref<Ret(Args...)> : private tc::no_adl::function_ref_base</*bNoExcept*/false, Ret, Args...> {
			using base_ = typename function_ref::function_ref_base;
			using base_::base_;
#ifndef _MSC_VER
			using base_::operator();
#else
			// Workaround C1001: An internal error has occurred in the compiler
			Ret operator()(Args... args) const& noexcept(false) {
				return base_::operator()(std::forward<Args>(args)...); // MAYTHROW
			}
#endif
		};

		template <typename Ret, typename ...Args>
		struct function_ref<Ret(Args...) noexcept> : private tc::no_adl::function_ref_base</*bNoExcept*/true, Ret, Args...> {
			using base_ = typename function_ref::function_ref_base;
			using base_::base_;
#ifndef _MSC_VER
			using base_::operator();
#else
			// Workaround C1001: An internal error has occurred in the compiler
			Ret operator()(Args... args) const& noexcept {
				return base_::operator()(std::forward<Args>(args)...);
			}
#endif
		};

		template <typename T>
		struct any_range_ref {
			using value_type = tc::decay_t<T>;

			template <typename Rng>
			any_range_ref(Rng&& rng) noexcept
				: m_pfuncTypeErased(
					[](tc::no_adl::any_ref anyrefRng, tc::no_adl::function_ref<tc::break_or_continue (T) noexcept> fn) noexcept -> tc::break_or_continue { // implicit cast of stateless lambda to function pointer
						return tc::for_each(anyrefRng.get_ref<std::remove_reference_t<Rng>>(), fn);
					}
				)
				, m_anyrefRng(tc::as_lvalue(rng))
			{}
		
			tc::break_or_continue operator()(tc::no_adl::function_ref<tc::break_or_continue (T) noexcept> fn) const& noexcept {
				return m_pfuncTypeErased(m_anyrefRng, fn);
			}

		private:
			tc::no_adl::type_erased_function_ptr</*bNoExcept*/true, tc::break_or_continue, tc::no_adl::function_ref<tc::break_or_continue (T) noexcept>> m_pfuncTypeErased;
			tc::no_adl::any_ref m_anyrefRng;
		};
	}
	using no_adl::function_ref;
	using no_adl::any_range_ref;
	using no_adl::any_ref;
}
