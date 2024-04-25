
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "as_lvalue.h"
#include "move.h"
#include "type_traits_fwd.h"

namespace tc {
	//-------------------------------------------------------------------------------------------------------------------------
	// `tc::temporary<T cv, Lifetime>` wraps a temporary object that was created Lifetime functions above in the callstack from a prvalue.
	// Like a reference, `tc::temporary<T cv, Lifetime> const` is equivalent to `tc::temporary<T cv, Lifetime>`.
	// Like an rvalue reference, `tc::temporary<T cv, Lifetime> cv2&` is equivalent to `T cv&` and `tc::temporary<T cv, Lifetime> cv2&&` is equivalent to `tc::temporay<T cv, Lifetime>`.
	namespace temporary_adl {
		template <typename T, unsigned Lifetime>
		struct temporary {
			static_assert(std::is_object<T>::value);
			static_assert(!tc::instance_tn<T, temporary>);

		private:
			T&& m_ref;

			template <typename, unsigned>
			friend struct temporary;

		public:
			template <typename U> requires std::same_as<std::remove_cvref_t<U>, std::remove_cv_t<T>> && std::is_convertible<std::remove_reference_t<U>*, T*>::value
			constexpr explicit(std::is_lvalue_reference<U>::value && !std::is_const<T>::value) temporary(U&& ref) noexcept : m_ref(tc_move_always_even_const(ref)) {}
 
			template <typename U, unsigned OtherLifetime> requires std::same_as<std::remove_cvref_t<U>, std::remove_cv_t<T>> && std::is_convertible<std::remove_reference_t<U>*, T*>::value
			constexpr explicit(OtherLifetime < Lifetime) temporary(temporary<U, OtherLifetime> const& other) noexcept : m_ref(tc_move_always_even_const(other.m_ref)) {}

			constexpr operator T&() const& noexcept {
				return m_ref;
			}
			constexpr operator T&&() && noexcept { // overload needed for clang: https://godbolt.org/z/9sv9YsMsc
				return tc_move_always_even_const(m_ref);
			}
			constexpr operator T&&() const&& noexcept {
				return tc_move_always_even_const(m_ref);
			}
		};
	}
	using temporary_adl::temporary;

	namespace no_adl {
		template <typename T>
		struct remove_temporary_impl {
			using type = T;
		};
		template <typename T> requires tc::instance_tn<T, tc::temporary>
		struct remove_temporary_impl<T> {
			using type = boost::mp11::mp_first<typename tc::is_instance_tn<T, tc::temporary>::arguments>;
		};
	}
	template <typename T>
	using remove_ref_temporary_t = typename no_adl::remove_temporary_impl<std::remove_reference_t<T>>::type;

	namespace no_adl { 
		// We use a nested structure to have a single top-level instantiation for all traits.
		// E.g. for a non-temporary type, `tc::increment_lifetime_t<T>` and `tc::decrement_lifetime_t<T>` only cause one type instanitation.
		// (The temporary traits are instantiated for many different types and can easily cause compile times to explode.)
		template <typename T> struct temporary_trait_impl { template <template <typename, unsigned> typename Fn> using apply = T; }; 
		
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime>> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> const> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> volatile> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> const volatile> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
		
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime>&> { template <template <typename, unsigned> typename Fn> using apply = T&; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> const&> { template <template <typename, unsigned> typename Fn> using apply = T&; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> volatile&> { template <template <typename, unsigned> typename Fn> using apply = T&; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> const volatile&> { template <template <typename, unsigned> typename Fn> using apply = T&; }; 
		
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime>&&> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> const &&> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> volatile &&> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
		template <typename T, unsigned Lifetime> struct temporary_trait_impl<tc::temporary<T, Lifetime> const volatile&&> { template <template <typename, unsigned> typename Fn> using apply = Fn<T, Lifetime>; }; 
	} 
	using no_adl::temporary_trait_impl;

	template <typename T>
	using canonicalize_temporary_t = typename temporary_trait_impl<T>::template apply<tc::temporary>;

	template <typename T>
	constexpr auto dangling_temporary_impl = false;
	template <typename T>
	constexpr auto dangling_temporary_impl<tc::temporary<T, 0>> = true;
	template <typename T>
	concept dangling_temporary = dangling_temporary_impl<tc::canonicalize_temporary_t<T>>;

	//-------------------------------------------------------------------------------------------------------------------------
	// tc_prvalue_as_temporary: if the expression is a prvalue, materializes it and wraps the rvalue reference into tc::temporary.
	template <typename T>
	using prvalue_as_temporary_t = std::conditional_t<std::is_reference<T>::value || tc::instance_tn<T, tc::temporary>, T, tc::temporary<T, 0>>;

	#define tc_prvalue_as_temporary(...) static_cast<tc::prvalue_as_temporary_t<decltype((__VA_ARGS__))>>(__VA_ARGS__)

	//-------------------------------------------------------------------------------------------------------------------------
	// tc_return_temporary: safely return an expression that is potentially a tc::temporary from a function by decaying it when necessary.
	// tc_store_temporary: safely store a temporary in a struct
	template <typename T, unsigned Lifetime>
	using return_temporary_impl = std::conditional_t<0 == Lifetime, std::remove_cv_t<T>, tc::temporary<T, Lifetime>>;
	template <typename T>
	using return_temporary_t = typename temporary_trait_impl<T>::template apply<return_temporary_impl>;

	#define tc_return_temporary(...) return static_cast<tc::return_temporary_t<decltype((__VA_ARGS__))>>(__VA_ARGS__)

	template <typename T, unsigned Lifetime>
	using store_temporary_impl = std::remove_cv_t<T>;
	template <typename T>
	using store_temporary_t = typename temporary_trait_impl<T>::template apply<store_temporary_impl>;

	#define tc_store_temporary(...) static_cast<tc::store_temporary_t<decltype((__VA_ARGS__))>>(__VA_ARGS__)

	//-------------------------------------------------------------------------------------------------------------------------
	// tc_unwrap_temporary: turns an expression that is potentially a tc::temporary into a plain reference.
	// Use this when calling functions that cannot handle tc::temporary.
	template <typename T, unsigned Lifetime>
	using unwrap_temporary_impl = T&&;
	template <typename T>
	using unwrap_temporary_t = typename temporary_trait_impl<T>::template apply<unwrap_temporary_impl>;

	#define tc_unwrap_temporary(...) static_cast<tc::unwrap_temporary_t<decltype((__VA_ARGS__))>>(__VA_ARGS__)

	//-------------------------------------------------------------------------------------------------------------------------
	// tc_rewrap_temporary: turns an expression back into a tc::temporary after a tc_unwrap_temporary.
	// Used as `tc_rewrap_temporary(Args..., f(tc_unwrap_temporary(tc_move_if_owned(args))...))`.
	// If none of `Args... args` are `tc::temporary`, equivalent to `f(tc_move_if_owned(args)...)`.
	// Otherwise, the `tc::temporary` arguments are unwrapped and then the result re-wrapped if `f` returns an rvalue reference.
	// `Args` only need to include the arguments whose lifetime influences the lifetime of a resulting rvalue reference.
	namespace no_adl {
		template <typename Result, typename ... Args>
		struct rewrap_temporary_impl {
			using type = Result;
		};

		template <typename Result, typename ... Args> requires (tc::instance_tn<Args, tc::temporary> || ...)
		struct rewrap_temporary_impl<Result&&, Args...> {
			static constexpr unsigned min_lifetime() noexcept {
				// Hand-written implementation due to header dependencies.
				auto result = static_cast<unsigned>(-1);
				([&]{
					if constexpr (tc::instance_tn<std::remove_reference_t<Args>, tc::temporary>) {
						auto constexpr lifetime = boost::mp11::mp_second<typename tc::is_instance_tn<std::remove_reference_t<Args>, tc::temporary>::arguments>::value;
						if (lifetime < result) result = lifetime;
					}
				}(), ...);
				return result;
			}

			static_assert(!tc::instance_tn<Result, tc::temporary>, "not possible since we canonicalized, which doesn't allow rvalues to temporary");
			using type = tc::temporary<Result, min_lifetime()>;
		};
	}
	template <typename Result, typename ... Args>
	using rewrap_temporary_t = typename no_adl::rewrap_temporary_impl<tc::canonicalize_temporary_t<Result>, tc::canonicalize_temporary_t<Args>...>::type;

	#define tc_rewrap_temporary(Args, ...) static_cast<tc::rewrap_temporary_t<decltype((__VA_ARGS__)), Args>>(__VA_ARGS__)

	//-------------------------------------------------------------------------------------------------------------------------
	// tc_increment_lifetime: needs to be called before passing a tc::temporary to a function; increases the lifetime of all temporaries by one, does nothing for other types.
	// tc_decrement_lifetime: needs to be called on the result of a function that were passed tc::temporary objects; decreases the lifetime of all temporaries by one, asserting temporaries of lifetime zero, does nothing for other types.
	// If using tc_invoke, it is done for you.
	template <typename T, unsigned Lifetime>
	using increment_lifetime_impl = tc::temporary<T, Lifetime + 1>;
	template <typename T>
	using increment_lifetime_t = typename temporary_trait_impl<T>::template apply<increment_lifetime_impl>;

	template <typename T, unsigned Lifetime> requires (0 < Lifetime) // dangling temporary returned from function
	using decrement_lifetime_impl = tc::temporary<T, Lifetime - 1>;
	template <typename T>
	using decrement_lifetime_t = typename temporary_trait_impl<T>::template apply<decrement_lifetime_impl>;

	#define tc_increment_lifetime(...) static_cast<tc::increment_lifetime_t<decltype((__VA_ARGS__))>>(__VA_ARGS__)
	#define tc_decrement_lifetime(...) static_cast<tc::decrement_lifetime_t<decltype((__VA_ARGS__))>>(__VA_ARGS__)

	//-------------------------------------------------------------------------------------------------------------------------
	// Type traits specializations.
	namespace no_adl {
		template <typename Temporary, typename TTarget> requires tc::instance_tn<std::remove_reference_t<Temporary>, tc::temporary>
		struct is_safely_convertible_to_reference<Temporary, TTarget> {
			static auto constexpr value
				// temporary<T> is not safely convertible to an rvalue reference as that would throw away lifetime information
				= std::is_lvalue_reference<TTarget>::value
				// it then delegates to the safe conversion of the underlying reference to TTarget
				&& is_safely_convertible_to_reference<tc::unwrap_temporary_t<Temporary>, TTarget>::value;
		};

		// decay of a temporary<T> results in T - this also takes care of tc::common_type_t
		template <typename T, unsigned Lifetime, bool bPreventSlicing>
		struct decay<tc::temporary<T, Lifetime>, bPreventSlicing> : decay<T, bPreventSlicing> {};

		// common_reference_t of a temporary and a reference results in a temporary of the underlying common reference
		template <typename T0, typename T1> requires tc::instance_tn<std::remove_reference_t<T0>, tc::temporary> || tc::instance_tn<std::remove_reference_t<T1>, tc::temporary>
		struct common_reference_impl<T0, T1> {
			using type = tc::rewrap_temporary_t<tc::common_reference_t<tc::unwrap_temporary_t<T0>, tc::unwrap_temporary_t<T1>>, T0, T1>;
		};
	}
}
