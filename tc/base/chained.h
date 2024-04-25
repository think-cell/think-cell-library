
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "invoke.h"

namespace tc {
	namespace no_adl {
		// Function pointers have disadvantages over function objects, in particular when being aggregated in wrapper objects:
		// - Invokation through a function pointer usually results in an extra indirection (like a virtual function call),
		//   unless the compiler can figure out the address is constant over the entire lifetime of the wrapper. Note that
		//   this is hard to prove for the compiler: The type of the wrapper is independent of the function address, so they
		//   might be assigned from wrappers of the same type, storing pointers to different addresses.
		// - Many wrappers in our library support default construction (which is useful when used with STL or Boost containers).
		//   Default construction would leave a function pointer unintitialized!
		template<typename T>
		struct verify_functor final{
			// TODO: We would like to verify specifically that T is a class or lambda, but unfortunately
			// there is no trait to test for the latter. For now, just blacklist (function-)pointers here,
			// as other types are not callable, anyway.
			static_assert(!std::is_pointer<T>::value, "Do not pass raw function pointer as functor. Use tc_fn instead.");
			using type=T;
		};

		template<typename T>
		using verify_functor_t=typename verify_functor<T>::type;
	}
	using no_adl::verify_functor_t;

	namespace no_adl {
		template <typename FuncSecond, typename FuncFirst>
		struct [[nodiscard]] chained_impl /*not final*/ {
			tc::verify_functor_t<tc::decay_t<FuncSecond>> m_funcSecond;
			tc::verify_functor_t<tc::decay_t<FuncFirst>> m_funcFirst;

			template <typename... Args>
			constexpr auto operator()(Args&&... args) const& return_decltype_allow_xvalue_slow_MAYTHROW(
				tc_invoke(m_funcSecond, tc_invoke_pack(m_funcFirst, tc_move_if_owned(args)))
			)

			constexpr auto inverted() const& MAYTHROW {
				return chained_impl<decltype(m_funcFirst.inverted()), decltype(m_funcSecond.inverted())>{
					m_funcFirst.inverted(),
					m_funcSecond.inverted()
				};
			}

			using is_transparent = void;
		};
	}

	template <typename FuncSecond, typename FuncFirst>
	constexpr auto chained(FuncSecond&& funcSecond, FuncFirst&& funcFirst) noexcept {
		return no_adl::chained_impl<FuncSecond, FuncFirst>{tc_move_if_owned(funcSecond), tc_move_if_owned(funcFirst)};
	}
}
