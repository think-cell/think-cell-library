
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <type_traits>

namespace tc {
	namespace static_castable {
		template<typename From, typename To, typename = decltype(static_cast<To>(std::declval<From>())) >
		std::true_type check(int) noexcept;
		
		template<typename From, typename To>
		std::false_type check(...) noexcept;
	}

	template<typename From, typename To>
	struct is_static_castable final : decltype(static_castable::check<From, To>(0)) {};

#pragma warning (push)
#pragma warning (disable: 4822) //  local class member function does not have a body
	namespace static_tests {
		struct S {};
		struct T : S {};
		struct U {};

		struct X {
			explicit X(T const&) noexcept;
		};

		struct Y {
			explicit operator T const&() /* no & */ noexcept;
		};

		static_assert(is_static_castable<S*, T*>::value);
		static_assert(is_static_castable<T*, S*>::value);
		static_assert(is_static_castable<T, S>::value);

		static_assert(!is_static_castable<T const*, S*>::value);
		static_assert(is_static_castable<S*, T const*>::value);

		static_assert(!is_static_castable<T, U>::value);
		static_assert(!is_static_castable<U, T>::value);

		static_assert(is_static_castable<T, X>::value);
		static_assert(is_static_castable<T const&, X>::value);
		static_assert(!is_static_castable<X, T>::value);

		static_assert(!is_static_castable<Y const, T const&>::value);
		static_assert(is_static_castable<Y, T const&>::value);
	}
#pragma warning (pop)
}
