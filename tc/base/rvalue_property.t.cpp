
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#include "assert_defs.h"
#include "rvalue_property.h"
#include "type_traits.h"
#include "../unittest.h"

namespace {
	struct A final {
		template<typename Self>
		static decltype(auto) test_(Self&& self) noexcept {
			return (tc_move_if_owned(self).i);
		}
		int i;
		
		RVALUE_THIS_OVERLOAD_CONST(test);
	};

	STATICASSERTSAME(decltype(std::declval<A>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<A const>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<A&&>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<A const&&>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<A&>().test()), int const&);
	STATICASSERTSAME(decltype(std::declval<A const&>().test()), int const&);

	
	struct B final {
		template<typename Self>
		static decltype(auto) test_(Self&& self) noexcept {
			return (tc_move_if_owned(self).i);
		}
		int i;
		
		RVALUE_THIS_OVERLOAD_MOVABLE(test);
	};
	
	STATICASSERTSAME(decltype(std::declval<B>().test()), int&&);
	STATICASSERTSAME(decltype(std::declval<B const>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<B&&>().test()), int&&);
	STATICASSERTSAME(decltype(std::declval<B const&&>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<B&>().test()), int const&);
	STATICASSERTSAME(decltype(std::declval<B const&>().test()), int const&);
	
	struct C final {
		template<typename Self>
		static decltype(auto) test_(Self&& self) noexcept {
			return (tc_move_if_owned(self).i);
		}
		int i;
		
		RVALUE_THIS_OVERLOAD_MOVABLE_MUTABLE_REF(test);
	};
	
	STATICASSERTSAME(decltype(std::declval<C>().test()), int&&);
	STATICASSERTSAME(decltype(std::declval<C const>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<C&&>().test()), int&&);
	STATICASSERTSAME(decltype(std::declval<C const&&>().test()), int const&&);
	STATICASSERTSAME(decltype(std::declval<C&>().test()), int&);
	STATICASSERTSAME(decltype(std::declval<C const&>().test()), int const&);
}
