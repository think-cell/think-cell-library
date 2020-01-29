
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "generic_macros.h"
#include <type_traits>
#include <boost/preprocessor/variadic/to_seq.hpp>

namespace tc {
	////////////////////////////////////////////////////////////////////////////////////
	// curiously recurring template patterns mapping comparison operators to compare
	namespace equality_comparable_adl {
		// operator!= below will be found by ADL for classes that derive from tc::equality_comparable.
		// On the downside, ADL will also kick in, for example, if such a class is being used as a template argument to another class template.
		// We certainly want to restrict our generic operator!= to calls where at least one of the two arguments derives from tc::equality_comparable.
		// Now there are a number of things to be considered:
		// - equality_comparable is a template (see below), so enable_if_t<is_base_of<...,Lhs>||is_base_of<...,Rhs>> cannot be used directly as a trait,
		// - trying to match arguments of type equality_comparable<T> const& does not work in all cases, because
		//		- the base class may not be accessible, or,
		//		- there may be multiple base classes deriving from different instantiations of equality_comparable, making the call ambigous.

		template <typename Lhs, typename Rhs>
		void ambiguous_if_equality_comparable(Lhs const&, Rhs const&) noexcept; // function call will be ambigous if (at least) one of Lhs or Rhs derives from (at least one) instance of equality_comparable

		template<typename Derived> // template to preserve empty base class optimization, if first member of Derived is also equality_comparable. Also avoids warning C4584 in case of multiple inheritance.
		struct equality_comparable {
			template <typename Lhs, typename Rhs>
			friend typename std::enable_if_t<
				std::is_base_of<equality_comparable, Lhs>::value || std::is_base_of<equality_comparable, Rhs>::value, int
			> ambiguous_if_equality_comparable(Lhs const&, Rhs const&) noexcept {
				// Need to provide an implementation, otherwise MSVC will not treat it as a separate function.
				static_assert(!std::is_same<Lhs,Lhs>::value, "function declaration is used for making overload resolution fail, there is no reason to instantiate this method.");
				return 0;
			}
		};
		
		template <typename Lhs, typename Rhs, typename = void>
		struct check_equality_comparable : std::true_type {};
		
		template <typename Lhs, typename Rhs>
		struct check_equality_comparable<Lhs, Rhs, decltype(ambiguous_if_equality_comparable(std::declval<Lhs>(), std::declval<Rhs>()))> : std::false_type {};
		
		template<typename Lhs, typename Rhs, std::enable_if_t<check_equality_comparable<Lhs, Rhs>::value>* = nullptr>
		[[nodiscard]] constexpr auto operator!=(Lhs const& lhs, Rhs const& rhs) noexcept -> decltype(lhs==rhs) {
			STATICASSERTSAME(bool, decltype(lhs==rhs));
			return !(lhs==rhs);
		}
	}
	using equality_comparable_adl::equality_comparable;
}

#define EQUAL_MEMBER_IMPL(member) (lhs.member == rhs.member)
#define EQUAL_MEMBERS(...) (PP_DELIMIT_TRANSFORMED_SEQ(EQUAL_MEMBER_IMPL, &&, BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__)))
