
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "enum.h"
#include "utility.h"
#include "../algorithm/minmax.h"

namespace tc {
	////////////////////////////////////////////////
	// initialization of TTarget member/element by TSource

	DEFINE_ENUM( econstruction_t, econstruction, (FORBIDDEN)(EXPLICIT)(IMPLICIT) );

	namespace construction_restrictiveness_detail {
		// Similar to http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4387.html :
		// - implicit construction == is_constructible && is_convertible,
		// - explicit construction == is_constructible && !is_convertible.
		// However, we make some unsafe conversions explicit or do not allow them at all.
		// Moreover, we always allow implicit construction if TTarget is constructed from a TTarget prvalue.
		// TODO: explicit construction, if there is a sensible definition for tc::explicit_cast<TTarget>(TSource) 
		template<typename TTarget, typename... Args>
		consteval tc::econstruction_t get_construction_restrictiveness() {
			static_assert(!std::is_rvalue_reference<TTarget>::value);
			if constexpr( 1 == sizeof...(Args) ) {
				if constexpr( std::is_same<std::remove_cv_t<TTarget>, std::remove_cv_t<tc::type::only_t<tc::type::list<Args...>>>>::value ) {
					return tc::econstructionIMPLICIT;
				}
			}
			if constexpr( tc::is_explicit_castable<TTarget, Args...>::value ) {
				return tc::is_implicitly_constructible<TTarget, Args...>::value
					? tc::econstructionIMPLICIT
					: tc::econstructionEXPLICIT;
			} else {
				return tc::econstructionFORBIDDEN;
			}
		}

		namespace no_adl {
			// MSVC up to 16.9 compilation fails if this is an alias template instead of a class template.
			template<typename TTarget, typename... Args>
			struct construction_restrictiveness : tc::constant<construction_restrictiveness_detail::get_construction_restrictiveness<TTarget, Args...>()> {};
		}
	}
	using construction_restrictiveness_detail::no_adl::construction_restrictiveness;

	namespace no_adl {
		// initialize N elements of TTarget by forwarding one arg per element
		template<typename TTarget, typename... Args>
		struct elementwise_construction_restrictiveness;

		template<tc::econstruction_t econstruction, typename TTarget, typename... Args>
		struct elementwise_construction_restrictiveness_impl final {
			static constexpr auto value = tc::min(econstruction, elementwise_construction_restrictiveness<TTarget, Args...>::value);
		};

		template<typename TTarget, typename... Args>
		struct elementwise_construction_restrictiveness_impl<tc::econstructionFORBIDDEN, TTarget, Args...> final {
			static constexpr auto value = tc::econstructionFORBIDDEN;
		};

		template<typename TTarget>
		struct elementwise_construction_restrictiveness<TTarget> final {
			static constexpr auto value = tc::econstructionIMPLICIT;
		};

		template<typename TTarget, typename Arg0, typename... Args>
		struct elementwise_construction_restrictiveness<TTarget, Arg0, Args...> final {
			static constexpr auto value = elementwise_construction_restrictiveness_impl<tc::construction_restrictiveness<TTarget, Arg0>::value, TTarget, Args...>::value;
		};
	}
	using no_adl::elementwise_construction_restrictiveness;
}
