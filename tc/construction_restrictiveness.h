
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "enum.h"
#include "minmax.h"

namespace tc {
	////////////////////////////////////////////////
	// initialization of TTarget member/element by TSource

	DEFINE_ENUM( econstruction_t, econstruction, (FORBIDDEN)(EXPLICIT)(IMPLICIT) );

	namespace no_adl {
		// Similar to http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4387.html :
		// - implicit construction == is_constructible && is_convertible,
		// - explicit construction == is_constructible && !is_convertible.
		// However, we make some unsafe conversions explicit or do not allow them at all.
		// TODO: exlicit construction, if there is a sensible definition for tc::explicit_cast<TTarget>(TSource) 
		template<typename TTarget, typename... Args>
		struct construction_restrictiveness : std::integral_constant<
			tc::econstruction_t,
			tc::is_explicit_castable<TTarget, Args...>::value
				? (
					tc::is_implicitly_constructible<TTarget, Args...>::value
						? tc::econstructionIMPLICIT
						: tc::econstructionEXPLICIT
				)
				: tc::econstructionFORBIDDEN
		> {
			static_assert(!std::is_rvalue_reference<TTarget>::value);
		};
	}
	using no_adl::construction_restrictiveness;

	namespace no_adl {
		// initialize N elements of TTarget by forwarding one arg per element
		template <typename TTarget, typename ...Args>
		struct elementwise_construction_restrictiveness final {
			static constexpr auto value = tc::min(tc::econstructionIMPLICIT, tc::construction_restrictiveness<TTarget, Args>::value...);
		};
	}
	using no_adl::elementwise_construction_restrictiveness;
}
