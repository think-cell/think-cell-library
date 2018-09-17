
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include <boost/mpl/has_xxx.hpp>
#include <type_traits>

namespace tc {
	////////////////////////////////////////////////////////////////////////////////////
	// curiously recurring template patterns mapping comparison operators to compare

	BOOST_MPL_HAS_XXX_TRAIT_DEF(equality_comparable_tag)

	namespace equality_comparable_adl {
		template< typename Derived >
		struct equality_comparable {
			using equality_comparable_tag = void;
		};

		template< typename Lhs, typename Rhs, std::enable_if_t<
			has_equality_comparable_tag<Lhs>::value || has_equality_comparable_tag<Rhs>::value
		>* = nullptr >
		bool operator!=(Lhs const& lhs, Rhs const& rhs) noexcept {
			static_assert(std::is_same<bool, decltype(lhs==rhs)>::value);
			return !(lhs==rhs);
		}
	}
	using equality_comparable_adl::equality_comparable;
}
