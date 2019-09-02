
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "bool_cast.h"
#include <boost/range/has_range_iterator.hpp>
#include <type_traits>

namespace tc {
	namespace no_adl {
		template< typename Rng >
		struct is_range_with_iterators : std::integral_constant< bool,
			boost::has_range_iterator<Rng>::value
		> {};
	}
	using no_adl::is_range_with_iterators;

	namespace no_adl {
		template<typename It, typename ConstIt>
		struct iterator_base;
	}
	using no_adl::iterator_base;

	namespace sub_range_adl {
		template< typename Rng >
		struct sub_range;
	}
	using sub_range_adl::sub_range;

	namespace no_adl {
		template< typename Func, typename Rng, bool HasIterator=is_range_with_iterators< Rng >::value >
		struct transform_adaptor;
	}
	using no_adl::transform_adaptor;

	namespace no_adl {
		template< typename Rng, typename Enable=void >
		struct make_sub_range_result;
	}
	template<typename Rng>
	using make_sub_range_result_t = typename no_adl::make_sub_range_result<Rng>::type;

	struct bool_context final {
		template< typename T >
		bool_context(T const& t) noexcept
			: m_b(tc::bool_cast(t))
		{}
		operator bool() const& noexcept { return m_b; }
	private:
		bool m_b;
	};
}
