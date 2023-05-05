
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/explicit_cast_fwd.h"
#include <boost/range/has_range_iterator.hpp>
#include <type_traits>

namespace tc {
	template<typename Rng>
	concept range_with_iterators = boost::has_range_iterator<Rng>::value;

	namespace no_adl {
		template<typename It>
		struct iterator_base;
	}
	using no_adl::iterator_base;

	namespace subrange_adl {
		template< typename Rng >
		struct subrange;
	}
	using subrange_adl::subrange;

	namespace transform_adaptor_adl {
		template< typename Func, typename Rng, bool HasIterator=tc::range_with_iterators< Rng > >
		struct transform_adaptor;
	}
	using transform_adaptor_adl::transform_adaptor;

	namespace no_adl {
		template< typename Pred, typename Rng, bool HasIterator=tc::range_with_iterators< Rng > >
		struct filter_adaptor;
	}
	using no_adl::filter_adaptor;

	namespace no_adl {
		template< typename Pred, typename Rng, bool HasIterator=tc::range_with_iterators< Rng > >
		struct take_while_adaptor;
	}
	using no_adl::take_while_adaptor;

	namespace no_adl {
		struct identity;
	}
	using no_adl::identity;

	namespace no_adl {
		template< typename Rng>
		struct make_subrange_result;
	}
	template<typename Rng>
	using make_subrange_result_t = typename no_adl::make_subrange_result<Rng>::type;

	namespace reverse_adaptor_adl {
		template<typename Rng, bool HasIterators = tc::range_with_iterators<Rng>>
		struct reverse_adaptor;
	}
	using reverse_adaptor_adl::reverse_adaptor;

	template<typename Rng>
	constexpr reverse_adaptor<Rng> reverse(Rng&& rng) noexcept;

	namespace no_adl {
		template<typename Rng>
		struct constexpr_size_impl;
	}

	namespace empty_range_adl {
		struct empty_range;
	}
	using empty_range_adl::empty_range;
}
