
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"

#include <boost/range/iterator.hpp>
#include <boost/range/detail/demote_iterator_traversal_tag.hpp>

namespace tc {
	template<typename Rng>
	using traversal_t =
		typename boost::iterator_traversal<
			typename boost::range_iterator<Rng>::type
		>::type
	;

	template<typename... traversal_tags>
	struct demote_iterator_traversal_tag;

	template<typename traversal_tag1, typename traversal_tag2>
	struct demote_iterator_traversal_tag<traversal_tag1, traversal_tag2> : boost::range_detail::demote_iterator_traversal_tag<traversal_tag1, traversal_tag2> {
	};

	template<typename head, typename... tail>
	struct demote_iterator_traversal_tag<head, tail...> : boost::range_detail::demote_iterator_traversal_tag<head, typename demote_iterator_traversal_tag<tail...>::type> {
	};

	template<typename... Tags>
	using demote_iterator_traversal_tag_t = typename demote_iterator_traversal_tag<Tags...>::type;
}