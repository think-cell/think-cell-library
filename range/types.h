//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"

#include <boost/range/iterator.hpp>
#include <boost/range/detail/demote_iterator_traversal_tag.hpp>

namespace tc {
	template<typename Rng>
	using traversal_t =
		typename boost::iterator_traversal<
			typename boost::range_iterator<std::remove_reference_t<Rng>>::type
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