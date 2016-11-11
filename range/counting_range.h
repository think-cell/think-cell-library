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

#include "sub_range.h"
#include "return_decltype.h"

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#else
#pragma warning( push )
#pragma warning( disable: 4244 )
#endif
#include <boost/iterator/counting_iterator.hpp>
#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma warning( pop )
#endif

// By default, boost uses long long as counting_iterator<int>::difference_type,
// which generates C4244 level 1 compiler warnings when cast back to int.
// Most pragmatically, difference_type should reflect the type of a-b, thus counting_iterator<int>::difference_type is int,
// and counting_iterator<unsigned short (or anything else shorter than int)>::difference_type is int.

namespace tc {
	namespace default_counting_iterator_adl_barrier {
		template< typename T >
		struct delayed_iterator_difference_type {
			using type=std::make_signed_t< decltype(std::declval<T>()-std::declval<T>()) >;
		};

		template< typename T >
		struct default_counting_iterator {
	// Force counting_iterators over enums to have random_access_traversal,
	// otherwise use default traversal for type T.
			using default_traversal = std::conditional_t< std::is_enum<T>::value,
		boost::iterators::random_access_traversal_tag,
		boost::iterators::use_default
	>;

	// For random_access_iterator, use delayed_iterator_difference
	template<typename Traversal>
	using difference_type = typename boost::mpl::eval_if_c<
		std::is_same<Traversal, boost::iterators::random_access_traversal_tag>::value,
		delayed_iterator_difference_type<T>,
		boost::mpl::identity< boost::iterators::use_default >
	>::type;

	// Evaluate default traversal for type T
	using traversal = typename boost::iterators::detail::counting_iterator_base<
		T, default_traversal, difference_type<default_traversal>
	>::traversal;

	using type = boost::iterators::counting_iterator<
		T,
		traversal,
		difference_type<traversal>
	>;
		};
	}

	template<typename T>
	using counting_iterator= typename default_counting_iterator_adl_barrier::default_counting_iterator<T>::type;

	template<typename TBegin, typename TEnd>
	auto make_counting_range(TBegin const& tBegin, TEnd const& tEnd) noexcept {
		using T = tc::counting_iterator<tc::common_type_t<TBegin, TEnd> >;
		return tc::make_iterator_range(static_cast<T>(tBegin), static_cast<T>(tEnd));
	}

	namespace range_of_iterators_adl_barrier {
		template<typename Rng>
		struct range_of_iterators {
			using const_iterator=counting_iterator< decltype(boost::begin(*std::declval<tc::reference_or_value<Rng> const&>())) >;
			using iterator=counting_iterator< decltype(boost::begin(*std::declval<tc::reference_or_value<Rng>&>())) >;

			template<typename Rhs>
			range_of_iterators(aggregate_tag, Rhs&& rhs) noexcept
				: m_rng(aggregate_tag(), std::forward<Rhs>(rhs))
			{}
			
			auto begin() const& noexcept
				return_ctor(const_iterator, (boost::begin(*m_rng)))
			
			auto end() const& noexcept
				return_ctor(const_iterator, (boost::end(*m_rng)))
			
			auto begin() & noexcept
				return_ctor(iterator, (boost::begin(*m_rng)))

			auto end() & noexcept
				return_ctor(iterator, (boost::end(*m_rng)))

		private:
			tc::reference_or_value<Rng> m_rng;
		};
	}
	template<typename Rng>
	auto make_range_of_iterators(Rng&& rng) noexcept
		return_ctor( range_of_iterators_adl_barrier::range_of_iterators< view_by_value_t<Rng> >, (aggregate_tag(), std::forward<Rng>(rng)) )
}