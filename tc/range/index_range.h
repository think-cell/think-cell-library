
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/type_traits_fwd.h"
#include "../base/as_lvalue.h"
#include "../base/reference_or_value.h"
#include "../base/static_polymorphism.h"
#include "../algorithm/break_or_continue.h"
#include "../algorithm/size.h"
#include "meta.h"


MODIFY_WARNINGS_BEGIN(((disable)(4018)))
#include <boost/range/difference_type.hpp>
#include <boost/range/category.hpp>
MODIFY_WARNINGS_END

#include <boost/range/iterator_range.hpp>

#include <boost/mpl/has_xxx.hpp>

#include <type_traits>

namespace tc {

	/////////////////////////////////////////////
	// required for all index ranges
	TC_HAS_EXPR(index, (Rng), std::declval<typename std::decay_t<Rng>::tc_index>())

	namespace no_adl {
		template<typename Rng>
		struct index final {
			static_assert( !std::is_reference<Rng>::value );
			using type=tc::iterator_t<Rng>;
		};

		template<tc::has_index Rng>
		struct index<Rng> final  {
			static_assert( !std::is_reference<Rng>::value );
			using type=typename Rng::tc_index;
		};

		template<typename Rng>
		using index_t=typename index<std::remove_reference_t<Rng>>::type;
		
	}
	using no_adl::index_t;

	template<typename Rng>
		requires (!tc::has_index<Rng>) && tc::borrowed_range<Rng>
	constexpr auto begin_index(Rng&& rng) return_decltype_MAYTHROW(
		tc::begin(rng)
	)

	template<tc::has_index Rng>
	constexpr auto begin_index(Rng&& rng) return_decltype_MAYTHROW(
		rng.begin_index()
	)

	template <typename Rng>
	concept index_range
		= tc::range_with_iterators<Rng> || (tc::has_index<Rng> && requires(Rng&& rng) { tc::begin_index(tc_move_if_owned(rng)); });

	template<tc::common_range Rng>
		requires (!tc::has_index<Rng>) && tc::borrowed_range<Rng>
	constexpr auto end_index(Rng&& rng) return_decltype_MAYTHROW(
		tc::end(rng)
	)

	template<tc::has_index Rng>
	constexpr auto end_index(Rng&& rng) return_decltype_MAYTHROW(
		rng.end_index()
	)

	template<typename Rng, typename It>
		requires (!tc::has_index<Rng>)
	constexpr bool at_end_index(Rng const& rng, It const& it) return_MAYTHROW(
		it == tc::end(rng)
	)

	template<tc::has_index Rng, typename Index>
	constexpr bool at_end_index(Rng const& rng, Index const& idx) return_MAYTHROW(
		rng.at_end_index(idx)
	)

	template<typename Rng, typename It>
		requires (!tc::has_index<Rng>)
	constexpr auto dereference_index(Rng&& rng, It&& it) noexcept(noexcept(*tc_move_if_owned(it)))
		-> typename std::conditional_t<
			tc::safely_convertible_to<decltype(*std::declval<It>()), std::iter_reference_t<tc::iterator_t<Rng>>>,
			tc::type::identity<std::iter_reference_t<tc::iterator_t<Rng>>>,
			tc::decay<std::iter_reference_t<tc::iterator_t<Rng>>>
		>::type
	{
		return *tc_move_if_owned(it);
	}

	template<tc::has_index Rng, typename Index>
	constexpr auto dereference_index(Rng&& rng, Index&& idx) return_decltype_xvalue_by_ref_MAYTHROW(
		tc_move_if_owned(rng).dereference_index(tc_move_if_owned(idx))
	)

	template<typename Rng, typename It>
		requires (!tc::has_index<Rng>)
	constexpr void increment_index(Rng const&, It& it) noexcept(noexcept(++it)) {
		++it;
	}

	template<tc::has_index Rng, typename Index>
	constexpr void increment_index(Rng const& rng, Index& idx) return_MAYTHROW(
		rng.increment_index(idx)
	)

	TC_HAS_EXPR(end_index, (Rng), tc::end_index(std::declval<Rng const&>()));

	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(base_range, &)
	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(border_base_index, const&, std::declval<typename T::tc_index &>())
	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(element_base_index, const&, std::declval<typename T::tc_index &>())

	namespace no_adl {
		// By standard, the lifetime of a reference may be limited to the lifetime of the iterator ("stashing iterators").
		// Stashing is forbidden for C++17 iterators that fulfill the ForwardIterator/BidirectionalIterator concept.
		// This essentially follows from the multipass guarantee: a==b <--> *a points to the same object as *b.
		// The C++17 STL still comes with some stashing iterators (LegacyForwardIterator/LegacyBidirectionalIterator concept).
		// A notable example is std::filesystem::path::iterator. Unfortunately, neither std:: nor boost::iterator_traits allows
		// us to tell apart legacy from non-stashing iterators.
		// tc::counting_iterator are stashing, as well as the iterators of all ranges adapted from a counting range.
		template <typename Element>
		struct is_stashing_element : tc::constant<false> {};

		template<typename Rng>
		struct has_stashing_index : is_stashing_element<index_t<Rng>> {
			static_assert( !std::is_reference<Rng>::value );
		};

		template<tc::has_index Rng>
		struct has_stashing_index<Rng> : tc::constant<Rng::c_bHasStashingIndex>  {
			static_assert( !std::is_reference<Rng>::value );
		};
	}
	using no_adl::is_stashing_element;
	using no_adl::has_stashing_index;

	/////////////////////////////////////////////
	// stable index range
	// If `tc::stable_index_on_move<Rng>`, `Rng` has indices and an index is not invalidated when the `Rng` object is moved.
	// This is true for:
	// * borrowed ranges, where the iterator indices are completely decoupled from `Rng` anyway
	// * `std::vector` (but not `std::string` due to SSO)
	// * ...
	template <typename Rng>
	constexpr auto enable_stable_index_on_move = false;

	template <typename Rng>
	concept stable_index_on_move
		= tc::index_range<Rng> && (std::is_lvalue_reference<Rng>::value || enable_stable_index_on_move<std::remove_cvref_t<Rng>>);

	template <typename Rng> requires tc::borrowed_range<Rng>
	constexpr auto enable_stable_index_on_move<Rng> = true;
	template <typename T, typename Alloc>
	constexpr auto enable_stable_index_on_move<std::vector<T, Alloc>> = true; // end iterator is not guaranteed by the standard but we assume it's still valid

	/////////////////////////////////////////////
	// bidirectional index ranges

	template<typename Rng, typename It>
		requires (!tc::has_index<Rng>)
	constexpr auto decrement_index(Rng const&, It& it) return_decltype_MAYTHROW(
		void(--it)
	)

	template<tc::has_index Rng, typename Index>
	constexpr auto decrement_index(Rng const& rng, Index& idx) return_decltype_MAYTHROW(
		rng.decrement_index(idx)
	)

	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(decrement_index, const&, std::declval<typename T::tc_index &>())
	TC_HAS_EXPR(decrement_index, (Rng), tc::decrement_index(std::declval<Rng const&>(), std::declval<index_t<Rng>&>()))

	/////////////////////////////////////////////
	// index ranges with distance

	template<typename Rng, typename ItLhs, typename ItRhs>
		requires (!tc::has_index<Rng>)
	constexpr auto distance_to_index(Rng const&, ItLhs const& itLhs, ItRhs const& itRhs) return_decltype_MAYTHROW(
		itRhs - itLhs
	)

	template<tc::has_index Rng, typename IndexLhs, typename IndexRhs>
	constexpr auto distance_to_index(Rng const& rng, IndexLhs const& idxLhs, IndexRhs const& idxRhs) return_decltype_MAYTHROW(
		rng.distance_to_index(idxLhs,idxRhs)
	)

	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(distance_to_index, const&, std::declval<typename T::tc_index const&>(), std::declval<typename T::tc_index const&>())
	TC_HAS_EXPR(distance_to_index, (Rng), tc::distance_to_index(std::declval<Rng const&>(), std::declval<index_t<Rng> const&>(), std::declval<index_t<Rng> const&>()));

	/////////////////////////////////////////////
	// random access index ranges

	template<typename Rng, typename It, typename Difference>
		requires (!tc::has_index<Rng>)
	constexpr void advance_index(Rng const&, It& it, Difference&& d) MAYTHROW {
		it += tc_move_if_owned(d);
	}

	template<tc::has_index Rng, typename Index, typename Difference>
	constexpr auto advance_index(Rng const& rng, Index& idx, Difference&& d) return_decltype_MAYTHROW(
		rng.advance_index(idx, tc_move_if_owned(d))
	)

	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(advance_index, const&, std::declval<typename T::tc_index &>(), std::declval<T const&>().distance_to_index(std::declval<typename T::tc_index const&>(), std::declval<typename T::tc_index const&>()))
	TC_HAS_EXPR(advance_index, (Rng), tc::advance_index(std::declval<Rng const&>(), std::declval<index_t<Rng> &>(), tc::distance_to_index(std::declval<Rng const&>(), std::declval<index_t<Rng> const&>(), std::declval<index_t<Rng> const&>())));


	/////////////////////////////////////////////
	// contiguous index ranges

	template<typename Rng, typename It>
#ifdef _LIBCPP_VERSION
		requires (!tc::has_index<Rng>) && tc::contiguous_range_detail::contiguous_iterator<std::decay_t<It>>
#else
		requires (!tc::has_index<Rng>) && std::contiguous_iterator<std::decay_t<It>>
#endif
	constexpr auto index_to_address(Rng&&, It&& it) MAYTHROW {
		return std::to_address(tc_move_if_owned(it));
	}

	template<tc::has_index Rng, typename Index>
	constexpr auto index_to_address(Rng&& rng, Index&& idx) return_decltype_MAYTHROW(
		tc_move_if_owned(rng).index_to_address(tc_move_if_owned(idx))
	)

	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(index_to_address, &&, std::declval<typename T::tc_index>())
	TC_HAS_EXPR(index_to_address, (Rng), tc::index_to_address(std::declval<Rng&&>(), std::declval<index_t<Rng>&&>()))

	/////////////////////////////////////////////
	// index ranges with middle point
	namespace iterator {
		#ifdef __clang__
			#pragma clang diagnostic push
			#pragma clang diagnostic ignored "-Wundefined-inline"
			#pragma clang diagnostic ignored "-Wundefined-internal"
		#endif
		template<typename It> constexpr It middle_point(It const&, It const&) noexcept;
		#ifdef __clang__
			#pragma clang diagnostic pop
		#endif
	}

	template<typename Rng, typename ItLhs, typename ItRhs>
		requires (!tc::has_index<Rng>)
	constexpr void middle_point(Rng const&, ItLhs& itLhs, ItRhs const& itRhs) MAYTHROW {
		itLhs = tc::iterator::middle_point(tc::as_const(itLhs), itRhs);
	}

	template<tc::has_index Rng, typename IndexLhs, typename IndexRhs >
	constexpr auto middle_point(Rng const& rng, IndexLhs& idxLhs, IndexRhs const& idxRhs) return_decltype_MAYTHROW(
		rng.middle_point(idxLhs,idxRhs)
	)

	TC_HAS_MEM_FN_XXX_CONCEPT_DEF(middle_point, const&, std::declval<typename T::tc_index &>(), std::declval<typename T::tc_index const&>());
	TC_HAS_EXPR(middle_point, (Rng), tc::middle_point(std::declval<Rng const&>(), std::declval<index_t<Rng> &>(), std::declval<index_t<Rng> const&>()));

	/////////////////////////////////////////////
	// make_iterator
	template<typename Rng, typename It > requires (!tc::has_index< std::remove_reference_t<Rng> >)
	constexpr decltype(auto) make_iterator(Rng&&, It&& it) noexcept {
		return tc_move_if_owned(it);
	}

	template<typename Rng, typename Index > requires tc::has_index< std::remove_reference_t<Rng> >
	constexpr decltype(auto) make_iterator(Rng&& rng, Index&& idx) noexcept {
		return tc_move_if_owned(rng).make_iterator(tc_move_if_owned(idx));
	}
}
