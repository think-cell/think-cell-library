
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "index_iterator.h"
#include "assert_defs.h"
#include "meta.h"
#include "for_each.h"

#include "casts.h"
#include "static_polymorphism.h"

MODIFY_WARNINGS_BEGIN(((disable)(4146)))
// warning C4146: unary minus operator applied to unsigned type, result still unsigned
// iterator_facade::distance_from returns -iterator_facade::distance_to which causes warning C4146 if difference_type is std::size_t
#include <boost/iterator/iterator_facade.hpp> 
MODIFY_WARNINGS_END

#include <boost/range/detail/demote_iterator_traversal_tag.hpp>
#include <boost/mpl/has_xxx.hpp>

#include <type_traits>

namespace tc {

	TC_HAS_MEM_FN_XXX_TRAIT_DEF(end_index, const&);

	//////////////////////////////////////////////////////////
	// range adaptors
	//
	// Basic building block for all ranges.
	// Comes in two variations, one for generator ranges, one for iterator ranges. 
	//
	namespace range_iterator_from_index_impl {

		template<
			typename Derived,
			typename Index
		>
		struct TC_EMPTY_BASES range_iterator_from_index {
		private:
			using this_type = range_iterator_from_index;
		public:
			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			using index = Index;

			using iterator = index_iterator< Derived, false >;
			using const_iterator = index_iterator< Derived, true >;
			STATIC_VIRTUAL_CONSTEXPR(begin_index)
			STATIC_VIRTUAL_CONSTEXPR(end_index)
			STATIC_VIRTUAL_CONSTEXPR(at_end_index)
			STATIC_VIRTUAL_CONSTEXPR(increment_index)
			STATIC_VIRTUAL_CONSTEXPR(decrement_index)
			STATIC_VIRTUAL_CONSTEXPR(dereference_index)
			STATIC_VIRTUAL_CONSTEXPR(advance_index)
			STATIC_VIRTUAL_CONSTEXPR(distance_to_index)
			STATIC_VIRTUAL_CONSTEXPR(middle_point)

			constexpr const_iterator make_iterator( index idx ) const& noexcept {
				return const_iterator(tc::derived_cast<Derived>(this), tc_move(idx));
			}

			constexpr const_iterator begin() const&
				return_MAYTHROW(make_iterator(begin_index()))

			template<typename Derived_=Derived, std::enable_if_t<!has_mem_fn_end_index<Derived_>::value || !tc::is_equality_comparable<Index>::value>* = nullptr>
			constexpr end_sentinel end() const& noexcept {
				return {};
			}

			template<typename Derived_ = Derived, std::enable_if_t<has_mem_fn_end_index<Derived_>::value && tc::is_equality_comparable<Index>::value>* = nullptr>
			constexpr const_iterator end() const&
				return_MAYTHROW(make_iterator(end_index()))

			constexpr iterator make_iterator( index idx ) & noexcept {
				return iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			constexpr iterator begin() &
				return_MAYTHROW(make_iterator(begin_index()))

			template<typename Derived_ = Derived, std::enable_if_t<has_mem_fn_end_index<Derived_>::value && tc::is_equality_comparable<Index>::value>* = nullptr>
			constexpr iterator end() &
				return_MAYTHROW(make_iterator(end_index()))

			constexpr bool empty() const&
				return_MAYTHROW(at_end_index(begin_index()))

			STATIC_OVERRIDE_MOD(
				template<typename Derived_ = Derived BOOST_PP_COMMA() std::enable_if_t<has_mem_fn_end_index<Derived_>::value && tc::is_equality_comparable<Index>::value>* = nullptr>
				constexpr,
			at_end_index)(index const& idx) const&
				return_MAYTHROW(end_index() == idx)
		};
	}
	using range_iterator_from_index_impl::range_iterator_from_index;

	namespace no_adl {
		template<typename Rng>
		struct TC_EMPTY_BASES range_adaptor_base_range : private tc::reference_or_value<Rng> {
		private:
			static_assert( !std::is_rvalue_reference<Rng>::value );

		public:
			constexpr range_adaptor_base_range()=default;
			template<typename Rhs>
			constexpr range_adaptor_base_range(tc::aggregate_tag_t, Rhs&& rhs) noexcept
				: tc::reference_or_value<Rng>(tc::aggregate_tag, std::forward<Rhs>(rhs))
			{}

			constexpr decltype(auto) base_range() & noexcept { return **this; }
			constexpr decltype(auto) base_range() const& noexcept { return **this; }
			constexpr decltype(auto) base_range() && noexcept { return *std::move(*this); }
			constexpr decltype(auto) base_range() const&& noexcept { return *std::move(*this); }
			constexpr decltype(auto) base_range_best_access() const& noexcept { return this->best_access(); }

			template<ENABLE_SFINAE>
			constexpr auto base_begin_index() const& return_decltype_MAYTHROW(
				tc::begin_index(SFINAE_VALUE(this)->base_range_best_access())
			)
			template<ENABLE_SFINAE>
			constexpr auto base_end_index() const& return_decltype_MAYTHROW(
				tc::end_index(SFINAE_VALUE(this)->base_range_best_access())
			)
		};
	}
	using no_adl::range_adaptor_base_range;

	namespace generator_range_adl {
		//-------------------------------------------------------------------------------------------------------------------------
		// First generator ranges
		//
		// a generator range is any type that supports an operator() with a template parameter that is a Function that can be 
		// called with the element type of the range. 
		// The generator range should support the break_or_continue protocol

		template<typename Rng>
		struct TC_EMPTY_BASES generator_range_adaptor : tc::range_adaptor_base_range<Rng> {
			constexpr generator_range_adaptor()=default;
			using range_adaptor_base_range<Rng>::range_adaptor_base_range;
			using is_generator_range_adaptor = void;
		};

		template<typename Self, typename Sink, typename std::remove_reference_t<Self>::is_generator_range_adaptor* = nullptr>
		constexpr auto for_each_impl(Self&& self, Sink&& sink) return_decltype_MAYTHROW(
			tc::for_each(
				std::forward<Self>(self).base_range(),
				std::forward<Self>(self).adapted_sink(std::forward<Sink>(sink), /*bReverse*/std::false_type())
			)
		)

		template<typename Self, typename Sink, typename std::remove_reference_t<Self>::is_generator_range_adaptor* = nullptr>
		constexpr auto for_each_reverse_impl(Self&& self, Sink&& sink) return_decltype_MAYTHROW(
			tc::for_each(
				tc::reverse(std::forward<Self>(self).base_range()),
				std::forward<Self>(self).adapted_sink(std::forward<Sink>(sink), /*bReverse*/std::true_type())
			)
		)
	}
	using generator_range_adl::generator_range_adaptor;

	namespace no_adl {
		//-------------------------------------------------------------------------------------------------------------------------
		// iterator/index based ranges
		//
		// they derive from the generator case, because the generator interface can transparently and efficiently be added
		// to any iterator or index based range.
		//

		template<
			typename Derived 
			, typename Rng
			, typename Base = tc::range_adaptor_base_range<Rng>
			, typename MaximumTraversal = boost::iterators::random_access_traversal_tag // This is used to ensure that filter_adaptor is never random access
			, bool WithMiddlePoint = true
		>
		struct TC_EMPTY_BASES index_range_adaptor
			: Base
			, range_iterator_from_index<
				Derived,
				tc::index_t<std::remove_reference_t<Rng>>
			>
		{
		private:
			using this_type = index_range_adaptor;
			
		public:
			constexpr index_range_adaptor()=default;

			using Base::Base;
			using index = tc::index_t<std::remove_reference_t<Rng>>;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;
		private:
			STATIC_OVERRIDE_MOD(constexpr,begin_index)() const& return_MAYTHROW(
				this->base_begin_index()
			)

			STATIC_OVERRIDE_MOD(template<ENABLE_SFINAE> constexpr,end_index)() const& return_decltype_MAYTHROW(
				SFINAE_VALUE(this)->base_end_index()
			)

			STATIC_OVERRIDE_MOD(constexpr,at_end_index)(index const& idx) const& return_MAYTHROW(
				tc::at_end_index(this->base_range(),idx)
			)

			STATIC_OVERRIDE_MOD(constexpr,dereference_index)(index const& idx) & return_decltype_xvalue_by_ref_MAYTHROW(
				tc::dereference_index(this->base_range(),idx)
			)

			STATIC_OVERRIDE_MOD(constexpr,dereference_index)(index const& idx) const& return_decltype_xvalue_by_ref_MAYTHROW(
				tc::dereference_index(this->base_range(),idx)
			)

			STATIC_OVERRIDE_MOD(constexpr,increment_index)(index& idx) const& return_MAYTHROW(
				tc::increment_index(this->base_range(),idx)
			)

			STATIC_OVERRIDE_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<
						tc::has_decrement_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value &&
						boost::iterators::detail::is_traversal_at_least<MaximumTraversal BOOST_PP_COMMA() boost::iterators::bidirectional_traversal_tag>::value
					>* = nullptr
				> constexpr,
				decrement_index
			)(index& idx) const& MAYTHROW -> void {
				tc::decrement_index(this->base_range(),idx);
			}

			STATIC_OVERRIDE_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<
						tc::has_advance_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value &&
						boost::iterators::detail::is_traversal_at_least<MaximumTraversal BOOST_PP_COMMA() boost::iterators::random_access_traversal_tag>::value
					>* = nullptr
				> constexpr,
				advance_index
			)(index& idx, typename boost::range_difference<Rng>::type d) const& MAYTHROW -> void {
				tc::advance_index(this->base_range(),idx,d);
			}

			STATIC_OVERRIDE_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<
						tc::has_distance_to_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value &&
						boost::iterators::detail::is_traversal_at_least<MaximumTraversal BOOST_PP_COMMA() boost::iterators::random_access_traversal_tag>::value
					>* = nullptr
				> constexpr,
				distance_to_index
			)(index const& idxLhs, index const& idxRhs) const& noexcept {
				return tc::distance_to_index(this->base_range(),idxLhs,idxRhs);
			}

			STATIC_OVERRIDE_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<SFINAE_VALUE(WithMiddlePoint) && tc::has_middle_point<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr
				> constexpr,
			middle_point)( index & idxBegin, index const& idxEnd ) const& noexcept -> void {
				tc::middle_point(this->base_range(),idxBegin,idxEnd);
			}
		};
	}
	using no_adl::index_range_adaptor;
}
