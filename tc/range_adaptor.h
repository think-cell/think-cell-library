
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "index_iterator.h"
#include "range_defines.h"
#include "meta.h"
#include "types.h"
#include "for_each.h"

#include "casts.h"
#include "static_polymorphism.h"

#pragma warning( push )
#pragma warning( disable: 4146 )
// warning C4146: unary minus operator applied to unsigned type, result still unsigned
// iterator_facade::distance_from returns -iterator_facade::distance_to which causes warning C4146 if difference_type is std::size_t
#include <boost/iterator/iterator_facade.hpp> 
#pragma warning( pop )

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
			typename Index,
			typename Traversal
		>
		struct range_iterator_from_index {
		private:
			using this_type = range_iterator_from_index;
		public:
			static_assert( boost::iterators::detail::is_iterator_traversal<Traversal>::value );
			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			using index = Index;

			using iterator = index_iterator< Derived, Traversal, false >;
			using const_iterator = index_iterator< Derived, Traversal, true >;

			STATIC_VIRTUAL_CONSTEXPR(begin_index)
			STATIC_VIRTUAL_CONSTEXPR(end_index)
			STATIC_VIRTUAL_CONSTEXPR(at_end_index)
			STATIC_VIRTUAL_CONSTEXPR(increment_index)
			STATIC_VIRTUAL_CONSTEXPR(decrement_index)
			STATIC_VIRTUAL_CONSTEXPR(dereference_index)
			STATIC_VIRTUAL_CONSTEXPR(equal_index)
			STATIC_VIRTUAL_CONSTEXPR(advance_index)
			STATIC_VIRTUAL_CONSTEXPR(distance_to_index)
			STATIC_VIRTUAL_CONSTEXPR(middle_point)

			constexpr const_iterator make_iterator( index idx ) const& noexcept {
				return const_iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			constexpr const_iterator begin() const& MAYTHROW {
				return make_iterator(begin_index());
			}

			template<typename Derived_=Derived, std::enable_if_t<!has_mem_fn_end_index<Derived_>::value || !has_mem_fn_equal_index<Derived_>::value>* = nullptr>
			constexpr end_sentinel end() const& noexcept {
				return {};
			}

			template<typename Derived_ = Derived, std::enable_if_t<has_mem_fn_end_index<Derived_>::value && has_mem_fn_equal_index<Derived_>::value>* = nullptr>
			constexpr const_iterator end() const& MAYTHROW {
				return make_iterator(end_index());
			}

			constexpr iterator make_iterator( index idx ) & noexcept {
				return iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			constexpr iterator begin() & MAYTHROW {
				return make_iterator(begin_index());
			}

			template<typename Derived_ = Derived, std::enable_if_t<has_mem_fn_end_index<Derived_>::value && has_mem_fn_equal_index<Derived_>::value>* = nullptr>
			constexpr iterator end() & MAYTHROW {
				return make_iterator(end_index());
			}

			constexpr bool empty() const& MAYTHROW {
				return at_end_index(
					begin_index()
				);
			}

			STATIC_OVERRIDE_MOD(constexpr,at_end_index)(index const& idx) const& MAYTHROW -> bool {
				return equal_index(idx, end_index());
			}
		};

		template<
			typename Derived,
			typename Index,
			typename Traversal
		>
		struct range_iterator_generator_from_index
			: tc::range_generator_from_index<Derived,
				range_iterator_from_index<Derived, Index, Traversal>
			>
		{};
	}
	using range_iterator_from_index_impl::range_iterator_from_index;
	using range_iterator_from_index_impl::range_iterator_generator_from_index;

	namespace no_adl {
		struct range_adaptor_access final {
			template< typename Derived, typename... Args>
			auto operator()(Derived&& derived, Args&& ... args) /* no & */ MAYTHROW return_decltype(
				std::forward<Derived>(derived).apply(std::forward<Args>(args)...) // MAYTHROW
			)
		};

		template<
			typename Derived 
			, typename Rng
			, typename Traversal=boost::iterators::use_default
			, bool HasIterator=is_range_with_iterators< Rng >::value
		>
		struct range_adaptor;

		//-------------------------------------------------------------------------------------------------------------------------
		// First generator ranges
		//
		// a generator range is any type that supports an operator() with a template parameter that is a Function that can be 
		// called with the element type of the range. 
		// The generator range should support the break_or_continue protocol

		template<
			typename Derived 
			, typename Rng
			, typename Traversal
		>
		struct range_adaptor<
			Derived 
			, Rng
			, Traversal
			, false
		> {
			static_assert( !std::is_rvalue_reference<Rng>::value );
			reference_or_value< Rng > m_baserng;
		
		// protected:
			// workaround for clang compiler bug http://llvm.org/bugs/show_bug.cgi?id=19140
			// friend struct range_adaptor<Derived, Rng, Traversal, true>;  // fixes the clang issue but not gcc
			using BaseRange=std::remove_reference_t< Rng >;

		protected:
			constexpr range_adaptor()=default;

			template< typename Rhs >
			constexpr explicit range_adaptor( aggregate_tag, Rhs&& rhs ) noexcept
			:	m_baserng( aggregate_tag(), std::forward<Rhs>(rhs) )
			{}
			template< typename Rhs >
			constexpr range_adaptor( Rhs&& rhs ) noexcept
			:	m_baserng(std::forward<Rhs>(rhs).m_baserng)
			{}

		public:
			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *std::move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}

			template< typename Func >
			constexpr auto operator()(Func func) /* no & */ MAYTHROW {
				return tc::for_each(
					*m_baserng,
					[&](auto&&... args) mutable MAYTHROW {
						return range_adaptor_access()(tc::derived_cast<Derived>(*this), func, std::forward<decltype(args)>(args)...);
					}
				);
			}

			template< typename Func >
			constexpr auto operator()(Func func) const /* no & */ MAYTHROW {
				return tc::for_each(
					*m_baserng,
					[&](auto&&... args) mutable MAYTHROW {
						return range_adaptor_access()(tc::derived_cast<Derived>(*this), func, std::forward<decltype(args)>(args)...);
					}
				);
			}
		};
		//-------------------------------------------------------------------------------------------------------------------------
		// iterator/index based ranges
		//
		// they derive from the generator case, because the generator interface can transparently and efficiently be added
		// to any iterator or index based range.
		//

		template<
			typename Derived 
			, typename Rng
			, typename Traversal
		>
		struct range_adaptor<
			Derived 
			, Rng
			, Traversal
			, true
		>
		: range_adaptor<Derived,Rng,Traversal,false>
		, range_iterator_from_index<
			Derived,
			tc::index_t<typename range_adaptor<Derived,Rng,Traversal,false>::BaseRange>,
			typename boost::range_detail::demote_iterator_traversal_tag<
				std::conditional_t< std::is_same<Traversal,boost::iterators::use_default>::value
					, boost::iterators::random_access_traversal_tag
					, Traversal
				>,
				tc::traversal_t<Rng>
			>::type
		>
		{
		private:
			using this_type = range_adaptor;
			using base_ = range_adaptor<Derived,Rng,Traversal,false>;
			
		protected:
			using typename base_::BaseRange;

			constexpr range_adaptor()=default;

			template< typename Rhs >
			constexpr explicit range_adaptor( aggregate_tag, Rhs&& rhs ) noexcept
			:	base_(aggregate_tag(), std::forward<Rhs>(rhs))
			{}

			template< typename Rhs >
			constexpr range_adaptor( Rhs&& rhs ) noexcept
			:	base_(std::forward<Rhs>(rhs))
			{}

		public:
			using index = tc::index_t<BaseRange>;

			STATIC_OVERRIDE_MOD(constexpr,begin_index)() const& MAYTHROW -> index {
				return tc::begin_index(this->m_baserng);
			}

			STATIC_OVERRIDE_MOD(template<typename other_type = this_type> constexpr,end_index)() const& MAYTHROW return_decltype(
				tc::end_index(tc::base_cast<other_type>(this)->m_baserng)
			)

			STATIC_OVERRIDE_MOD(constexpr,at_end_index)(index const& idx) const& MAYTHROW -> bool {
				return tc::at_end_index(*this->m_baserng,idx);
			}

			STATIC_OVERRIDE_MOD(constexpr,dereference_index)(index const& idx) & MAYTHROW ->decltype(auto) {
				return tc::dereference_index(*this->m_baserng,idx);
			}

			STATIC_OVERRIDE_MOD(constexpr,dereference_index)(index const& idx) const& MAYTHROW ->decltype(auto) {
				return tc::dereference_index(*this->m_baserng,idx);
			}

			STATIC_OVERRIDE_MOD(template<typename other_type = this_type> constexpr,equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept return_decltype(
				tc::equal_index(*tc::base_cast<other_type>(this)->m_baserng,idxLhs,idxRhs)
			)

			STATIC_OVERRIDE_MOD(constexpr,increment_index)(index& idx) const& MAYTHROW -> void {
				tc::increment_index(*this->m_baserng,idx);
			}

			STATIC_OVERRIDE_MOD(constexpr,decrement_index)(index& idx) const& MAYTHROW -> void {
				tc::decrement_index(*this->m_baserng,idx);
			}

			STATIC_OVERRIDE_MOD(constexpr,advance_index)(index& idx, typename boost::range_difference<Rng>::type d) const& MAYTHROW -> void {
				tc::advance_index(*this->m_baserng,idx,d);
			}

			STATIC_OVERRIDE_MOD(constexpr,distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept {
				return tc::distance_to_index(*this->m_baserng,idxLhs,idxRhs);
			}

			STATIC_OVERRIDE_MOD(constexpr,middle_point)( index & idxBegin, index const& idxEnd ) const& noexcept -> void {
				tc::middle_point(*this->m_baserng,idxBegin,idxEnd);
			}
		};
	}
	using no_adl::range_adaptor;

	namespace no_adl {
		template<typename Rng, bool bConst>
		struct reference_for_value_or_reference_with_index_range {
			using type = tc::range_reference_t<
				decltype(
					*std::declval<
						tc::apply_if_t<
							bConst,
							std::add_const,
							reference_or_value< Rng >
						>
					&>()
				)
			>;
		};

		template<typename Rng, bool bConst>
		using reference_for_value_or_reference_with_index_range_t = typename reference_for_value_or_reference_with_index_range<Rng, bConst>::type;

		template< typename Derived, typename Rng, typename Traversal >
		struct range_reference<range_adaptor<Derived, Rng, Traversal, false> > : reference_for_value_or_reference_with_index_range<Rng, false> {};

		template< typename Derived, typename Rng, typename Traversal >
		struct range_reference<range_adaptor<Derived, Rng, Traversal, false> const> : reference_for_value_or_reference_with_index_range<Rng, true> {};
	}
}
