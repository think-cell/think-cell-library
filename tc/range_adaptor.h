
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "index_iterator.h"
#include "range_defines.h"
#include "meta.h"
#include "for_each.h"

#include "casts.h"
#include "static_polymorphism.h"

#pragma warning( push )
#pragma warning( disable: 4146 )
// warning C4146: unary minus operator applied to unsigned type, result still unsigned
// iterator_facade::distance_from returns -iterator_facade::distance_to which causes warning C4146 if difference_type is std::size_t
#include <boost/iterator/iterator_facade.hpp> 
#pragma warning( pop )

#include <boost/range/detail/demote_iterator_traversal_tag.hpp>
#include <boost/mpl/has_xxx.hpp>

#include <type_traits>

#if defined(TC_WIN) && _MSC_VER <= 1915
// In MSVC 2017, statically polymorphic functions can't be used in constant expressions, unless they are implemented in the same class where they're declared.
// This is caused by a compiler bug: if a base class pointer is cast to a derived class pointer, it cannot be used to access member variables from the derived class.
// The workaround used here is to define all of the utility functions from the base class in the derived class instead, so that no cast is necessary.
#define TC_RANGE_ITERATOR_HELPER_BASE_CLASS_WORKAROUND
#endif

// Simulate iterator interface on top of index interface.
// This is equivalent to inheriting from range_iterator_from_index, which does not work in constant expressions in MSVC 2017 due to the use of static polymorphism.
// This macro should be included in a class definition.
// The class must have an 'index' type alias, and a 'Derived' type alias which is the fully-derived type (CRTP).
// Note: this can only be used if TC_RANGE_ITERATOR_HELPER_BASE_CLASS_WORKAROUND is defined. If it is not defined, you must inherit from the base class instead.
#define DEFINE_RANGE_ITERATOR_FROM_INDEX \
	using iterator = index_iterator< Derived, false >; \
	using const_iterator = index_iterator< Derived, true >; \
	STATIC_VIRTUAL_CONSTEXPR(begin_index) \
	STATIC_VIRTUAL_CONSTEXPR(end_index) \
	STATIC_VIRTUAL_CONSTEXPR(at_end_index) \
	STATIC_VIRTUAL_CONSTEXPR(increment_index) \
	STATIC_VIRTUAL_CONSTEXPR(decrement_index) \
	STATIC_VIRTUAL_CONSTEXPR(dereference_index) \
	STATIC_VIRTUAL_CONSTEXPR(equal_index) \
	STATIC_VIRTUAL_CONSTEXPR(advance_index) \
	STATIC_VIRTUAL_CONSTEXPR(distance_to_index) \
	STATIC_VIRTUAL_CONSTEXPR(middle_point) \
	\
	constexpr const_iterator make_iterator( index idx ) const& noexcept { \
		return const_iterator(tc::derived_cast<Derived>(this), tc_move(idx)); \
	} \
	\
	constexpr const_iterator begin() const& \
		return_MAYTHROW(make_iterator(begin_index())) \
	\
	template<typename Derived_=Derived, std::enable_if_t<!has_mem_fn_end_index<Derived_>::value || !has_mem_fn_equal_index<Derived_>::value>* = nullptr> \
	constexpr end_sentinel end() const& noexcept { \
		return {}; \
	} \
	\
	template<typename Derived_ = Derived, std::enable_if_t<has_mem_fn_end_index<Derived_>::value && has_mem_fn_equal_index<Derived_>::value>* = nullptr> \
	constexpr const_iterator end() const& \
		return_MAYTHROW(make_iterator(end_index())) \
	\
	constexpr iterator make_iterator( index idx ) & noexcept { \
		return iterator(tc::derived_cast<Derived>(this),tc_move(idx)); \
	} \
	\
	constexpr iterator begin() & \
		return_MAYTHROW(make_iterator(begin_index())) \
	\
	template<typename Derived_ = Derived, std::enable_if_t<has_mem_fn_end_index<Derived_>::value && has_mem_fn_equal_index<Derived_>::value>* = nullptr> \
	constexpr iterator end() & \
		return_MAYTHROW(make_iterator(end_index())) \
	\
	constexpr bool empty() const& \
		return_MAYTHROW(at_end_index(begin_index())) \
	\
	STATIC_OVERRIDE_MOD( \
		template<typename Derived_ = Derived BOOST_PP_COMMA() std::enable_if_t<has_mem_fn_end_index<Derived_>::value && has_mem_fn_equal_index<Derived_>::value>* = nullptr> \
		constexpr, \
	at_end_index)(index const& idx) const& MAYTHROW -> bool { \
		return equal_index(idx, end_index()); \
	}

#define DEFINE_RANGE_ITERATOR_FROM_INDEX_GENERATOR_EXTENSIONS \
	template< typename Func, typename Derived_=Derived > \
	constexpr auto operator()(Func func) /* no & */ noexcept(noexcept( \
		this->increment_index(std::declval<typename Derived_::index &>()) \
	) && noexcept( \
		tc::continue_if_not_break( func, this->dereference_index(this->begin_index()) ) \
	)) -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::range_reference_t<Derived_>>())), INTEGRAL_CONSTANT(tc::continue_)> { \
		for( auto idx=this->begin_index(); \
			!this->at_end_index(idx); \
			this->increment_index(idx) \
		) { \
			RETURN_IF_BREAK( tc::continue_if_not_break( func, this->dereference_index(idx) ) ); \
		} \
		return INTEGRAL_CONSTANT(tc::continue_)(); \
	} \
	template< typename Func, typename Derived_=Derived > \
	constexpr auto operator()(Func func) const/* no & */ noexcept(noexcept( \
		this->increment_index(std::declval<typename Derived_::index &>()) \
	) && noexcept( \
		tc::continue_if_not_break(func, this->dereference_index(this->begin_index())) \
	)) -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<tc::range_reference_t<Derived_>>())), INTEGRAL_CONSTANT(tc::continue_)> { \
		for( auto idx=this->begin_index(); \
			!this->at_end_index(idx); \
			this->increment_index(idx) \
		) { \
			RETURN_IF_BREAK( tc::continue_if_not_break( func, this->dereference_index(idx) ) ); \
		} \
		return INTEGRAL_CONSTANT(tc::continue_)(); \
	}

// Equivalent to inheriting from range_iterator_generator_from_index, which does not work in constant expressions in MSVC 2017 due to the use of static polymorphism.
// Macro arguments are the same as for DEFINE_RANGE_ITERATOR_FROM_INDEX
#define DEFINE_RANGE_ITERATOR_GENERATOR_FROM_INDEX \
	DEFINE_RANGE_ITERATOR_FROM_INDEX \
	DEFINE_RANGE_ITERATOR_FROM_INDEX_GENERATOR_EXTENSIONS

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
		struct range_iterator_from_index {
		private:
			using this_type = range_iterator_from_index;
		public:
			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			using index = Index;

			DEFINE_RANGE_ITERATOR_FROM_INDEX
		};

		template<
			typename Derived,
			typename Index
		>
		struct range_iterator_generator_from_index
			: range_iterator_from_index<Derived, Index>
		{
			DEFINE_RANGE_ITERATOR_FROM_INDEX_GENERATOR_EXTENSIONS
		};
	}
	using range_iterator_from_index_impl::range_iterator_from_index;
	using range_iterator_from_index_impl::range_iterator_generator_from_index;

	namespace no_adl {
		template<typename Derived, typename Func>
		struct range_adaptor_access {
			Derived& m_derived;
			Func& m_func;

			template<typename Arg>
			auto operator()(Arg&& arg) const& return_decltype_MAYTHROW(
				m_derived.apply(m_func, std::forward<Arg>(arg)) // MAYTHROW
			)
		};
	}
	
	template<typename Derived, typename Func>
	auto make_range_adaptor_access(Derived& derived, Func& func) noexcept {
		return no_adl::range_adaptor_access<Derived, Func>{derived, func};
	}

	namespace no_adl {
		template<
			typename Derived 
			, typename Rng
			, typename MaximumTraversal = boost::iterators::random_access_traversal_tag // This is used to ensure that filter_adaptor is never random access
			, bool WithMiddlePoint = true
			, bool HasIterator = is_range_with_iterators< Rng >::value
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
			, typename MaximumTraversal
			, bool WithMiddlePoint
		>
		struct range_adaptor<
			Derived 
			, Rng
			, MaximumTraversal
			, WithMiddlePoint
			, false
		> {
			static_assert( !std::is_rvalue_reference<Rng>::value );
			reference_or_value< Rng > m_baserng;

		protected:
			constexpr range_adaptor()=default;

			template< typename Rhs >
			constexpr explicit range_adaptor( aggregate_tag_t, Rhs&& rhs ) noexcept
			:	m_baserng( aggregate_tag, std::forward<Rhs>(rhs) )
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

			template< typename Func, typename Derived2=Derived >
			constexpr auto operator()(Func func) const& return_decltype_MAYTHROW(
				tc::for_each(
					*m_baserng,
					make_range_adaptor_access(tc::derived_cast<Derived2>(*this), func)
				)
			)

			template<typename This, typename Func>
			constexpr static auto enumerate_reversed(This&& rngThis, Func&& func) return_decltype_MAYTHROW(
				tc::for_each(
					tc::reverse(*std::forward<This>(rngThis).m_baserng),
					make_range_adaptor_access(rngThis, func)
				)
			)
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
			, typename MaximumTraversal
			, bool WithMiddlePoint
		>
		struct range_adaptor<
			Derived 
			, Rng
			, MaximumTraversal
			, WithMiddlePoint
			, true
		>
		: range_adaptor<Derived,Rng,MaximumTraversal,WithMiddlePoint,false>
		, range_iterator_from_index<
			Derived,
			tc::index_t<std::remove_reference_t<Rng>>
		>
		{
		private:
			using this_type = range_adaptor;
			using base_ = range_adaptor<Derived,Rng,MaximumTraversal,WithMiddlePoint,false>;
			
		protected:
			constexpr range_adaptor()=default;

			template< typename Rhs >
			constexpr explicit range_adaptor( aggregate_tag_t, Rhs&& rhs ) noexcept
			:	base_(aggregate_tag, std::forward<Rhs>(rhs))
			{}

			template< typename Rhs >
			constexpr range_adaptor( Rhs&& rhs ) noexcept
			:	base_(std::forward<Rhs>(rhs))
			{}

		public:
			using index = tc::index_t<std::remove_reference_t<Rng>>;
		private:
			STATIC_OVERRIDE_MOD(constexpr,begin_index)() const& MAYTHROW -> index {
				return tc::begin_index(this->m_baserng);
			}

			STATIC_OVERRIDE_MOD(template<typename other_type = this_type> constexpr,end_index)() const& return_decltype_MAYTHROW(
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

			STATIC_OVERRIDE_MOD(template<typename other_type = this_type> constexpr,equal_index)(index const& idxLhs, index const& idxRhs) const& return_decltype_noexcept(
				tc::equal_index(*tc::base_cast<other_type>(this)->m_baserng,idxLhs,idxRhs)
			)

			STATIC_OVERRIDE_MOD(constexpr,increment_index)(index& idx) const& MAYTHROW -> void {
				tc::increment_index(*this->m_baserng,idx);
			}

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
				tc::decrement_index(*this->m_baserng,idx);
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
				tc::advance_index(*this->m_baserng,idx,d);
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
				return tc::distance_to_index(*this->m_baserng,idxLhs,idxRhs);
			}

			STATIC_OVERRIDE_MOD(
				template<
					ENABLE_SFINAE BOOST_PP_COMMA()
					std::enable_if_t<SFINAE_VALUE(WithMiddlePoint) && tc::has_middle_point<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr
				> constexpr,
			middle_point)( index & idxBegin, index const& idxEnd ) const& noexcept -> void {
				tc::middle_point(*this->m_baserng,idxBegin,idxEnd);
			}
		};
	}
	using no_adl::range_adaptor;

#ifndef TC_RANGE_ITERATOR_HELPER_BASE_CLASS_WORKAROUND
#undef DEFINE_RANGE_ITERATOR_FROM_INDEX
#undef DEFINE_RANGE_ITERATOR_FROM_INDEX_GENERATOR_EXTENSIONS
#undef DEFINE_RANGE_ITERATOR_GENERATOR_FROM_INDEX
#endif
}
