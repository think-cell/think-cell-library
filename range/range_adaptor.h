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

#include "index_iterator.h"
#include "range_defines.h"
#include "meta.h"
#include "types.h"

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
			static_assert( boost::iterators::detail::is_iterator_traversal<Traversal>::value, "" );
			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			using index = Index;

			using iterator = index_iterator< Derived, Traversal, false >;
			using const_iterator = index_iterator< Derived, Traversal, true >;

			STATIC_VIRTUAL(begin_index)
			STATIC_VIRTUAL(end_index)
			STATIC_VIRTUAL(at_end_index)
			STATIC_VIRTUAL(increment_index)
			STATIC_VIRTUAL(decrement_index)
			STATIC_VIRTUAL(dereference_index)
			STATIC_VIRTUAL(equal_index)
			STATIC_VIRTUAL(advance_index)
			STATIC_VIRTUAL(distance_to_index)
			STATIC_VIRTUAL(middle_point)

			const_iterator make_iterator( index idx ) const &  noexcept {
				return const_iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			const_iterator begin() const& MAYTHROW {
				return make_iterator(begin_index());
			}

			const_iterator end() const& MAYTHROW {
				return make_iterator(end_index());
			}

			iterator make_iterator( index idx ) & noexcept {
				return iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			iterator begin() & MAYTHROW {
				return make_iterator(begin_index());
			}

			iterator end() & MAYTHROW {
				return make_iterator(end_index());
			}

			bool empty() const& MAYTHROW {
				return at_end_index(
					begin_index()
				);
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const& MAYTHROW -> bool {
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

	namespace range_adaptor_impl {
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

		template< typename T >
		T& move_if( T& t, std::false_type ) noexcept {
			return t;
		}

		template< typename T >
		T&& move_if( T& t, std::true_type ) noexcept {
			return std::move(t);
		}

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
			static_assert( !std::is_rvalue_reference<Rng>::value, "" );
			reference_or_value< index_range_t<Rng> > m_baserng;
		
		// protected:
			// workaround for clang compiler bug http://llvm.org/bugs/show_bug.cgi?id=19140
			// friend struct range_adaptor<Derived, Rng, Traversal, true>;  // fixes the clang issue but not gcc
			using BaseRange=std::remove_reference_t< index_range_t<Rng> >;

		protected:
			range_adaptor()=default;

			template< typename Rhs >
			explicit range_adaptor( aggregate_tag, Rhs&& rhs ) noexcept
			:	m_baserng( aggregate_tag(), std::forward<Rhs>(rhs) )
			{}
			template< typename Rhs >
			range_adaptor( Rhs&& rhs ) noexcept
			:	m_baserng(std::forward<Rhs>(rhs).m_baserng)
			{}

		public:
			auto base_range() & noexcept return_decltype(
				*m_baserng
			)

			auto base_range() const & noexcept return_decltype(
				*m_baserng
			)

			auto base_range_move() return_decltype_rvalue_by_ref(
				move_if( *m_baserng, std::integral_constant< bool, !std::is_reference<Rng>::value >() )
			)

		private:

			template< typename Func, bool Abortable >
			struct adaptor;

			template< typename Func >
			struct adaptor<Func, true> final {
		private:
				Derived const& m_derived;
				tc::decay_t<Func> mutable m_func;

			public:
				explicit adaptor(Derived const& derived, Func&& func) noexcept
				: m_derived( derived )
				, m_func( std::forward<Func>(func) ) {}

				template<typename... Args> break_or_continue operator()(Args&& ... args) const& MAYTHROW {
					return continue_if_not_break(
						range_adaptor_access(),
						m_derived,
						m_func,
						std::forward<Args>(args)...
					);
				}
			};

			template< typename Func >
			struct adaptor<Func, false> final {
			private:
				Derived const& m_derived;
				tc::decay_t<Func> mutable m_func;

			public:
				explicit adaptor(Derived const& derived, Func&& func) noexcept
					: m_derived( derived )
					, m_func( std::forward<Func>(func) ) {}

				template<typename... Args>
				void operator()(Args&& ... args) const& MAYTHROW {
					static_assert(
						// Note: Instead of the following static_assert it would be possible to
						// use return_decltype() and let the functor check in ensure_index_range
						// check the correct type. The problem with that is, that
						// VC12 very soon reaches compiler limits on chained decltypes. This
						// chain is interrupted by returning void here.
						!std::is_same<
							decltype(range_adaptor_access()( m_derived, m_func, std::forward<Args>(args)... )),
							break_or_continue
						>::value,
						"void generator ranges must not be used with functors returning break_or_continue!"
					);
					range_adaptor_access()( m_derived, m_func, std::forward<Args>(args)... );
				}
			};

		public:

			template< typename Func, std::enable_if_t<
				std::is_same<
					std::result_of_t<BaseRange&(adaptor<Func, true>)>,
					break_or_continue
				>::value>* = nullptr>
			break_or_continue operator()(Func&& func) /* no & */ MAYTHROW {
				return base_range()(adaptor<Func, true>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}
		
			template< typename Func, std::enable_if_t<
				!std::is_same<
					std::result_of_t<BaseRange(adaptor<Func, true>)>,
					break_or_continue
				>::value
			>* = nullptr>
			void operator()(Func&& func) /* no & */ MAYTHROW {
				static_assert(
					std::is_same<
						std::result_of_t<BaseRange&(adaptor<Func, false>)>,
						void
					>::value,
					"Generator range must return either tc::break_or_continue or void"
				);

				base_range()(adaptor<Func, false>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}

			template< typename Func, std::enable_if_t<
				std::is_same<
					std::result_of_t<BaseRange const&(adaptor<Func, true>)>,
					break_or_continue
				>::value>* = nullptr>
			break_or_continue operator()(Func&& func) const/* no & */ MAYTHROW {
				return base_range()(adaptor<Func, true>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}

			template< typename Func, std::enable_if_t<
				!std::is_same<
					std::result_of_t<BaseRange const&(adaptor<Func, true>)>,
					break_or_continue
				>::value
			>* = nullptr>
			void operator()(Func&& func) const/* no & */ MAYTHROW {
				static_assert(
					std::is_same<
						std::result_of_t<BaseRange const&(adaptor<Func, false>)>,
						void
					>::value,
					"Generator range must return either tc::break_or_continue or void"
				);
				base_range()(adaptor<Func, false>(derived_cast<Derived>(*this), std::forward<Func>(func)));
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
			typename range_adaptor<Derived,Rng,Traversal,false>::BaseRange::index,
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

			range_adaptor()=default;

			template< typename Rhs >
			explicit range_adaptor( aggregate_tag, Rhs&& rhs ) noexcept
			:	base_(aggregate_tag(), std::forward<Rhs>(rhs))
			{}

			template< typename Rhs >
			range_adaptor( Rhs&& rhs ) noexcept
			:	base_(std::forward<Rhs>(rhs))
			{}

		public:
			using index = typename BaseRange::index;

			STATIC_OVERRIDE(begin_index)() const& MAYTHROW -> index {
				return this->base_range().begin_index();
			}

			STATIC_OVERRIDE(end_index)() const& MAYTHROW -> index {
				return this->base_range().end_index();
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const& MAYTHROW -> bool {
				return this->base_range().at_end_index(idx);
			}

			STATIC_OVERRIDE(dereference_index)(index const& idx) & MAYTHROW ->decltype(auto) {
				return this->base_range().dereference_index(idx);
			}

			STATIC_OVERRIDE(dereference_index)(index const& idx) const& MAYTHROW ->decltype(auto) {
				return this->base_range().dereference_index(idx);
			}

			STATIC_OVERRIDE(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return this->base_range().equal_index(idxLhs,idxRhs);
			}

			STATIC_OVERRIDE(increment_index)(index& idx) const& MAYTHROW -> void {
				this->base_range().increment_index(idx);
			}

			STATIC_OVERRIDE(decrement_index)(index& idx) const& MAYTHROW -> void {
				this->base_range().decrement_index(idx);
			}

			STATIC_OVERRIDE(advance_index)(index& idx, typename boost::range_difference<std::remove_reference_t<Rng>>::type d) const& MAYTHROW -> void {
				this->base_range().advance_index(idx,d);
			}

			STATIC_OVERRIDE(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> typename boost::range_difference<std::remove_reference_t<Rng>>::type {
				return this->base_range().distance_to_index(idxLhs,idxRhs);
			}

			STATIC_OVERRIDE(middle_point)( index & idxBegin, index const& idxEnd ) const& noexcept -> void {
				this->base_range().middle_point(idxBegin,idxEnd);
			}
		};
	}
	using range_adaptor_impl::range_adaptor;
}


