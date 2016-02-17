#pragma once

#include "index_iterator.h"
#include "range_defines.h"
#include "meta.h"

#include "casts.h"
#include "static_polymorphism.h"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/mpl/has_xxx.hpp>

#include <type_traits>
#include <boost/range/detail/demote_iterator_traversal_tag.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {

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
			STATIC_VIRTUAL(dereference_index)

			const_iterator make_iterator( index idx ) const {
				return const_iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			const_iterator begin() const {
				return make_iterator(begin_index());
			}

			const_iterator end() const {
				return make_iterator(end_index());
			}

			iterator make_iterator( index idx ) {
				return iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			iterator begin() {
				return make_iterator(begin_index());
			}

			iterator end() {
				return make_iterator(end_index());
			}

			bool empty() const {
				return at_end_index(
					begin_index()
				);
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const -> bool {
				return tc::derived_cast<Derived>(this)->equal_index(idx, end_index());
			}
		};
	}
	using range_iterator_from_index_impl::range_iterator_from_index;

	namespace range_adaptor_impl {
		struct range_adaptor_access {
			template< typename Derived, typename... Args>
			auto operator()(Derived&& derived, Args&& ... args) return_decltype(
				std::forward<Derived>(derived).apply(std::forward<Args>(args)...)
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
		T& move_if( T& t, std::false_type ) {
			return t;
		}

		template< typename T >
		T&& move_if( T& t, std::true_type ) {
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
			reference_or_value< typename index_range<Rng>::type > m_baserng;
		
		// protected:
			// workaround for clang compiler bug http://llvm.org/bugs/show_bug.cgi?id=19140
			// friend struct range_adaptor<Derived, Rng, Traversal, true>;  // fixes the clang issue but not gcc
			using BaseRange=std::remove_reference_t< typename index_range<Rng>::type >;

		protected:
			range_adaptor()
				// m_baserng may be default-constructible
			{}

			template< typename Rhs >
			range_adaptor( Rhs&& rhs, aggregate_tag )
			:	m_baserng( std::forward<Rhs>(rhs), aggregate_tag() )
			{}
			template< typename Rhs >
			range_adaptor( Rhs&& rhs )
			:	m_baserng(std::forward<Rhs>(rhs).m_baserng)
			{}

			// explicitly define the copy constructor to do what the template above does, as it would if the implicit copy consturctor wouldn't interfere
			range_adaptor( range_adaptor const& rhs)
			:	m_baserng(rhs.m_baserng)
			{}

		public:
			auto base_range() return_decltype(
				*m_baserng
			)

			auto base_range() const return_decltype(
				*m_baserng
			)

			auto base_range_move() return_decltype_rvalue_by_ref(
				move_if( *m_baserng, std::integral_constant< bool, !std::is_reference<Rng>::value >() )
			)

		private:

			template< typename Func, bool Abortable >
			struct adaptor;

			template< typename Func >
			struct adaptor<Func, true> {
            private:
				Derived const& m_derived;
				std::decay_t<Func> mutable m_func;

			public:
				adaptor(Derived const& derived, Func&& func)
				: m_derived( derived )
				, m_func( std::forward<Func>(func) ) {}

				template<typename... Args> break_or_continue operator()(Args&& ... args) const {
					return continue_if_not_break(
						range_adaptor_access(),
						m_derived,
						m_func,
						std::forward<Args>(args)...
					);
				}
			};

			template< typename Func >
			struct adaptor<Func, false> {
            private:
				Derived const& m_derived;
				std::decay_t<Func> mutable m_func;

			public:
				adaptor(Derived const& derived, Func&& func)
					: m_derived( derived )
					, m_func( std::forward<Func>(func) ) {}

				template<typename... Args>
				void operator()(Args&& ... args) const {
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

			template< typename Func >
			typename std::enable_if<
				std::is_same<
					tc::result_of_t<BaseRange(adaptor<Func, true>)>,
					break_or_continue
				>::value,
				break_or_continue
			>::type
			operator()(Func&& func) {
				return base_range()(adaptor<Func, true>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}
		
			template< typename Func >
			typename std::enable_if<
				!std::is_same<
					tc::result_of_t<BaseRange(adaptor<Func, true>)>,
					break_or_continue
				>::value &&
				std::is_same<
					tc::result_of_t<BaseRange(adaptor<Func, false>)>,
					void
				>::value
			>::type
			operator()(Func&& func) {
				return base_range()(adaptor<Func, false>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}

			template< typename Func >
			typename std::enable_if<
				std::is_same<
					tc::result_of_t<BaseRange(adaptor<Func, true>)>,
					break_or_continue
				>::value,
				break_or_continue
			>::type
			operator()(Func&& func) const {
				return base_range()(adaptor<Func, true>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}

			template< typename Func >
			typename std::enable_if<
				!std::is_same<
					tc::result_of_t<BaseRange(adaptor<Func, true>)>,
					break_or_continue
				>::value &&
				std::is_same<
					tc::result_of_t<BaseRange(adaptor<Func, false>)>,
					void
				>::value
			>::type
			operator()(Func&& func) const {
				return base_range()(adaptor<Func, false>(derived_cast<Derived>(*this), std::forward<Func>(func)));
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
				typename boost::iterator_traversal<
					typename boost::range_iterator<std::remove_reference_t<Rng>>::type
				>::type
			>::type
		>
		{
		private:
			using base_ = range_adaptor<Derived,Rng,Traversal,false>;
			using this_type = range_adaptor;
		protected:
			using typename base_::BaseRange;

			range_adaptor()
				// m_baserng may be default-constructible
			{}

			template< typename Rhs >
			range_adaptor( Rhs&& rhs, aggregate_tag )
			:	base_(std::forward<Rhs>(rhs), aggregate_tag())
			{}

			template< typename Rhs >
			range_adaptor( Rhs&& rhs )
			:	base_(std::forward<Rhs>(rhs))
			{}

			range_adaptor( range_adaptor const& rhs )
			:	base_(rhs)
			{}
		
		public:
			using index = typename BaseRange::index;

			STATIC_OVERRIDE(begin_index)() const -> index {
				return this->base_range().begin_index();
			}

			STATIC_OVERRIDE(end_index)() const -> index {
				return this->base_range().end_index();
			}

			STATIC_OVERRIDE(at_end_index)(index const& idx) const -> bool {
				return this->base_range().at_end_index(idx);
			}

			STATIC_OVERRIDE(dereference_index)(index const& idx)
				return_decltype( THIS_IN_DECLTYPE base_range().dereference_index(idx) )

			STATIC_OVERRIDE(dereference_index)(index const& idx) const
				return_decltype( make_const(THIS_IN_DECLTYPE base_range()).dereference_index(idx) )

			bool equal_index(index const& idxLhs, index const& idxRhs) const {
				return this->base_range().equal_index(idxLhs,idxRhs);
			}

			STATIC_OVERRIDE(increment_index)(index& idx) const -> void {
				this->base_range().increment_index(idx);
			}

			void decrement_index(index& idx) const {
				this->base_range().decrement_index(idx);
			}

			void advance_index(index& idx, typename boost::range_difference<std::remove_reference_t<Rng>>::type d) const {
				this->base_range().advance_index(idx,d);
			}

			typename boost::range_difference<std::remove_reference_t<Rng>>::type distance_to_index(index const& idxLhs, index const& idxRhs) const {
				return this->base_range().distance_to_index(idxLhs,idxRhs);
			}

			void middle_point( index & idxBegin, index const& idxEnd ) const {
				this->base_range().middle_point(idxBegin,idxEnd);
			}
		};
	}
	using range_adaptor_impl::range_adaptor;
}


