#pragma once

#include "range_defines.h"

#include "index_range.h"
#include "meta.h"

#include "Library/Utilities/casts.h"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/mpl/has_xxx.hpp>
#include <boost/mpl/logical.hpp>

#include <type_traits>
#include <boost/type_traits/common_type.hpp>
#include <boost/range/detail/demote_iterator_traversal_tag.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {

	//////////////////////////////////////////////////////////
	// range adaptors
	//
	// Basic building block for all ranges.
	// Comes in two variations, one for generator ranges, one for iterator ranges. 
	//

	template< typename Rng >
	struct make_sub_range_result;

	namespace range_adaptor_impl {

		class range_adaptor_access {
		public:
	#define PART1() \
			template< typename Derived,
	#define PART2() \
			> static break_or_continue apply( Derived && derived, 
	#define PART3() ) { \
				return std::forward<Derived>(derived).apply(
	#define PART4() ); \
			}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4
		};

		template<
			typename Derived 
			, typename Rng
			, typename Aggregate
			, typename ValueType=boost::use_default
			, typename Traversal=boost::use_default
			, bool HasIterator=is_range_with_iterators< Rng >::value
		>
		class range_adaptor;

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
			, typename Aggregate
			, typename ValueType
			, typename Traversal
		>
		class range_adaptor<
			Derived 
			, Rng
			, Aggregate
			, ValueType
			, Traversal
			, false
		> {
			reference_or_value< typename index_range<Rng>::type > m_baserng;
		protected:
			typedef typename std::remove_reference< typename index_range<Rng>::type >::type BaseRange;

			// range_adaptor<Rng &>( range_adaptor<Rng &> const& )
			// would allow creating a mutable range from a const range.
			// More specifically, the copy could modify elements, although the original promised not to.
			// So we only implement range_adaptor<Rng &>( range_adaptor<Rng &> & )
			template< typename RngOther >
			struct is_const_compatible_range : boost::mpl::or_<
				// RngOther is copied:
				boost::mpl::not_< std::is_reference<Rng> >,
				// RngOther is referenced const:
				std::is_const< typename std::remove_reference<Rng>::type >,
				// RngOther is referenced mutable, so RngOther must be mutable reference:
				boost::mpl::not_< std::is_const< typename std::remove_reference<RngOther>::type > >
			>::type {};

			template< typename RngOther >
			struct const_compatible_range {
				typedef typename boost::mpl::if_< 
					is_const_compatible_range<RngOther const&>,
					RngOther const&,
					RngOther &
				>::type type;
			};

			range_adaptor()
				// m_baserng may be default-constructible
			{}

			template< typename Rhs >
			range_adaptor( Rhs && rhs, aggregate_tag )
			:	m_baserng( std::forward<Rhs>(rhs) )
			{}
			template< typename Rhs >
			range_adaptor( Rhs && rhs )
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
				*make_const(m_baserng)
			)

			auto base_range_move() return_decltype(
				move_if( *m_baserng, std::integral_constant< bool, !std::is_reference<Rng>::value >() )
			)

		private:
			template< typename Func, bool Abortable >
			class adaptor;

			template< typename Func >
			class adaptor<Func, true> {
				Derived const& m_derived;
				typename std::decay<Func>::type mutable m_func;

			public:
				adaptor( Derived const& derived, Func && func )
				: m_derived( derived )
				, m_func( std::forward<Func>(func) ) {}

	#define PART1() \
				template<
	#define PART2() \
				> break_or_continue operator()(
	#define PART3() ) const { \
					return range_adaptor_access::apply( m_derived, m_func, 
	#define PART4() ); \
				}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4
			};

			template< typename Func >
			class adaptor<Func, false> {
				Derived const& m_derived;
				typename std::decay<Func>::type mutable m_func;

			public:
				adaptor( Derived const& derived, Func && func )
				: m_derived( derived )
				, m_func( std::forward<Func>(func) ) {}

	#define PART1() \
				template<
	#define PART2() \
				> void operator()(
	#define PART3() ) const { \
					range_adaptor_access::apply( m_derived, m_func, 
	#define PART4() ); \
				}
	PERFECT_FORWARD
	#undef PART1
	#undef PART2
	#undef PART3
	#undef PART4
			};

		public:
			template< typename Func >
			break_or_continue operator()(Func && func) {
				return continue_if_void( base_range(), adaptor<Func,
					std::is_same< typename std::result_of< decltype( base_range() )( adaptor<Func,true> ) >::type, break_or_continue >::value
					>( derived_cast<Derived>(*this), std::forward<Func>(func) ) );
			}

			template< typename Func >
			break_or_continue operator()(Func && func) const {
				return continue_if_void( base_range(), adaptor<Func,
					std::is_same< typename std::result_of< decltype( base_range() )( adaptor<Func,true> ) >::type, break_or_continue >::value
					>( derived_cast<Derived>(*this), std::forward<Func>(func) ) );
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
			, typename Aggregate
			, typename ValueType
			, typename Traversal
		>
		class range_adaptor<
			Derived 
			, Rng
			, Aggregate
			, ValueType
			, Traversal
			, true
		> : public range_adaptor<Derived,Rng,Aggregate,ValueType,Traversal,false> {
			typedef range_adaptor<Derived,Rng,Aggregate,ValueType,Traversal,false> base_;
		protected:
			using typename base_::BaseRange;

			range_adaptor()
				// m_baserng may be default-constructible
			{}

			template< typename Rhs >
			range_adaptor( Rhs && rhs, aggregate_tag )
			:	base_(std::forward<Rhs>(rhs), aggregate_tag())
			{}

			template< typename Rhs >
			range_adaptor( Rhs && rhs )
			:	base_(std::forward<Rhs>(rhs))
			{}

			range_adaptor( range_adaptor const& rhs )
			:	base_(rhs)
			{}
		
		public:
			typedef typename BaseRange::index index;
			static bool const index_valid_after_copy=std::is_reference< typename index_range<Rng>::type >::value || BaseRange::index_valid_after_copy;

			index begin_index() const {
				return this->base_range().begin_index();
			}

			index end_index() const {
				return this->base_range().end_index();
			}

			bool at_end_index(index const& idx) const {
				return this->base_range().at_end_index(idx);
			}

			auto dereference_index(index const& idx)
				return_decltype( THIS_IN_DECLTYPE base_range().dereference_index(idx) )

			auto dereference_index(index const& idx) const
				return_decltype( make_const(THIS_IN_DECLTYPE base_range()).dereference_index(idx) )

			bool equal_index(index const& idxLhs, index const& idxRhs) const {
				return this->base_range().equal_index(idxLhs,idxRhs);
			}

			void increment_index(index& idx) const {
				this->base_range().increment_index(idx);
			}

			void decrement_index(index& idx) const {
				this->base_range().decrement_index(idx);
			}

			void advance_index(index& idx, typename boost::range_difference<typename std::remove_reference<Rng>::type>::type d) const {
				this->base_range().advance_index(idx,d);
			}

			typename boost::range_difference<typename std::remove_reference<Rng>::type>::type distance_to_index(index const& idxLhs, index const& idxRhs) const {
				return this->base_range().distance_to_index(idxLhs,idxRhs);
			}

			void middle_point( index & idxBegin, index const& idxEnd ) const {
				this->base_range().middle_point(idxBegin,idxEnd);
			}

			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

		private:
			template< typename DerivedConst >
			struct delayed_difference_type {
				typedef decltype( std::declval<DerivedConst const>().distance_to_index(std::declval<typename DerivedConst::index>(),std::declval<typename DerivedConst::index>()) ) type;
			};
			template< typename DerivedConst >
			struct range_traits {
			private:
				typedef typename boost::mpl::if_<
					std::is_const<DerivedConst>
					, typename std::remove_reference<Rng>::type const
					, typename std::remove_reference<Rng>::type
				>::type RngConst;
				typedef typename boost::mpl::if_< std::is_same<Traversal,boost::use_default>
					, boost::random_access_traversal_tag
					, Traversal
				>::type traversal_of_adaptor;
				static_assert( boost::detail::is_iterator_traversal<traversal_of_adaptor>::value, "" );
				typedef typename boost::iterator_traversal<
					typename boost::range_iterator<RngConst>::type
				>::type traversal_of_base;
				static_assert( boost::detail::is_iterator_traversal<traversal_of_base>::value, "" );
			public:
				typedef typename boost::range_detail::demote_iterator_traversal_tag<
					traversal_of_adaptor,
					traversal_of_base
				>::type traversal;
				static_assert( boost::detail::is_iterator_traversal<traversal>::value, "" );

				typedef decltype( std::declval<DerivedConst>().dereference_index(std::declval<typename DerivedConst::index>()) ) reference;

				typedef typename boost::mpl::if_<
					std::is_same<ValueType,boost::use_default>
					, typename boost::mpl::if_<
						// reference unchanged -> value_type also unchanged
						std::is_same<typename boost::range_reference< RngConst >::type,reference>
						, typename boost::range_value< RngConst >::type
						, typename std::decay<reference>::type
					>::type
					, ValueType
				>::type value_type;

				typedef typename boost::mpl::eval_if< std::is_convertible< traversal, boost::random_access_traversal_tag >,
					delayed_difference_type<DerivedConst>,
					std::common_type</*default of iterator_facade, needed to compile interfaces relying on difference_tye:*/std::ptrdiff_t>
				>::type difference_type;
			};
		public:
			template<typename DerivedConst>
			class common_iterator
			: public boost::iterator_facade<
				common_iterator<DerivedConst>
				, typename range_traits<DerivedConst>::value_type
				, typename range_traits<DerivedConst>::traversal
				, typename range_traits<DerivedConst>::reference
				, typename range_traits<DerivedConst>::difference_type
				>
			{
			private:
				typedef boost::iterator_facade<
					common_iterator<DerivedConst>
					, typename range_traits<DerivedConst>::value_type
					, typename range_traits<DerivedConst>::traversal
					, typename range_traits<DerivedConst>::reference
					, typename range_traits<DerivedConst>::difference_type
				> base_;
				friend class boost::iterator_core_access;
				friend class common_iterator;
				friend class range_adaptor;

				DerivedConst* m_pidxrng;
				typename DerivedConst::index m_idx;

				struct enabler {};

			public:
				typedef typename base_::reference reference;
				typedef typename base_::difference_type difference_type;

				common_iterator()
					: m_pidxrng(nullptr)
					, m_idx()
				{}

				template<typename OtherDerivedConst>
				common_iterator(
					common_iterator<OtherDerivedConst> const& other
				, typename boost::enable_if<
						std::is_convertible<OtherDerivedConst*,DerivedConst*>
					, enabler
					>::type = enabler()
				)
				: m_pidxrng(other.m_pidxrng)
				, m_idx(other.m_idx) {}

				common_iterator(DerivedConst* pidxrng, typename DerivedConst::index idx)
				: m_pidxrng(pidxrng)
				, m_idx(tc_move(idx)) {}

				reference dereference() const {
					return VERIFY(m_pidxrng)->dereference_index(m_idx);
				}

				template<typename OtherDerivedConst>
				bool equal(common_iterator<OtherDerivedConst> const& itRhs) const {
					return VERIFYEQUAL(VERIFY(m_pidxrng),itRhs.m_pidxrng)->equal_index(m_idx,itRhs.m_idx);
				}

				void increment() {
					VERIFY(m_pidxrng)->increment_index(m_idx);
				}

				void decrement() {
					VERIFY(m_pidxrng)->decrement_index(m_idx);
				}

				void advance(difference_type d) {
					VERIFY(m_pidxrng)->advance_index(m_idx,d);
				}

				template<typename OtherDerivedConst>
				difference_type distance_to(common_iterator<OtherDerivedConst> const& itRhs) const {
					return VERIFYEQUAL(VERIFY(m_pidxrng),itRhs.m_pidxrng)->distance_to_index(m_idx,itRhs.m_idx);
				}

				friend common_iterator middle_point( common_iterator const& itBegin, common_iterator const& itEnd ) {
					common_iterator it=itBegin;
					VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng)->middle_point(it.m_idx,itEnd.m_idx);
					return it;
				}

				auto base() const
					return_decltype( make_const(VERIFY(m_pidxrng))->base_range().make_iterator(m_idx) )

				// sub_range from iterator pair
				friend typename tc::make_sub_range_result< DerivedConst & >::type make_iterator_range_impl( common_iterator itBegin, common_iterator itEnd ) {
					return typename tc::make_sub_range_result< DerivedConst & >::type( *VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng), tc_move(itBegin).m_idx, tc_move(itEnd).m_idx );
				}
			};

			template<typename DerivedConst>
			static auto iterator2index( common_iterator<DerivedConst> const& it )
				decltype_return_ref( it.m_idx )

			typedef common_iterator< Derived >			iterator;
			typedef common_iterator< Derived const >	const_iterator;

			const_iterator make_iterator( index idx ) const {
				return const_iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			const_iterator begin() const {
				return make_iterator(tc::derived_cast<Derived>(this)->begin_index());
			}

			const_iterator end() const {
				return make_iterator(tc::derived_cast<Derived>(this)->end_index());
			}

			iterator make_iterator( index idx ) {
				return iterator(tc::derived_cast<Derived>(this),tc_move(idx));
			}

			iterator begin() {
				return make_iterator(tc::derived_cast<Derived>(this)->begin_index());
			}

			iterator end() {
				return make_iterator(tc::derived_cast<Derived>(this)->end_index());
			}

			bool empty() const {
				return tc::derived_cast<Derived>(this)->at_end_index(
					tc::derived_cast<Derived>(this)->begin_index()
				);
			}
		};
	}
	using range_adaptor_impl::range_adaptor;
}


