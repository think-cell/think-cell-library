#pragma once

#include "range_defines.h"

#include "index_range.h"
#include "meta.h"

#include "Library/Utilities/casts.h"

#include <boost/iterator/iterator_facade.hpp>
#include <boost/mpl/has_xxx.hpp>

#include <type_traits>
#include <boost/range/detail/demote_iterator_traversal_tag.hpp>

namespace RANGE_PROPOSAL_NAMESPACE {

	template< typename DerivedConst >
	struct delayed_difference_type {
		typedef decltype(std::declval<DerivedConst const>().distance_to_index(std::declval<typename DerivedConst::index>(), std::declval<typename DerivedConst::index>())) type;
	};

	template< typename Rng >
	struct range_traits {
	public:
		using IndexRange = typename index_range<typename std::remove_reference<Rng>::type>::type;

		typedef decltype(std::declval<IndexRange>().dereference_index(std::declval<typename IndexRange::index>())) reference;

		typedef typename std::decay<reference>::type value_type;

		template<typename Traversal>
		struct difference_type {
			using type =
				typename boost::mpl::eval_if_c<
					std::is_convertible< Traversal, boost::iterators::random_access_traversal_tag >::value,
					delayed_difference_type<IndexRange>,
					boost::mpl::identity<
						/*default of iterator_facade, needed to compile interfaces relying on difference_tye:*/
						std::ptrdiff_t
					>
				>::type;
		};
	};
	template<typename Rng, typename Traversal>
	using range_difference_type = typename range_traits<Rng>::template difference_type<Traversal>::type;

	//////////////////////////////////////////////////////////
	// range adaptors
	//
	// Basic building block for all ranges.
	// Comes in two variations, one for generator ranges, one for iterator ranges. 
	//
	namespace range_iterator_from_index_impl {

		template<typename T>
		struct sfinae_has_member_function_base_range {
			using type2 = decltype(std::declval<T const>().base_range());
			using type = void;
		};

		template<
			typename Derived,
			typename Index,
			typename Traversal
		>
		struct range_iterator_from_index {
			static_assert( boost::iterators::detail::is_iterator_traversal<Traversal>::value, "" );
			////////////////////////////////////////////////////////
			// simulate iterator interface on top of index interface

			typedef Index index;

		public:
			template<typename DerivedConst>
			class common_iterator
			: public boost::iterators::iterator_facade<
				common_iterator<DerivedConst>
				, typename range_traits<DerivedConst>::value_type
				, Traversal
				, typename range_traits<DerivedConst>::reference
				, range_difference_type<DerivedConst,Traversal>
				>
			{
			private:
				typedef boost::iterators::iterator_facade<
					common_iterator<DerivedConst>
					, typename range_traits<DerivedConst>::value_type
					, Traversal
					, typename range_traits<DerivedConst>::reference
					, range_difference_type<DerivedConst,Traversal>
				> base_;
				friend class boost::iterator_core_access;
				friend class common_iterator;
				friend struct range_iterator_from_index;

				DerivedConst* m_pidxrng;
				index m_idx;

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
				, typename std::enable_if<
						std::is_convertible<OtherDerivedConst*,DerivedConst*>::value
					, enabler
					>::type = enabler()
				)
				: m_pidxrng(other.m_pidxrng)
				, m_idx(other.m_idx) {}

				common_iterator(DerivedConst* pidxrng, index idx)
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

				template<
					typename IndexRange = DerivedConst,
					typename sfinae_has_member_function_base_range<IndexRange>::type* = nullptr
				>
				auto base() const -> decltype( make_const(std::declval<IndexRange*>())->base_range().make_iterator(m_idx) )
				{
					return make_const(VERIFY(m_pidxrng))->base_range().make_iterator(m_idx);
				}

				// sub_range from iterator pair
				friend typename tc::make_sub_range_result< DerivedConst & >::type make_iterator_range_impl( common_iterator itBegin, common_iterator itEnd ) {
					return typename tc::make_sub_range_result< DerivedConst & >::type( *VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng), tc_move(itBegin).m_idx, tc_move(itEnd).m_idx );
				}
			};

			template<typename DerivedConst>
			static index const& iterator2index( common_iterator<DerivedConst> const& it ) {
				return it.m_idx;
			}

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
	using range_iterator_from_index_impl::range_iterator_from_index;

	namespace range_adaptor_impl {
		struct range_adaptor_access {
			template< typename Derived, typename... Args>
			auto operator()( Derived && derived, Args&&... args) return_decltype(
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
		
		public: // protected:
			// workaround for clang compiler bug http://llvm.org/bugs/show_bug.cgi?id=19140
			// friend struct range_adaptor<Derived, Rng, Traversal, true>;  // fixes the clang issue but not gcc
			typedef typename std::remove_reference< typename index_range<Rng>::type >::type BaseRange;

		protected:
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
				typedef typename std::conditional< 
					is_const_compatible_range<RngOther const&>::value,
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

				template<typename... Args> break_or_continue operator()(Args&&... args) const {
					return continue_if_void(
						range_adaptor_access(),
						m_derived,
						m_func,
						std::forward<Args>(args)...
					);
				}
			};

			template< typename Func >
			class adaptor<Func, false> {
				Derived const& m_derived;
				typename std::decay<Func>::type mutable m_func;

			public:
				adaptor( Derived const& derived, Func && func )
					: m_derived( derived )
					, m_func( std::forward<Func>(func) ) {}

				template<typename... Args>
				void operator()(Args&&... args) const {
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
					typename tc::result_of<BaseRange(adaptor<Func, true>)>::type,
					break_or_continue
				>::value,
				break_or_continue
			>::type
			operator()(Func && func) {
				return base_range()(adaptor<Func, true>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}

			template< typename Func >
			typename std::enable_if<
				!std::is_same<
					typename tc::result_of<BaseRange(adaptor<Func, true>)>::type,
					break_or_continue
				>::value &&
				std::is_same<
					typename tc::result_of<BaseRange(adaptor<Func, false>)>::type,
					void
				>::value
			>::type
			operator()(Func && func) {
				return base_range()(adaptor<Func, false>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}

			template< typename Func >
			typename std::enable_if<
				std::is_same<
					typename tc::result_of<BaseRange(adaptor<Func, true>)>::type,
					break_or_continue
				>::value,
				break_or_continue
			>::type
			operator()(Func && func) const {
				return base_range()(adaptor<Func, true>(derived_cast<Derived>(*this), std::forward<Func>(func)));
			}

			template< typename Func >
			typename std::enable_if<
				!std::is_same<
					typename tc::result_of<BaseRange(adaptor<Func, true>)>::type,
					break_or_continue
				>::value &&
				std::is_same<
					typename tc::result_of<BaseRange(adaptor<Func, false>)>::type,
					void
				>::value
			>::type
			operator()(Func && func) const {
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
				typename std::conditional< std::is_same<Traversal,boost::iterators::use_default>::value
					, boost::iterators::random_access_traversal_tag
					, Traversal
				>::type,
				typename boost::iterator_traversal<
					typename boost::range_iterator<typename std::remove_reference<Rng>::type>::type
				>::type
			>::type
		>
		{
		private:
			typedef range_adaptor<Derived,Rng,Traversal,false> base_;
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
		};
	}
	using range_adaptor_impl::range_adaptor;
}


