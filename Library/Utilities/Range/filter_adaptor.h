#pragma once

#include "range_defines.h"

#include "Library/ErrorReporting/tc_move.h" 
#include "range_adaptor.h"
#include "meta.h"

namespace RANGE_PROPOSAL_NAMESPACE {

	namespace filter_adaptor_impl {

		template< typename Pred, typename Rng, bool HasIterator=is_range_with_iterators< Rng >::value >
		class filter_adaptor;

		template< typename Pred, typename Rng >
		class filter_adaptor<Pred, Rng, false> : public range_adaptor<filter_adaptor<Pred,Rng>, Rng, typename std::is_empty<Pred>::type
			, boost::use_default
			, boost::bidirectional_traversal_tag // filter_adaptor is bidirectional at best
		> {
			typedef range_adaptor<filter_adaptor<Pred,Rng>, Rng, typename std::is_empty<Pred>::type
				, boost::use_default
				, boost::bidirectional_traversal_tag // filter_adaptor is bidirectional at best
			> base_;

		protected:
			static_assert( !std::is_reference<Pred>::value, "" );
			static_assert( !std::is_const<Pred>::value, "" );
			Pred m_pred;

		private:
			friend class range_adaptor_impl::range_adaptor_access;
			template< typename Apply, typename A0 >
			break_or_continue apply(Apply && apply, A0 && a0) const {
				if( m_pred( a0 ) ) return continue_if_void( std::forward<Apply>(apply), std::forward<A0>(a0) );
				else return continue_;
			}

		public:
			typedef void ctor_const_overload_support;

			// default ctor
			filter_adaptor() {}

			// Range adaptors other than sub_ranges should not be copyable.
			// Otherwise, copying a range adaptor may have value or reference semantics,
			// depending whether the base range is by-value or by-reference.
			// Instead, the caller should explicit decide, and either
			// - store a reference or sub_range of the range,
			// - or copy the values into another container.

			filter_adaptor( filter_adaptor && rng ) 
				: base_(tc_move(rng).base_range_move(), aggregate_tag())
				, m_pred(tc_move(rng).m_pred)
			{}

			filter_adaptor& operator=( filter_adaptor && rng ) {
				base_::operator=(tc_move(rng).base_range_move(), aggregate_tag());
				m_pred=tc_move(rng).m_pred;
				return *this;
			}

		protected:
			filter_adaptor( filter_adaptor const& rng ) 
				: base_(rng.base_range(), aggregate_tag())
				, m_pred(rng.m_pred)
			{}

			filter_adaptor& operator=( filter_adaptor const& rng ) {
				base_::operator=(rng.base_range(), aggregate_tag());
				m_pred=rng.m_pred;
				return *this;
			}

		public:
			// templated copy ctors
/*			template< typename RngOther, typename PredOther, bool bHasIteratorOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,bHasIteratorOther> & rng, ctor_const_overload=ctor_const_overload() )
				: base_(rng.base_range(), aggregate_tag())
				, m_pred(rng.m_pred)
			{}

			template< typename RngOther, typename PredOther, bool bHasIteratorOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,bHasIteratorOther> && rng, ctor_const_overload=ctor_const_overload() )
				: base_(tc_move(rng).base_range_move(), aggregate_tag())
				, m_pred(tc_move(rng).m_pred)
			{}

			template< typename RngOther, typename PredOther, bool bHasIteratorOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,bHasIteratorOther> const& rng, ctor_const_overload )
				: base_(rng.base_range(), aggregate_tag())
				, m_pred(rng.m_pred)
			{}

			template< typename RngOther, typename PredOther, bool bHasIteratorOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,bHasIteratorOther> const& rng, typename std::enable_if<
				base_::template is_const_compatible_range<filter_adaptor<PredOther,RngOther,bHasIteratorOther> const&>::value
			, unused_arg>::type=unused_arg() )
				: base_(rng.base_range(), aggregate_tag())
				, m_pred(rng.m_pred)
			{}

			// some user-defined copy ctor to disable implicit one, with same semantics as templated copy ctor
			filter_adaptor( typename base_::template const_compatible_range<filter_adaptor>::type rng )
				: base_(rng.base_range(), aggregate_tag())
				, m_pred(rng.m_pred)
			{}*/

			// other ctors
			template< typename RngRef, typename PredRef >
			filter_adaptor( RngRef && rng, PredRef && pred, typename std::enable_if< !std::is_same<typename std::remove_reference<PredRef>::type, ctor_const_overload>::value, unused_arg>::type=unused_arg() )
				: base_(std::forward<RngRef>(rng), aggregate_tag())
				, m_pred(std::forward<PredRef>(pred))
			{}
		};

		template< typename Pred, typename Rng >
		class filter_adaptor<Pred, Rng, true> : public filter_adaptor<Pred, Rng, false> {
			typedef filter_adaptor<Pred, Rng, false> base_;

		public:
			using typename base_::index;
			typedef void ctor_const_overload_support;

		private:
			void increment_until_kept(index& idx) const {
				// always call operator() const, which is assumed to be thread-safe
				while(!base_::at_end_index(idx) && !boost::implicit_cast<bool>(this->m_pred(base_::dereference_index(idx)))) {
					base_::increment_index(idx);
				}
			}

		public:
			// default ctor
			filter_adaptor() {}

			filter_adaptor( filter_adaptor && rng ) 
				: base_(tc::base_cast<base_>(tc_move(rng)))
			{}

			filter_adaptor& operator=( filter_adaptor && rng ) {
				base_::operator=(tc::base_cast<base_>(tc_move(rng)));
				return *this;
			}

		protected:
			filter_adaptor( filter_adaptor const& rng ) 
				: base_(tc::base_cast<base_>(rng))
			{}

			filter_adaptor& operator=( filter_adaptor const& rng ) {
				base_::operator=(tc::base_cast<base_>(rng));
				return *this;
			}

		public:
/*			// templated copy ctors
			template< typename RngOther, typename PredOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,true> & rng, ctor_const_overload=ctor_const_overload() )
				: base_(rng)
			{}

			template< typename RngOther, typename PredOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,true> && rng, ctor_const_overload=ctor_const_overload() )
				: base_(tc_move(rng))
			{}

			template< typename RngOther, typename PredOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,true> const& rng, ctor_const_overload )
				: base_(rng,ctor_const_overload())
			{}

			template< typename RngOther, typename PredOther >
			filter_adaptor( filter_adaptor<PredOther,RngOther,true> const& rng, typename std::enable_if<
				base_::template is_const_compatible_range<filter_adaptor<PredOther,RngOther,true> const&>::value
			, unused_arg>::type=unused_arg() )
				: base_(rng)
			{}

			// some user-defined copy ctor to disable implicit one, with same semantics as templated copy ctor
			filter_adaptor( typename base_::template const_compatible_range<filter_adaptor>::type rng )
				: base_(rng)
			{}*/

			// other ctors
			template< typename RngRef, typename PredRef >
			filter_adaptor( RngRef && rng, PredRef && pred, typename std::enable_if< !std::is_same<typename std::remove_reference<PredRef>::type, ctor_const_overload>::value, unused_arg>::type=unused_arg() )
			:	base_( std::forward<RngRef>(rng)
			,	std::forward<PredRef>(pred))
			{}

			index begin_index() const {
				index idx=base_::begin_index();
				increment_until_kept(idx);
				return idx;
			}

			void increment_index(index& idx) const {
				base_::increment_index(idx);
				increment_until_kept(idx);
			}

			void decrement_index(index& idx) const {
				do {
					base_::decrement_index(idx);
					// always call operator() const, which is assumed to be thread-safe
				} while(!boost::implicit_cast<bool>(m_pred(base_::dereference_index(idx))));
			}

			void advance_index() const;
			void distance_to_index() const;

			void middle_point( index & idx, index const& idxEnd ) const {
				index const idxBegin = idx;
				base_::middle_point(idx,idxEnd);
			
				// always call operator() const, which is assumed to be thread-safe
				while(!base_::equal_index(idxBegin, idx) && !boost::implicit_cast<bool>(this->m_pred(base_::dereference_index(idx)))) {
					base_::decrement_index(idx);
				}
			}
		};
	}
	using filter_adaptor_impl::filter_adaptor;

	template<typename Rng, typename Pred>
	auto filter( Rng && rng, Pred && pred )
		return_ctor( filter_adaptor<typename std::decay<Pred>::type BOOST_PP_COMMA() Rng >, (std::forward<Rng>(rng),std::forward<Pred>(pred)) )
}
