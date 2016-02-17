#pragma once

#include "range_defines.h"

#include "tc_move.h" 
#include "range_adaptor.h"
#include "meta.h"

namespace RANGE_PROPOSAL_NAMESPACE {

	namespace filter_adaptor_impl {

		template< typename Pred, typename Rng, bool HasIterator=is_range_with_iterators< Rng >::value >
		struct filter_adaptor;

		template< typename Pred, typename Rng >
		struct filter_adaptor<Pred, Rng, false> : range_adaptor<filter_adaptor<Pred,Rng>, Rng
			, boost::iterators::bidirectional_traversal_tag // filter_adaptor is bidirectional at best
		> {
        private:
			using base_ = range_adaptor<filter_adaptor<Pred,Rng>, Rng
				, boost::iterators::bidirectional_traversal_tag // filter_adaptor is bidirectional at best
			>;

		protected:
			static_assert( tc::is_decayed<Pred>::value, "" );
			Pred m_pred;

		private:
			friend struct range_adaptor_impl::range_adaptor_access;

			template< typename Apply, typename A0 >
			typename std::enable_if<
				std::is_same<
					tc::result_of_t< Apply( A0 )>,
					break_or_continue
				>::value,
				break_or_continue
			>::type
			apply(Apply&& apply, A0&& a0) const {
				if( m_pred( a0 ) ) return std::forward<Apply>(apply)(std::forward<A0>(a0));
				else return continue_;
			}

			template< typename Apply, typename A0 >
			typename std::enable_if<
				!std::is_same<
					tc::result_of_t< Apply( A0 )>,
					break_or_continue
				>::value
			>::type
			apply(Apply&& apply, A0&& a0) const {
				if( m_pred( a0 ) ) std::forward<Apply>(apply)(std::forward<A0>(a0));
			}

		public:
			// default ctor
			filter_adaptor() = default;
			filter_adaptor( filter_adaptor&& rng ) = default;
			filter_adaptor& operator=( filter_adaptor && rng ) = default;
			filter_adaptor( filter_adaptor const& rng ) = default;
			filter_adaptor& operator=( filter_adaptor const& rng ) = default;

			// other ctors
			template< typename RngRef, typename PredRef >
			filter_adaptor( RngRef&& rng, PredRef&& pred )
				: base_(std::forward<RngRef>(rng), aggregate_tag())
				, m_pred(std::forward<PredRef>(pred))
			{}
		};

		template< typename Pred, typename Rng >
		struct filter_adaptor<Pred, Rng, true> : filter_adaptor<Pred, Rng, false> {
			static_assert( 
				std::is_same< Rng, typename range_by_value<Rng>::type >::value,
				"adaptors must hold ranges by value"
			);
        private:
			using base_ = filter_adaptor<Pred, Rng, false>;
			using this_type = filter_adaptor;

		public:
			using typename base_::index;

		private:
			void increment_until_kept(index& idx) const {
				// always call operator() const, which is assumed to be thread-safe
				while(!base_::STATIC_VIRTUAL_METHOD_NAME(at_end_index)(idx) && !tc::bool_cast(this->m_pred(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx)))) {
					base_::STATIC_VIRTUAL_METHOD_NAME(increment_index)(idx);
				}
			}

		public:
			// default ctor
			filter_adaptor() {}

			filter_adaptor( filter_adaptor&& rng ) 
				: base_(tc::base_cast<base_>(tc_move(rng)))
			{}

			filter_adaptor& operator=( filter_adaptor&& rng ) {
				base_::operator=(tc::base_cast<base_>(tc_move(rng)));
				return *this;
			}

			filter_adaptor( filter_adaptor const& rng ) 
				: base_(tc::base_cast<base_>(rng))
			{}

			filter_adaptor& operator=( filter_adaptor const& rng ) {
				base_::operator=(tc::base_cast<base_>(rng));
				return *this;
			}

			// other ctors
			template< typename RngRef, typename PredRef >
			explicit filter_adaptor( RngRef&& rng, PredRef&& pred)
			:	base_( std::forward<RngRef>(rng)
			,	std::forward<PredRef>(pred))
			{}

			STATIC_FINAL(begin_index)() const -> index {
				index idx=base_::STATIC_VIRTUAL_METHOD_NAME(begin_index)();
				increment_until_kept(idx);
				return idx;
			}

			STATIC_FINAL(increment_index)(index& idx) const -> void {
				base_::STATIC_VIRTUAL_METHOD_NAME(increment_index)(idx);
				increment_until_kept(idx);
			}

			void decrement_index(index& idx) const {
				do {
					base_::decrement_index(idx);
					// always call operator() const, which is assumed to be thread-safe
				} while(!tc::bool_cast(this->m_pred(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx))));
			}

			void advance_index() const;
			void distance_to_index() const;

			void middle_point( index & idx, index const& idxEnd ) const {
				index const idxBegin = idx;
				base_::middle_point(idx,idxEnd);
			
				// always call operator() const, which is assumed to be thread-safe
				while(!base_::equal_index(idxBegin, idx) && !tc::bool_cast(this->m_pred(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx)))) {
					base_::decrement_index(idx);
				}
			}
		};
	}
	using filter_adaptor_impl::filter_adaptor;

	template<typename Rng, typename Pred>
	auto filter(Rng&& rng, Pred&& pred)
		return_ctor( filter_adaptor<std::decay_t<Pred> BOOST_PP_COMMA() typename range_by_value<Rng>::type >, (std::forward<Rng>(rng),std::forward<Pred>(pred)) )
}
