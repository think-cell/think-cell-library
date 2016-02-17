#pragma once
#include "index_range.h"
#include "range_defines.h"

namespace RANGE_PROPOSAL_NAMESPACE {

	template< typename DerivedConst >
	struct delayed_difference_type {
		using type = decltype(std::declval<DerivedConst const>().distance_to_index(std::declval<typename DerivedConst::index>(), std::declval<typename DerivedConst::index>()));
	};

	template< typename Rng >
	struct range_traits {
		using IndexRange = typename index_range<std::remove_reference_t<Rng>>::type;

		using reference = decltype(std::declval<IndexRange>().dereference_index(std::declval<typename IndexRange::index>()));
		using index = decltype(std::declval<IndexRange const>().begin_index());

		using value_type = std::decay_t<reference>;

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

	namespace index_iterator_impl {
		template<typename IndexRange, typename Traversal, bool bConst>
		struct index_iterator;
	}

	template<typename IndexRange, typename Traversal, bool bConst>
	typename index_iterator_impl::index_iterator<IndexRange,Traversal,bConst>::index const& iterator2index(index_iterator_impl::index_iterator<IndexRange,Traversal,bConst> const& it);

	namespace index_iterator_impl {
		template<typename T>
		struct sfinae_has_member_function_base_range {
			using type2 = decltype(std::declval<T const>().base_range());
			using type = void;
		};

		template< typename T, bool bConst >
		using conditional_const_t=std::conditional_t< bConst, T const, T >;

		template<typename IndexRange, typename Traversal, bool bConst>
		struct index_iterator
		: boost::iterators::iterator_facade<
			index_iterator<IndexRange,Traversal,bConst>
			, typename range_traits< conditional_const_t<IndexRange,bConst> >::value_type
			, Traversal
			, typename range_traits< conditional_const_t<IndexRange,bConst> >::reference
			, range_difference_type< conditional_const_t<IndexRange,bConst>,Traversal>
			>
		{
			static_assert( std::is_same< IndexRange, std::decay_t<IndexRange> >::value, "" );

		private:
			using base_ = boost::iterators::iterator_facade<
				index_iterator<IndexRange,Traversal,bConst>
				, typename range_traits< conditional_const_t<IndexRange,bConst>>::value_type
				, Traversal
				, typename range_traits< conditional_const_t<IndexRange,bConst>>::reference
				, range_difference_type< conditional_const_t<IndexRange,bConst>,Traversal>
			>;
			friend class boost::iterator_core_access;
			friend struct index_iterator<IndexRange,Traversal,!bConst>;

			conditional_const_t<IndexRange,bConst>* m_pidxrng;
			using index = typename range_traits<IndexRange>::index;
			index m_idx;

			template<typename IndexRange, typename Traversal, bool bConst>
			friend typename index_iterator<IndexRange,Traversal,bConst>::index const& RANGE_PROPOSAL_NAMESPACE::iterator2index(index_iterator<IndexRange,Traversal,bConst> const& it);

			struct enabler {};

		public:
			using reference = typename base_::reference;
			using difference_type = typename base_::difference_type;

			index_iterator()
				: m_pidxrng(nullptr)
				, m_idx()
			{}

			template<bool bConstOther>
			index_iterator(
				index_iterator<IndexRange,Traversal,bConstOther> const& other
			, typename std::enable_if<
					bConst || !bConstOther
				, enabler
				>::type = enabler()
			)
			: m_pidxrng(other.m_pidxrng)
			, m_idx(other.m_idx) {}

			index_iterator( conditional_const_t<IndexRange,bConst>* pidxrng, index idx)
			: m_pidxrng(pidxrng)
			, m_idx(tc_move(idx)) {}

			reference dereference() const {
				return VERIFY(m_pidxrng)->dereference_index(m_idx);
			}

			template<bool bConstOther>
			bool equal(index_iterator<IndexRange,Traversal,bConstOther> const& itRhs) const {
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

			template<bool bConstOther>
			difference_type distance_to(index_iterator<IndexRange,Traversal,bConstOther> const& itRhs) const {
				return VERIFYEQUAL(VERIFY(m_pidxrng),itRhs.m_pidxrng)->distance_to_index(m_idx,itRhs.m_idx);
			}

			friend index_iterator middle_point( index_iterator const& itBegin, index_iterator const& itEnd ) {
				index_iterator it=itBegin;
				VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng)->middle_point(it.m_idx,itEnd.m_idx);
				return it;
			}

			template<
				typename IndexRange_ = IndexRange,
				typename sfinae_has_member_function_base_range<IndexRange_>::type* = nullptr
			>
			auto base() const -> decltype( make_const(std::declval< conditional_const_t<IndexRange_,bConst>*>())->base_range().make_iterator(m_idx) )
			{
				return make_const(VERIFY(m_pidxrng))->base_range().make_iterator(m_idx);
			}

			// sub_range from iterator pair
			friend typename tc::make_sub_range_result< conditional_const_t<IndexRange,bConst> & >::type make_iterator_range_impl( index_iterator itBegin, index_iterator itEnd ) {
				return typename tc::make_sub_range_result< conditional_const_t<IndexRange,bConst> & >::type( *VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng), tc_move(itBegin).m_idx, tc_move(itEnd).m_idx );
			}

			explicit operator bool() const {
				return tc::bool_cast(m_pidxrng);
			}
		};
	}
	using index_iterator_impl::index_iterator;

	template<typename IndexRange, typename Traversal, bool bConst>
	typename index_iterator_impl::index_iterator<IndexRange,Traversal,bConst>::index const& iterator2index(index_iterator_impl::index_iterator<IndexRange,Traversal,bConst> const& it) {
		return it.m_idx;
	}
}