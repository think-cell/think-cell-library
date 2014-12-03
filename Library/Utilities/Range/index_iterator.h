#pragma once
#include "index_range.h"
#include "range_defines.h"

namespace RANGE_PROPOSAL_NAMESPACE {

	template< typename DerivedConst >
	struct delayed_difference_type {
		typedef decltype(std::declval<DerivedConst const>().distance_to_index(std::declval<typename DerivedConst::index>(), std::declval<typename DerivedConst::index>())) type;
	};

	template< typename Rng >
	struct range_traits {
		using IndexRange = typename index_range<typename std::remove_reference<Rng>::type>::type;

		typedef decltype(std::declval<IndexRange>().dereference_index(std::declval<typename IndexRange::index>())) reference;
		typedef decltype(std::declval<IndexRange const>().begin_index()) index;

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
		struct conditional_const {
			typedef typename std::conditional< bConst, T const, T >::type type;
		};

		template<typename IndexRange, typename Traversal, bool bConst>
		struct index_iterator
		: boost::iterators::iterator_facade<
			index_iterator<IndexRange,Traversal,bConst>
			, typename range_traits<typename conditional_const<IndexRange,bConst>::type>::value_type
			, Traversal
			, typename range_traits<typename conditional_const<IndexRange,bConst>::type>::reference
			, range_difference_type<typename conditional_const<IndexRange,bConst>::type,Traversal>
			>
		{
			static_assert( std::is_same< IndexRange, typename std::decay<IndexRange>::type >::value, "" );

		private:
			typedef boost::iterators::iterator_facade<
				index_iterator<IndexRange,Traversal,bConst>
				, typename range_traits<typename conditional_const<IndexRange,bConst>::type>::value_type
				, Traversal
				, typename range_traits<typename conditional_const<IndexRange,bConst>::type>::reference
				, range_difference_type<typename conditional_const<IndexRange,bConst>::type,Traversal>
			> base_;
			friend class boost::iterator_core_access;
			friend struct index_iterator<IndexRange,Traversal,!bConst>;

			typename conditional_const<IndexRange,bConst>::type* m_pidxrng;
			typedef typename range_traits<IndexRange>::index index;
			index m_idx;

			template<typename IndexRange, typename Traversal, bool bConst>
			friend typename index_iterator<IndexRange,Traversal,bConst>::index const& RANGE_PROPOSAL_NAMESPACE::iterator2index(index_iterator<IndexRange,Traversal,bConst> const& it);

			struct enabler {};

		public:
			typedef typename base_::reference reference;
			typedef typename base_::difference_type difference_type;

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

			index_iterator(typename conditional_const<IndexRange,bConst>::type* pidxrng, index idx)
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
			auto base() const -> decltype( make_const(std::declval<typename conditional_const<IndexRange_,bConst>::type*>())->base_range().make_iterator(m_idx) )
			{
				return make_const(VERIFY(m_pidxrng))->base_range().make_iterator(m_idx);
			}

			// sub_range from iterator pair
			friend typename tc::make_sub_range_result< typename conditional_const<IndexRange,bConst>::type & >::type make_iterator_range_impl( index_iterator itBegin, index_iterator itEnd ) {
				return typename tc::make_sub_range_result< typename conditional_const<IndexRange,bConst>::type & >::type( *VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng), tc_move(itBegin).m_idx, tc_move(itEnd).m_idx );
			}
		};
	}
	using index_iterator_impl::index_iterator;

	template<typename IndexRange, typename Traversal, bool bConst>
	typename index_iterator_impl::index_iterator<IndexRange,Traversal,bConst>::index const& iterator2index(index_iterator_impl::index_iterator<IndexRange,Traversal,bConst> const& it) {
		return it.m_idx;
	}
}