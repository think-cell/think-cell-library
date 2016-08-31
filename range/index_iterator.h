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
#include "index_range.h"
#include "range_defines.h"

#pragma warning( push )
#pragma warning( disable: 4146 )
// warning C4146: unary minus operator applied to unsigned type, result still unsigned
// iterator_facade::distance_from returns -iterator_facade::distance_to which causes warning C4146 if difference_type is std::size_t
#include <boost/iterator/iterator_facade.hpp> 
#pragma warning( pop )

namespace tc {

	template< typename DerivedConst >
	struct delayed_difference_type {
		using type = decltype(std::declval<DerivedConst const&>().distance_to_index(std::declval<typename DerivedConst::index>(), std::declval<typename DerivedConst::index>()));
	};

	template< typename Rng >
	struct range_traits {
		using IndexRange = std::remove_reference_t<index_range_t<Rng>>;

		using reference = decltype(std::declval<IndexRange&>().dereference_index(std::declval<typename IndexRange::index>()));
		using index = decltype(std::declval<IndexRange const&>().begin_index());

		using value_type = tc::decay_t<reference>;

		template<typename Traversal>
		struct difference_type final {
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

	template<typename It, typename Enable=void >
	struct element {
		static_assert( tc::is_decayed<It>::value, "" );
		struct type : It {
			using difference_type=typename std::iterator_traits<It>::difference_type;
			using value_type=typename std::iterator_traits<It>::value_type;
			using pointer=typename std::iterator_traits<It>::pointer;
			using reference=typename std::iterator_traits<It>::reference;
			using iterator_category=typename std::iterator_traits<It>::iterator_category;

			type() noexcept : m_bValid(false) {}

			// no implicit conversion from It; we must ensure that It has the semantics of element rather than border before allowing the construction
			explicit type(It const& it) noexcept : It(it), m_bValid(true) {}
			explicit type(It && it) noexcept : It(tc_move(it)), m_bValid(true) {}

			explicit operator bool() const& noexcept {
				return m_bValid;
			}

			type& operator++() & noexcept {
				_ASSERT(m_bValid);
				++tc::base_cast<It>(*this);
				return *this;
			}

			type& operator--() & noexcept {
				_ASSERT(m_bValid);
				--tc::base_cast<It>(*this);
				return *this;
			}

			reference operator*() const& noexcept {
				_ASSERT(m_bValid);
				return *tc::base_cast<It>(*this);
			}

			pointer operator->() const& noexcept {
				_ASSERT(m_bValid);
				return tc::base_cast<It>(*this).operator->();
			}

		private:
			bool m_bValid;
		};
	};

	template<typename It>
	struct element< It, std::enable_if_t< has_bool_cast<It>::value > > {
		static_assert( tc::is_decayed<It>::value, "" );
		using type=It;
	};

	template< typename It >
	using element_t=typename element<It>::type;

	namespace index_iterator_impl {
		template<typename IndexRange, typename Traversal, bool bConst>
		struct index_iterator;
	}

	template<typename IndexRange, typename Traversal, bool bConst>
	typename index_iterator_impl::index_iterator<IndexRange,Traversal,bConst>::index const& iterator2index(index_iterator_impl::index_iterator<IndexRange,Traversal,bConst> const& it) noexcept;

	namespace index_iterator_impl {

		template< typename T, bool bConst >
		using conditional_const_t=std::conditional_t< bConst, T const, T >;

		template<typename IndexRange, typename Traversal, bool bConst>
		struct index_iterator final
		: boost::iterators::iterator_facade<
			index_iterator<IndexRange,Traversal,bConst>
			, typename range_traits< conditional_const_t<IndexRange,bConst> >::value_type
			, Traversal
			, typename range_traits< conditional_const_t<IndexRange,bConst> >::reference
			, range_difference_type< conditional_const_t<IndexRange,bConst>,Traversal>
		>
		{
			static_assert( tc::is_decayed< IndexRange >::value, "" );

		private:
			friend class boost::iterator_core_access;
			friend struct index_iterator<IndexRange,Traversal,!bConst>;

			conditional_const_t<IndexRange,bConst>* m_pidxrng;
			using index = typename range_traits<IndexRange>::index;
			index m_idx;

			template<typename IndexRange, typename Traversal, bool bConst>
			friend typename index_iterator<IndexRange,Traversal,bConst>::index const& tc::iterator2index(index_iterator<IndexRange,Traversal,bConst> const& it) noexcept;

			struct enabler final {};

		public:
			using reference = typename index_iterator::reference;
			using difference_type = typename index_iterator::difference_type;

			index_iterator() noexcept
				: m_pidxrng(nullptr)
			{}

			template<bool bConstOther>
			index_iterator(
				index_iterator<IndexRange,Traversal,bConstOther> const& other
			, std::enable_if_t<
					bConst || !bConstOther
				, enabler
				> = enabler()
			) noexcept
			: m_pidxrng(other.m_pidxrng)
			, m_idx(other.m_idx) {}

			index_iterator( conditional_const_t<IndexRange,bConst>* pidxrng, index idx) noexcept
			: m_pidxrng(pidxrng)
			, m_idx(tc_move(idx)) {}

			reference dereference() const& MAYTHROW {
				return VERIFY(m_pidxrng)->dereference_index(m_idx);
			}

			index const& get_index() const& noexcept {
				return m_idx;
			}

			template<bool bConstOther>
			bool equal(index_iterator<IndexRange,Traversal,bConstOther> const& itRhs) const& noexcept {
				return VERIFYEQUAL(VERIFY(m_pidxrng),itRhs.m_pidxrng)->equal_index(m_idx,itRhs.m_idx);
			}

			void increment() & noexcept {
				VERIFY(m_pidxrng)->increment_index(m_idx);
			}

			void decrement() & noexcept {
				VERIFY(m_pidxrng)->decrement_index(m_idx);
			}

			void advance(difference_type d) & noexcept {
				VERIFY(m_pidxrng)->advance_index(m_idx,d);
			}

			template<bool bConstOther>
			difference_type distance_to(index_iterator<IndexRange,Traversal,bConstOther> const& itRhs) const& noexcept {
				return VERIFYEQUAL(VERIFY(m_pidxrng),itRhs.m_pidxrng)->distance_to_index(m_idx,itRhs.m_idx);
			}

			friend index_iterator middle_point( index_iterator const& itBegin, index_iterator const& itEnd ) noexcept {
				index_iterator it=itBegin;
				VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng)->middle_point(it.m_idx,itEnd.m_idx);
				return it;
			}

			template<typename IndexRange_ = IndexRange>
			auto bound_base() const& noexcept {
				return tc::as_const(VERIFY(m_pidxrng))->base_range().make_iterator(m_pidxrng->bound_base_index(m_idx));
			}

			template<typename IndexRange_ = IndexRange>
			auto element_base() const& noexcept {
				using It=tc::element_t< decltype( tc::as_const(VERIFY(m_pidxrng))->base_range().make_iterator(m_pidxrng->element_base_index(m_idx)) ) >;
				if(m_pidxrng) {
					return static_cast<It>( tc::as_const(VERIFY(m_pidxrng))->base_range().make_iterator(m_pidxrng->element_base_index(m_idx)) );
				} else {
					return It{};
				}
			}

			// sub_range from iterator pair
			friend typename tc::make_sub_range_result< conditional_const_t<IndexRange,bConst> & >::type make_iterator_range_impl( index_iterator itBegin, index_iterator itEnd ) noexcept {
				return typename tc::make_sub_range_result< conditional_const_t<IndexRange,bConst> & >::type( *VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng), tc_move(itBegin).m_idx, tc_move(itEnd).m_idx );
			}

			explicit operator bool() const& noexcept {
				return tc::bool_cast(m_pidxrng);
			}
		};
	}
	using index_iterator_impl::index_iterator;

	template<typename IndexRange, typename Traversal, bool bConst>
	typename index_iterator_impl::index_iterator<IndexRange,Traversal,bConst>::index const& iterator2index(index_iterator_impl::index_iterator<IndexRange,Traversal,bConst> const& it) noexcept {
		return it.m_idx;
	}
}