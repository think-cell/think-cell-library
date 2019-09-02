
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "index_range.h"
#include "range_defines.h"

#pragma warning( push )
#pragma warning( disable: 4146 )
// warning C4146: unary minus operator applied to unsigned type, result still unsigned
// iterator_facade::distance_from returns -iterator_facade::distance_to which causes warning C4146 if difference_type is std::size_t
#include <boost/iterator/iterator_facade.hpp> 
#pragma warning( pop )

#include <boost/mpl/identity.hpp>

namespace tc {
	TC_HAS_EXPR(bool_cast, (T), tc::bool_cast(std::declval<T>()))
	TC_HAS_MEM_FN_XXX_TRAIT_DEF(equal_index, const&, std::declval<typename T::index const&>(), std::declval<typename T::index const&>());

	template< typename DerivedConst >
	struct delayed_difference_type {
		using type = decltype(std::declval<DerivedConst const&>().distance_to_index(std::declval<typename DerivedConst::index>(), std::declval<typename DerivedConst::index>()));
	};

	template< typename Rng >
	struct range_traits {

		using IndexRange = std::remove_reference_t<Rng>;
		static_assert( has_index<IndexRange>::value );

		using reference = decltype(std::declval<IndexRange&>().dereference_index(std::declval<typename IndexRange::index>()));
		using index = decltype(std::declval<IndexRange const&>().begin_index());

		using value_type = tc::decay_t<reference>;

		template<typename Traversal>
		struct difference_type final : std::conditional_t<
			std::is_convertible< Traversal, boost::iterators::random_access_traversal_tag >::value,
			delayed_difference_type<IndexRange>,
			boost::mpl::identity<
				/*default of iterator_facade, needed to compile interfaces relying on difference_type:*/
				std::ptrdiff_t
			>
		> {};
	};

	template<typename Rng, typename Traversal>
	using range_difference_type = typename range_traits<Rng>::template difference_type<Traversal>::type;

	template<typename It, typename Enable=void >
	struct element {
		static_assert( tc::is_decayed<It>::value );
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
			
			type& operator=(It const& it) & noexcept {
				It::operator=(it);
				m_bValid=true;
				return *this;
			}
			type& operator=(It && it) & noexcept {
				It::operator=(tc_move(it));
				m_bValid=true;
				return *this;
			}

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

			template <typename It_ = It>
			auto element_base() const& noexcept {
				using ItBase = element<decltype(tc::base_cast<It>(*this).element_base())>;
				return m_bValid ? ItBase{tc::base_cast<It>(*this).element_base()} : ItBase{};
			}

		private:
			bool m_bValid;
		};
	};

	template<typename It>
	struct element< It, std::enable_if_t< has_bool_cast<It>::value > > {
		static_assert( tc::is_decayed<It>::value );
		using type=It;
	};

	template< typename It >
	using element_t=typename element<It>::type;

	template<typename T, typename Func>
	auto not_singleton_or(T&& t, Func func) noexcept->decltype(func()) {
		if(t) {
			return std::forward<T>(t);
		} else {
			return func();
		}
	}

	namespace index_iterator_impl {
		template<typename IndexRange, typename Traversal, bool bConst>
		struct index_iterator;
	}

	struct end_sentinel final {};

	namespace index_iterator_impl {

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
			static_assert( tc::is_decayed< IndexRange >::value );

		private:
			friend class boost::iterator_core_access;
			friend struct index_iterator<IndexRange,Traversal,!bConst>;

			conditional_const_t<IndexRange,bConst>* m_pidxrng;
			using index = typename range_traits<IndexRange>::index;
		public: // TODO private
			index m_idx;

		private:
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

			template<
				bool bConstOther,
				std::enable_if_t<
					tc::has_mem_fn_equal_index<conditional_const_t<IndexRange,bConstOther>>::value
				>* = nullptr
			>
			bool equal(index_iterator<IndexRange,Traversal,bConstOther> const& itRhs) const& noexcept {
				return VERIFYEQUAL(VERIFY(m_pidxrng),itRhs.m_pidxrng)->equal_index(m_idx,itRhs.m_idx);
			}

			template<
				bool bConstOther,
				std::enable_if_t<
					!tc::has_mem_fn_equal_index<conditional_const_t<IndexRange,bConstOther>>::value
				>* = nullptr
			>
			bool equal(index_iterator<IndexRange,Traversal,bConstOther> const& itRhs) const& noexcept {
				// Must not be called. Underlying range is probably InputIterator.
				// TODO: Enable SFINAE of equal call in iterator_facade. Then, this can be checked at compile time
				_ASSERTFALSE;
				return true;
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
			auto border_base() const& noexcept {
				return tc::make_iterator( VERIFY(m_pidxrng)->base_range(), m_pidxrng->border_base_index(m_idx));
			}

			template<typename IndexRange_ = IndexRange>
			auto element_base() const& noexcept {
				using It=tc::element_t< tc::decay_t< decltype( tc::make_iterator( VERIFY(m_pidxrng)->base_range(), m_pidxrng->element_base_index(m_idx)) ) > >;
				if(m_pidxrng) {
					return It( tc::make_iterator( VERIFY(m_pidxrng)->base_range(), m_pidxrng->element_base_index(m_idx)) );
				} else {
					return It();
				}
			}

			// sub_range from iterator pair
			template<typename IndexRange_, typename Traversal_, bool bConst_>
			friend tc::make_sub_range_result_t< conditional_const_t<IndexRange_,bConst_> & > make_iterator_range_impl( index_iterator<IndexRange_, Traversal_, bConst_> itBegin, index_iterator<IndexRange_, Traversal_, bConst_> itEnd ) noexcept;

			explicit operator bool() const& noexcept {
				return tc::bool_cast(m_pidxrng);
			}

			friend bool operator==(index_iterator const& it, end_sentinel) noexcept {
				return it.m_pidxrng->at_end_index(it.m_idx);
			}
			friend bool operator==(end_sentinel, index_iterator const& it) noexcept {
				return it.m_pidxrng->at_end_index(it.m_idx);
			}
			friend bool operator!=(index_iterator const& it, end_sentinel) noexcept {
				return !it.m_pidxrng->at_end_index(it.m_idx);
			}
			friend bool operator!=(end_sentinel, index_iterator const& it) noexcept {
				return !it.m_pidxrng->at_end_index(it.m_idx);
			}
		};

		template<typename IndexRange, typename Traversal, bool bConst>
		tc::make_sub_range_result_t< conditional_const_t<IndexRange,bConst> & > make_iterator_range_impl( index_iterator<IndexRange, Traversal, bConst> itBegin, index_iterator<IndexRange, Traversal, bConst> itEnd ) noexcept {
			return tc::make_sub_range_result_t< conditional_const_t<IndexRange,bConst> & >( *VERIFYEQUAL(VERIFY(itBegin.m_pidxrng),itEnd.m_pidxrng), tc_move(itBegin).m_idx, tc_move(itEnd).m_idx );
		}
	}
	using index_iterator_impl::index_iterator;

	template<typename It, std::enable_if_t<tc::is_instance2<index_iterator,std::remove_reference_t<It>>::value>* =nullptr >
	constexpr auto iterator2index(It&& it) noexcept return_decltype_xvalue_by_ref(
		std::forward<It>(it).m_idx
	)

	template<typename It, std::enable_if_t<!tc::is_instance2<index_iterator,std::remove_reference_t<It>>::value>* =nullptr >
	constexpr decltype(auto) iterator2index(It&& it) noexcept {
		return std::forward<It>(it);
	}
}
