
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "../base/assert_defs.h"
#include "index_range.h"
#include "iterator_facade.h"
#include "../optional.h"

#include <boost/iterator/detail/facade_iterator_category.hpp>

namespace tc {
	namespace no_adl {
		template<typename It>
		struct nullable_iterator {
			static_assert(tc::decayed<It>);
			template<typename Derived>
			struct base : It {
				using difference_type = typename std::iterator_traits<It>::difference_type;
				using value_type = typename std::iterator_traits<It>::value_type;
				using pointer = typename std::iterator_traits<It>::pointer;
				using reference = typename std::iterator_traits<It>::reference;
				using iterator_category = typename std::iterator_traits<It>::iterator_category;

			private:
				constexpr Derived& derived() noexcept {return tc::derived_cast<Derived>(*MSVC_WORKAROUND_THIS);}
			public:
				constexpr base() noexcept : m_bValid(false) {}

				// no implicit conversion from It; we must ensure that It has the semantics of element rather than border before allowing the construction
				constexpr explicit base(It const& it) noexcept : It(it), m_bValid(true) {}
				constexpr explicit base(It && it) noexcept : It(tc_move(it)), m_bValid(true) {}

				constexpr Derived& operator=(It const& it) & noexcept {
					It::operator=(it);
					m_bValid = true;
					return derived();
				}
				constexpr Derived& operator=(It && it) & noexcept {
					It::operator=(tc_move(it));
					m_bValid = true;
					return derived();
				}

				constexpr explicit operator bool() const& noexcept {
					return m_bValid;
				}

				constexpr Derived& operator++() & noexcept {
					_ASSERT(m_bValid);
					++tc::base_cast<It>(*this);
					return derived();
				}

				constexpr Derived& operator--() & noexcept {
					_ASSERT(m_bValid);
					--tc::base_cast<It>(*this);
					return derived();
				}

				constexpr decltype(auto) operator*() const& noexcept {
					_ASSERT(m_bValid);
					return *tc::base_cast<It>(*this);
				}
				constexpr decltype(auto) operator*() const&& noexcept {
					_ASSERT(m_bValid);
					return *tc::base_cast<It>(tc_move_always_even_const(*this));
				}
				constexpr decltype(auto) operator*() && noexcept {
					_ASSERT(m_bValid);
					return *tc::base_cast<It>(tc_move_always(*this));
				}

				constexpr pointer operator->() const& noexcept {
					_ASSERT(m_bValid);
					return tc::base_cast<It>(*this).operator->();
				}

			protected:
				bool m_bValid;
			};

			struct element_type final : base<element_type> {
				using base_ = base<element_type>;
				using base_::base_;
				using base_::operator=;

				constexpr auto element_base() const& noexcept requires requires(It const& it) { it.element_base(); } {
					using ItBase = typename nullable_iterator<decltype(tc::base_cast<It>(*this).element_base())>::element_type;
					return this->m_bValid ? ItBase{ tc::base_cast<It>(*this).element_base() } : ItBase{};
				}
			};

			struct border_type final : base<border_type> {
				using base_ = base<border_type>;
				using base_::base_;
				using base_::operator=;

				constexpr auto border_base() const& noexcept requires requires(It const& it) { it.border_base(); } {
					using ItBase = typename nullable_iterator<decltype(tc::base_cast<It>(*this).border_base())>::border_type;
					return this->m_bValid ? ItBase{ tc::base_cast<It>(*this).border_base() } : ItBase{};
				}
			};
		};

		template<typename It> requires tc::explicit_castable_from<bool, It>
		struct nullable_iterator<It> {
			static_assert(tc::decayed<It>);
			using element_type = It;
			using border_type = It;
		};
	}

	template< typename It >
	using element_t = typename no_adl::nullable_iterator<It>::element_type;
	
	template< typename It >
	using border_t = typename no_adl::nullable_iterator<It>::border_type;

	template< typename It >
	constexpr auto make_element(It&& it) noexcept {
		return element_t<tc::decay_t<It>>(tc_move_if_owned(it));
	}

	// Regarding constness:
	// const_iterators point to const ranges; non-const iterators point to non-const ranges.
	// The index type, traversal, and difference_type must be the same regardless of whether the range is const.
	// The reference type, and pointer type may be different.
	// The value_type should be the same but there is nothing preventing it from being different.
		
	// Other than dereference_index, base_range and creating subranges, all range functions are called on const references.

	template< typename T, bool bConst >
	using conditional_const_t=std::conditional_t< bConst, T const, T >;

	namespace index_iterator_impl {
		template<typename IndexRange, bool bConst>
		struct index_iterator;
	}
	using index_iterator_impl::index_iterator;

	namespace index_iterator_traits_no_adl {
		template< typename IndexRange>
		struct difference_type_base {
			using difference_type = std::ptrdiff_t; // needed to compile interfaces relying on difference_type
		};

		template< has_mem_fn_distance_to_index IndexRange>
		struct difference_type_base<IndexRange> {
			using difference_type = decltype(std::declval<IndexRange const&>().distance_to_index(std::declval<typename IndexRange::tc_index const&>(), std::declval<typename IndexRange::tc_index const&>()));
		};

		template< typename IndexRange>
		struct value_type_base {
			using value_type = void;
		};

		template< typename IndexRange > requires requires { typename tc::range_value_t<IndexRange>; }
		struct value_type_base<IndexRange> {
			using value_type = tc::range_value_t<IndexRange>;
		};
	}
}

namespace std {
	template<typename IndexRange, bool bConst>
	struct iterator_traits<tc::index_iterator<IndexRange, bConst>>
		: tc::index_iterator_traits_no_adl::difference_type_base<IndexRange> // should not be different for const and non-const
		, tc::index_iterator_traits_no_adl::value_type_base<tc::conditional_const_t<IndexRange, bConst>&>
	{
		static_assert(tc::has_index<IndexRange>);

		// IndexRange::dereference_index does not take an argument of type Index. Did you write tc::transform(Rng, Fn) and Fn takes the wrong argument?
		using reference = std::iter_reference_t<tc::index_iterator<IndexRange, bConst>>;

		// Since C++20, iterator_traits<It>::pointer is synthesized in this way by default, if typename It::pointer does not exist.
		using pointer = decltype(std::declval<tc::index_iterator<IndexRange, bConst>&>().operator->());

		// Some of our ranges, may not even conform to LecayInputIterator concept.
		// We ignore the requirements on value_type and reference, because library implementors do not rely on them anyway.
		static auto compute_iterator_concept() {
			if constexpr (!tc::has_mem_fn_decrement_index<IndexRange>) {
				return std::forward_iterator_tag();
			} else if constexpr (!tc::has_mem_fn_distance_to_index<IndexRange>) {
				return std::bidirectional_iterator_tag();
			} else if constexpr (!tc::has_mem_fn_index_to_address<IndexRange>) {
				return std::random_access_iterator_tag();
			} else {
				return std::contiguous_iterator_tag();
			}
		}
		using iterator_concept = decltype(compute_iterator_concept());

		// Note that iterator_category must not be std::contiguous_iterator_tag, as code may not expect it.
		using iterator_category = std::conditional_t<std::is_same_v<iterator_concept, std::contiguous_iterator_tag>, std::random_access_iterator_tag, iterator_concept>;
	};

	template<typename IndexRange, bool bConst>
		requires std::same_as<typename iterator_traits<tc::index_iterator<IndexRange, bConst>>::iterator_concept, std::contiguous_iterator_tag>
	struct pointer_traits<tc::index_iterator<IndexRange, bConst>>
	{
		using pointer = tc::index_iterator<IndexRange, bConst>;

		// Note that it is *not* the value_type! We want to preserve a const, while the value type strips it.
		using element_type = std::remove_reference_t<typename iterator_traits<pointer>::reference>;

		using difference_type = typename iterator_traits<pointer>::difference_type;

		// Note that we cannot provide rebind or pointer_to, but don't need that for our use cases either.

		static constexpr element_type* to_address(pointer const& ptr) noexcept
		{
			return ptr.get_range().index_to_address(ptr.get_index());
		}
	};	
}

namespace tc {
	namespace no_adl {
		struct end_sentinel final {};
	}
	using no_adl::end_sentinel;

	namespace index_iterator_impl {
		template<typename IndexRange, bool bConst>
		struct index_iterator : tc::iterator_facade<index_iterator<IndexRange, bConst>>
		{
			static_assert(tc::decayed<IndexRange>);

			static constexpr bool c_bHasStashingElement = IndexRange::c_bHasStashingIndex;

		private:
			friend class boost::iterator_core_access;
			friend struct index_iterator<IndexRange,!bConst>;

			tc::optional_reference_or_value<tc::conditional_const_t<IndexRange, bConst>&> m_oidxrng;

		public: // TODO private
			using tc_index = index_t<IndexRange>;
			tc_index m_idx;

		private:
			using this_type = index_iterator<IndexRange, bConst>;

		public:
			constexpr index_iterator() noexcept
				: m_oidxrng()
				, m_idx()
			{}

			template<bool bConstOther> requires bConst || (!bConstOther)
			constexpr index_iterator(index_iterator<IndexRange,bConstOther> const& other) noexcept
				: m_oidxrng(other.m_oidxrng)
				, m_idx(other.m_idx)
			{}

			constexpr index_iterator(conditional_const_t<IndexRange, bConst>& idxrng, tc_index idx) noexcept
				: m_oidxrng(tc::make_optional_reference_or_value(idxrng))
				, m_idx(tc_move(idx))
			{}

			constexpr decltype(auto) get_range() const& noexcept {
				return *m_oidxrng;
			}
			constexpr tc_index const& get_index() const& noexcept {
				return m_idx;
			}
			constexpr tc_index&& get_index() && noexcept {
				return tc_move(m_idx);
			}

			friend index_iterator middle_point( index_iterator const& itBegin, index_iterator const& itEnd ) noexcept requires tc::has_mem_fn_middle_point<IndexRange> {
				index_iterator it=itBegin;
				_ASSERTE(itBegin.m_oidxrng);
				if constexpr (!tc::empty_type<IndexRange>) _ASSERTE(std::addressof(*itBegin.m_oidxrng) == std::addressof(*itEnd.m_oidxrng));
				tc::as_const(*itBegin.m_oidxrng).middle_point(it.m_idx,itEnd.m_idx);
				return it;
			}

			constexpr auto border_base() const& noexcept requires
				tc::has_mem_fn_base_range<IndexRange> && tc::has_mem_fn_border_base_index<IndexRange>
			{
				return tc::make_iterator( m_oidxrng->base_range(), tc::as_const(*m_oidxrng).border_base_index(m_idx));
			}

			constexpr auto element_base() const& noexcept requires
				tc::has_mem_fn_base_range<IndexRange> && tc::has_mem_fn_element_base_index<IndexRange>
			{
				using It = tc::element_t<tc::decay_t<decltype(tc::make_iterator(m_oidxrng->base_range(), tc::as_const(*m_oidxrng).element_base_index(m_idx)))>>;
				if(m_oidxrng) {
					return It(tc::make_iterator(m_oidxrng->base_range(), tc::as_const(*m_oidxrng).element_base_index(m_idx)));
				} else {
					return It();
				}
			}

			constexpr decltype(auto) operator*() const& return_MAYTHROW(
				m_oidxrng->dereference_index(m_idx)
			)
			constexpr decltype(auto) operator*() const&& return_MAYTHROW(
				(*tc_move_always_even_const(m_oidxrng)).dereference_index(tc_move_always_even_const(m_idx))
			)
			constexpr decltype(auto) operator*() && return_MAYTHROW(
				(*tc_move_always(m_oidxrng)).dereference_index(tc_move_always(m_idx))
			)

			constexpr this_type& operator++() noexcept(noexcept(m_oidxrng->increment_index(m_idx))) {
				tc::as_const(*m_oidxrng).increment_index(m_idx);
				return *this;
			}

			constexpr this_type& operator--() noexcept(noexcept(m_oidxrng->decrement_index(m_idx))) requires tc::has_mem_fn_decrement_index<IndexRange> {
				tc::as_const(*m_oidxrng).decrement_index(m_idx);
				return *this;
			}

			explicit constexpr operator bool() const& noexcept {
				tc_return_cast(m_oidxrng);
			}

			constexpr bool operator==(end_sentinel) const& noexcept {
				return tc::as_const(*m_oidxrng).at_end_index(m_idx);
			}
			template <bool OtherConst> requires tc::is_equality_comparable<tc_index>::value
			constexpr bool operator==(index_iterator<IndexRange, OtherConst> const& rhs) const& noexcept {
				if constexpr (!tc::empty_type<IndexRange>) _ASSERTE(std::addressof(*m_oidxrng) == std::addressof(*rhs.m_oidxrng));
				_ASSERTDEBUG( m_oidxrng || m_idx == rhs.m_idx );
				return m_idx == rhs.m_idx;
			}

			// For iterator_facade.
			template<typename N> requires tc::has_mem_fn_advance_index<IndexRange>
			constexpr void advance(N&& n) noexcept(noexcept(m_oidxrng->advance_index(m_idx, n))) {
				tc::as_const(*m_oidxrng).advance_index(m_idx, tc_move_if_owned(n));
			}
		};

		template< tc::has_mem_fn_distance_to_index IndexRange_, bool bConst1, bool bConst2>
		constexpr auto operator -( index_iterator<IndexRange_, bConst1> const& itLhs, index_iterator<IndexRange_, bConst2> const& itRhs) noexcept {
			if constexpr (!tc::empty_type<IndexRange_>) _ASSERTE(std::addressof(itLhs.get_range()) == std::addressof(itRhs.get_range()));
			return tc::as_const(itLhs.get_range()).distance_to_index(itRhs.get_index(), itLhs.get_index());
		}
	}
	using index_iterator_impl::index_iterator;

	template <typename It>
	constexpr auto is_index_iterator = false;
	template<typename IndexRange, bool bConst>
	constexpr auto is_index_iterator<index_iterator<IndexRange, bConst>> = true;

	template <typename Rng> requires tc::empty_type<Rng> && is_index_iterator<tc::iterator_t<Rng>>
	constexpr auto enable_borrowed_range<Rng> = true;

	namespace iterator2index_detail {
		template<typename It, typename Rng>
		concept iterator_of = tc::decayed_derived_from<It, tc::iterator_t<std::remove_cvref_t<Rng>>>
			|| tc::decayed_derived_from<It, tc::iterator_t<std::remove_cvref_t<Rng> const>>;
	}

	template<typename Rng, typename It>
	constexpr decltype(auto) iterator2index(It&& it) noexcept {
		if constexpr(tc::has_index<std::remove_reference_t<Rng>>) {
			return std::remove_reference_t<Rng>::iterator2index(tc_move_if_owned(it));
		} else {
			static_assert(iterator2index_detail::iterator_of<It, Rng>);
			return tc_move_if_owned(it);
		}
	}
}
