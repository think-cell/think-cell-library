
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "subrange.h"
#include "return_decltype.h"

#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wshorten-64-to-32"
#else
#pragma warning( push )
#pragma warning( disable: 4244 )
#endif
#include <boost/iterator/counting_iterator.hpp>
#ifdef __clang__
#pragma clang diagnostic pop
#else
#pragma warning( pop )
#endif

#include <boost/mpl/identity.hpp>

// By default, boost uses long long as counting_iterator<int>::difference_type,
// which generates C4244 level 1 compiler warnings when cast back to int.
// Most pragmatically, difference_type should reflect the type of a-b, thus counting_iterator<int>::difference_type is int,
// and counting_iterator<unsigned short (or anything else shorter than int)>::difference_type is int.

namespace tc {

	namespace counting_iterator_adl {
		
		template<typename T>
		struct delayed_iterator_difference_type {
			using type = std::make_signed_t<decltype(std::declval<T>() - std::declval<T>())>;
		};

		TC_HAS_EXPR(decrement, (T), --std::declval<T&>());
		TC_HAS_EXPR(subtract, (T), std::declval<T const&>() - std::declval<T const&>());

		template<typename T>
		struct counting_iterator
			: iterator_facade<counting_iterator<T>>
		{
			static_assert(tc::is_decayed<T>::value);

		private:
			using traversal = std::conditional_t<has_decrement<T>::value,
				std::conditional_t<has_subtract<T>::value,
					boost::iterators::random_access_traversal_tag,
					boost::iterators::bidirectional_traversal_tag
				>,
				boost::iterators::forward_traversal_tag
			>;

			T value;

		public:

			// Non-random-access iterators might not be subtractable, but they still need a default difference_type, which is unrelated to subtraction.
			using difference_type = typename std::conditional_t<
				has_subtract<T>::value,
				delayed_iterator_difference_type<T>,
				boost::mpl::identity<std::ptrdiff_t>
			>::type;

			using value_type = T;
			using pointer = T const*;
			using reference = T const&;
			using iterator_category = typename boost::iterators::detail::iterator_facade_default_category<traversal, value_type, reference>::type;

			constexpr counting_iterator(T value)
				noexcept(std::is_nothrow_move_constructible<T>::value)
				: value(tc_move(value)) {}

			constexpr counting_iterator() = default;

			constexpr T const* operator->() const& noexcept { return std::addressof(value); }
			constexpr T const* operator->() const&& = delete; // && not required yet. Could return a tc::no_adl::iterator_facade_value_wrapper

			constexpr T && operator*() && noexcept { return tc_move(value); }
			constexpr T const& operator*() const& noexcept { return value; }
			constexpr T const&& operator*() const&& noexcept { return value; }

			template<typename U>
			friend constexpr bool operator ==(counting_iterator<T> const& a, counting_iterator<U> const& b) return_MAYTHROW(
				*a == *b
			)

			template < ENABLE_SFINAE, tc::decay_t<decltype(++std::declval<SFINAE_TYPE(T&)>())>* = nullptr >
			constexpr counting_iterator& operator++() noexcept(noexcept(
				++value
			)) {
				++value;
				return *this;
			}

			template < ENABLE_SFINAE, tc::decay_t<decltype(--std::declval<SFINAE_TYPE(T&)>())>* = nullptr >
			constexpr counting_iterator& operator--() noexcept(noexcept(
				--value
			)) {
				--value;
				return *this;
			}

#pragma warning(push)
#pragma warning(disable:4244) // conversion possibly loses data; e.g. if T is an integral type smaller than int (so difference_type is int).
			template < ENABLE_SFINAE, typename T_ = T, tc::decay_t<decltype(std::declval<SFINAE_TYPE(T&)>() += std::declval<difference_type &>())>* = nullptr >
			friend constexpr counting_iterator& operator +=(SFINAE_TYPE(counting_iterator&) it, difference_type n) noexcept(noexcept(
				it.value += n
			)) {
				it.value += n;
				return it;
			}
#pragma warning(pop)

			// iterator_facade's operator[] will not work for counting_iterator - it tries to return a dangling rvalue reference. This function returns by value.
			template < ENABLE_SFINAE, tc::decay_t<decltype(std::declval<SFINAE_TYPE(counting_iterator &)>() += std::declval<difference_type &>())>* = nullptr >
			constexpr auto operator[](SFINAE_TYPE(difference_type) index) const noexcept(
				noexcept(std::declval<counting_iterator&>() += index)
				&& std::is_nothrow_copy_constructible<counting_iterator>::value
				&& std::is_nothrow_copy_constructible<T>::value
			) {
				auto it = *this;
				it += index;
				return *it;
			}
		};

		template<typename T>
		constexpr auto operator -(counting_iterator<T> const& a, counting_iterator<T> const& b) return_decltype_NOEXCEPT( // NOEXCEPT() for e.g. counting_iterator<std::vector<T>::iterator>
			tc::signed_cast(*a - *b)
		)
	}

	using counting_iterator_adl::counting_iterator;

	template<typename TBegin, typename TEnd>
	[[nodiscard]] constexpr auto iota(TBegin const& tBegin, TEnd const& tEnd) noexcept {
		using T = tc::counting_iterator<tc::common_type_t<TBegin, TEnd> >;
		if constexpr (std::is_convertible<typename boost::iterator_traversal<T>::type, boost::iterators::random_access_traversal_tag>::value) {
			_ASSERTE(!(static_cast<T>(tEnd) < static_cast<T>(tBegin)));
		}
		return tc::make_iterator_range(static_cast<T>(tBegin), static_cast<T>(tEnd));
	}

	namespace no_adl {
		template<typename Rng>
		struct [[nodiscard]] range_of_elements {
			using const_iterator=tc::counting_iterator< decltype(tc::begin(*std::declval<tc::reference_or_value<Rng> const&>())) >;
			using iterator=tc::counting_iterator< decltype(tc::begin(*std::declval<tc::reference_or_value<Rng>&>())) >;

			template<typename Rhs>
			constexpr range_of_elements(aggregate_tag_t, Rhs&& rhs) noexcept
				: m_rng(aggregate_tag, std::forward<Rhs>(rhs))
			{}

			constexpr auto begin() const&
				return_ctor_MAYTHROW(const_iterator, (tc::begin(*m_rng)))
			
			constexpr auto end() const&
				return_ctor_MAYTHROW(const_iterator, (tc::end(*m_rng)))
			
			constexpr auto begin() &
				return_ctor_MAYTHROW(iterator, (tc::begin(*m_rng)))

			constexpr auto end() &
				return_ctor_MAYTHROW(iterator, (tc::end(*m_rng)))

		private:
			tc::reference_or_value<Rng> m_rng;
		};
	}
	template<typename Rng>
	constexpr auto make_range_of_iterators(Rng&& rng)
		return_ctor_noexcept( no_adl::range_of_elements< Rng >, (aggregate_tag, std::forward<Rng>(rng)) )
}