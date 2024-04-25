
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../algorithm/element.h"

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
			static_assert(tc::decayed<T>);

		private:
			using traversal = std::conditional_t<has_decrement<T>,
				std::conditional_t<has_subtract<T>,
					boost::iterators::random_access_traversal_tag,
					boost::iterators::bidirectional_traversal_tag
				>,
				boost::iterators::forward_traversal_tag
			>;

			T value;

		public:

			// Non-random-access iterators might not be subtractable, but they still need a default difference_type, which is unrelated to subtraction.
			using difference_type = typename std::conditional_t<
				has_subtract<T>,
				delayed_iterator_difference_type<T>,
				std::type_identity<std::ptrdiff_t>
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
			constexpr T const&& operator*() const&& noexcept { return tc_move_always_even_const(value); }

			template<typename U>
			friend constexpr bool operator ==(counting_iterator<T> const& a, counting_iterator<U> const& b) return_MAYTHROW(
				*a == *b
			)

			constexpr counting_iterator& operator++() noexcept(noexcept(
				++value
			)) requires requires { ++std::declval<T&>(); } {
				++value;
				return *this;
			}

			constexpr counting_iterator& operator--() noexcept(noexcept(
				--value
			)) requires requires { --std::declval<T&>(); } {
				--value;
				return *this;
			}

			// For iterator_facade.
MODIFY_WARNINGS_BEGIN(((disable)(4244))) // conversion possibly loses data; e.g. if T is an integral type smaller than int (so difference_type is int).
			template < ENABLE_SFINAE, tc::decay_t<decltype(std::declval<SFINAE_TYPE(T&)>() += std::declval<difference_type&>())>* = nullptr >
			constexpr void advance(difference_type n) noexcept(noexcept(
				value += n
				)) {
				value += n;
			}
MODIFY_WARNINGS_END

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

		template<typename T, std::enable_if_t<!std::is_unsigned<T>::value>* = nullptr>
		constexpr auto operator -(counting_iterator<T> const& a, counting_iterator<T> const& b) return_decltype_NOEXCEPT( // NOEXCEPT() for e.g. counting_iterator<tc::iterator_t<tc::vector<T>>>
			tc::as_signed(*a - *b)
		)

		template<typename T, std::enable_if_t<std::is_unsigned<T>::value>* = nullptr>
		constexpr auto operator -(counting_iterator<T> const& a, counting_iterator<T> const& b) return_decltype_noexcept(
			tc::as_signed(*a) - tc::as_signed(*b)
		)
	}

	using counting_iterator_adl::counting_iterator;

	namespace no_adl {
		template <typename T>
		struct is_stashing_element<tc::counting_iterator<T>> : tc::constant<true> {};
	}

	template <typename Rng> requires tc::instance<tc::iterator_t<Rng>, tc::counting_iterator>
	constexpr auto enable_borrowed_range<Rng> = true;

	template<typename TBegin, typename TEnd>
	[[nodiscard]] constexpr auto iota(TBegin const& tBegin, TEnd const& tEnd) noexcept {
		using T = tc::counting_iterator<tc::common_type_t<TBegin, TEnd> >;
		if constexpr (std::convertible_to<typename boost::iterator_traversal<T>::type, boost::iterators::random_access_traversal_tag>) {
			_ASSERTE(!(static_cast<T>(tEnd) < static_cast<T>(tBegin)));
		}
		return tc::make_iterator_range(static_cast<T>(tBegin), static_cast<T>(tEnd));
	}

	namespace no_adl {
		template<typename Rng>
		struct is_iota_range_impl final: tc::constant<false> {};

		template<typename T>
		struct is_iota_range_impl<subrange<universal_range<tc::counting_iterator<T>>>> final: tc::constant<true> {
			STATICASSERTSAME(decltype(tc::iota(std::declval<T>(), std::declval<T>())), subrange<universal_range<tc::counting_iterator<T>>>);
		};
	}

	template<typename Rng>
	using is_iota_range = tc::no_adl::is_iota_range_impl<std::remove_cvref_t<Rng>>;

	namespace no_adl {
		template<typename Rng>
		struct [[nodiscard]] range_of_elements {
			template<typename Rhs>
			constexpr range_of_elements(aggregate_tag_t, Rhs&& rhs) noexcept
				: m_rng(aggregate_tag, tc_move_if_owned(rhs))
			{}

			constexpr auto begin() const&
				return_MAYTHROW(tc::counting_iterator<tc::decay_t<decltype(tc::begin(*m_rng))>>(tc::begin(*m_rng)))
			
			constexpr auto end() const&
				return_MAYTHROW(tc::counting_iterator<tc::decay_t<decltype(tc::end(*m_rng))>>(tc::end(*m_rng)))
			
			constexpr auto begin() &
				return_MAYTHROW(tc::counting_iterator<tc::decay_t<decltype(tc::begin(*m_rng))>>(tc::begin(*m_rng)))

			constexpr auto end() &
				return_MAYTHROW(tc::counting_iterator<tc::decay_t<decltype(tc::end(*m_rng))>>(tc::end(*m_rng)))

		private:
			tc::reference_or_value<Rng> m_rng;
		};
	}
	template<typename Rng>
	constexpr auto make_range_of_iterators(Rng&& rng)
		return_ctor_noexcept( no_adl::range_of_elements< Rng >, (aggregate_tag, tc_move_if_owned(rng)) )

	namespace no_adl {
		template<auto Begin, decltype(Begin) End
#ifdef _MSC_VER // MSVC 19.31 sometimes incorrectly folds iota_range_constant<EnumUnrelated(0), EnumUnrelated(7)> and iota_range_constant<Enum(0), Enum(7)>. I don't have a concise repro for this behavior.
			, typename MsvcWorkaround = decltype(Begin)
#endif
		>
		struct [[nodiscard]] iota_range_constant {
			static constexpr auto begin() noexcept {
				return tc::counting_iterator(Begin);
			}
			static constexpr auto end() noexcept {
				return tc::counting_iterator(End);
			}

			static constexpr auto size() noexcept {
				return tc::least_uint_constant<tc::as_unsigned(End - Begin)>{};
			}
		};

		template<typename Enum>
		struct [[nodiscard]] all_values final : iota_range_constant<tc::contiguous_enum<Enum>::begin(), tc::contiguous_enum<Enum>::end()> {
			static constexpr std::size_t index_of(Enum e) noexcept {
				return e - tc::contiguous_enum<Enum>::begin();
			}
#ifdef _MSC_VER
		private:
			static auto constexpr _natvis_begin = tc::contiguous_enum<Enum>::begin(); // natvis visualizations cannot call functions even if they are constexpr.
			static_assert( _natvis_begin == _natvis_begin ); // Make sure it is ODR-used.
#endif
		};

		template <typename RangeReturn, IF_TC_CHECKS(typename CheckUnique,) typename Enum>
		[[nodiscard]] constexpr decltype(auto) find_first_or_unique_impl(std::type_identity<RangeReturn>, IF_TC_CHECKS(CheckUnique bCheckUnique,) all_values<Enum> rng, Enum e) MAYTHROW {
			if constexpr( RangeReturn::requires_iterator ) {
				return RangeReturn::pack_element(tc::begin(rng) + tc::explicit_cast<decltype(tc::end(rng) - tc::begin(rng))>(rng.index_of(e)), rng);
			} else {
				return RangeReturn::template pack_element<all_values>(e);
			}
		}
	}
	using no_adl::iota_range_constant;
	using no_adl::all_values;

	template <typename Enum>
	constexpr auto enable_borrowed_range<tc::all_values<Enum>> = true;

	template<typename Enum>
	inline auto constexpr all_constants = tc::tuple_transform(
		tc::index_sequence_as_tuple(std::make_index_sequence<tc::size(tc::all_values<Enum>())>()),
		[](auto const constn) constexpr {
			return tc::constant<static_cast<Enum>(tc_at_nodebug(tc::all_values<Enum>(), constn()))>();
		}
	);

	template<typename Derived, typename Counter>
	using iota_range_adaptor = tc::index_range_adaptor<Derived, tc::no_adl::universal_range</*It*/tc::counting_iterator<Counter>>, tc::index_range_adaptor_flags::inherit_traversal>;
}
