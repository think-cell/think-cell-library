
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "return_decltype.h"
#include "equality_comparable.h"
#include "casts.h"
#include "as_lvalue.h"

namespace tc
{
	namespace no_adl
	{
		template< typename T >
		struct iterator_facade_value_wrapper {
			constexpr iterator_facade_value_wrapper(T&& value)
				noexcept(std::is_nothrow_move_constructible<T>::value)
				: m_value(tc_move(value)) {}

			// This must return a non-const pointer, so that it can be moved from.
			constexpr T* operator->() noexcept { return &m_value; }

		private:
			T m_value;
		};
	}

	namespace iterator_facade_adl
	{
		std::false_type is_iterator_facade_derived_impl(...);

		// This base class implements several iterator operators in terms of other operators.
		//
		// iterator_facade relies on the following members being implemented in the derived class:
		// Always:
		//   using difference_type = ...;
		//   bool operator==(iterator const&, iterator const&)
		//   iterator& operator++()
		//   any_type operator*() const&  // may return a reference or a value
		//
		// For bidirectional or random access traversal:
		//   iterator& operator--()
		//
		// For random access traversal:
		//   iterator& operator+=(iterator&, difference_type)
		//   difference_type operator-(iterator const&, iterator const&)

		template< typename Derived >
		struct iterator_facade : equality_comparable<Derived> {
			template< typename Derived_ = Derived >
			constexpr auto operator[](typename std::iterator_traits<Derived_>::difference_type index) const& return_decltype_MAYTHROW(
				*(derived_cast<Derived_>(*this) + index)
			)
			
			template< typename Derived_ = Derived >
			constexpr decltype(auto) operator->() const& {
				static_assert(std::is_same<Derived_, Derived>::value);
				using reference = decltype(*std::declval<Derived const&>());
				
				if constexpr (std::is_reference<reference>::value) {
					// There are no rvalue pointers; return a pointer even if operator* returns an rvalue reference.
					// std::addressof will not accept an rvalue reference argument
					return std::addressof(tc::as_lvalue(**derived_cast<Derived>(this)));
				} else {
					// If operator* returns a value type (e.g. for transform_adaptor iterators) then operator-> returns a proxy instead of a pointer
					
					// If operator* returns a const or volatile object by value, it's not clear whether we should keep that qualifier
					static_assert(std::is_same<reference, std::remove_cv_t<reference>>::value, "Iterator dereferencing should not result in a const or volatile value type");

					return no_adl::iterator_facade_value_wrapper<reference>(**derived_cast<Derived>(this));
				}
			}

			friend std::true_type is_iterator_facade_derived_impl(iterator_facade const&);
		};

		// The operators are found using ADL. ADL can find these operators even if the operand does not inherit from iterator_facade,
		// so we also check is_iterator_facade_derived using SFINAE.
		template< typename T >
		struct is_iterator_facade_derived : decltype(is_iterator_facade_derived_impl(std::declval<T const&>())) {};

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr >
		constexpr auto operator <(It const& itLhs, It const& itRhs) return_decltype_noexcept(
			(itLhs - itRhs) < 0
		)

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr >
		constexpr auto operator >(It const& itLhs, It const& itRhs) return_decltype_noexcept(
			itRhs < itLhs
		)

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr >
		constexpr auto operator <=(It const& itLhs, It const& itRhs) return_decltype_noexcept(
			!(itRhs < itLhs)
		)

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr >
		constexpr auto operator >=(It const& itLhs, It const& itRhs) return_decltype_noexcept(
			!(itLhs < itRhs)
		)

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr, typename ItDecayed = tc::decay_t<It> >
		constexpr auto operator +(It&& itLhs, typename std::iterator_traits<ItDecayed>::difference_type rhs) noexcept(std::is_nothrow_constructible<ItDecayed, It&&>::value && noexcept(std::declval<ItDecayed&>() += rhs)) {
			ItDecayed itResult = std::forward<It>(itLhs);
			itResult += rhs;
			return itResult;
		}

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr >
		constexpr auto operator +(typename std::iterator_traits<tc::decay_t<It>>::difference_type lhs, It&& itRhs) noexcept(noexcept(std::forward<It>(itRhs) + lhs)) {
			return std::forward<It>(itRhs) + lhs;
		}

		// tc::decay_t should be std::void_t when it works properly in MSVC
		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr, tc::decay_t<decltype(std::declval<It&>() += -std::declval<typename std::iterator_traits<tc::decay_t<It>>::difference_type>())>* = nullptr >
		constexpr auto operator -=(It& itLhs, typename std::iterator_traits<It>::difference_type rhs) noexcept(noexcept(std::declval<It&>() += -rhs)) {
			itLhs += -rhs;
			return itLhs;
		}

		// tc::decay_t should be std::void_t when it works properly in MSVC
		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr, typename ItDecayed = tc::decay_t<It>, tc::decay_t<decltype(std::declval<ItDecayed&>() -= std::declval<typename std::iterator_traits<ItDecayed>::difference_type>())>* = nullptr >
		constexpr auto operator -(It&& itLhs, typename std::iterator_traits<ItDecayed>::difference_type rhs) noexcept(std::is_nothrow_constructible<ItDecayed, It&&>::value && noexcept(std::declval<ItDecayed&>() -= rhs)) {
			ItDecayed itResult = std::forward<It>(itLhs);
			itResult -= rhs;
			return itResult;
		}

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr >
		constexpr tc::decay_t<It> operator ++(It& itLhs, int) noexcept(std::is_nothrow_copy_constructible<It>::value && noexcept(++itLhs)) {
			tc::decay_t<It> result = itLhs;
			++itLhs;
			return result;
		}

		template< typename It, std::enable_if_t<is_iterator_facade_derived<It>::value>* = nullptr >
		constexpr tc::decay_t<It> operator --(It& itLhs, int) noexcept(std::is_nothrow_copy_constructible<It>::value && noexcept(--itLhs)) {
			tc::decay_t<It> result = itLhs;
			--itLhs;
			return result;
		}
	}
	using iterator_facade_adl::iterator_facade;
}