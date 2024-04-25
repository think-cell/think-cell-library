
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/return_decltype.h"
#include "../base/explicit_cast.h"
#include "../base/as_lvalue.h"

namespace tc
{
	namespace iterator_facade_detail
	{
		template<typename T>
		struct value_wrapper final {
			template<typename It>
			constexpr value_wrapper(It const& it) noexcept(noexcept(*it))
				: m_value(*it)
			{}

			// This must return a non-const pointer, so that it can be moved from.
			constexpr T* operator->() noexcept { return std::addressof(m_value); }

		private:
			T m_value;
		};

#ifdef _MSC_VER
		// Due to a bug in the MSVC standard library, we need to allow std::ptrdiff_t as distance type as well.
		// Note that this is only the type in the interface of the operators, not the actual one as reported by the traits.
		// https://github.com/microsoft/STL/issues/3663
		template <typename It>
		using difference_type = std::ptrdiff_t;
#else
		template <typename It>
		using difference_type = typename std::iterator_traits<std::decay_t<It>>::difference_type;
#endif
	
		template <typename It>	
		constexpr auto as_difference_type(difference_type<It> value) noexcept {
			using actual_difference_type = typename std::iterator_traits<std::decay_t<It>>::difference_type;
			return tc::explicit_cast<actual_difference_type>(value);
		}

		struct TC_EMPTY_BASES iterator_facade_base{};

		template <typename T>
		concept iterator_facade = std::is_base_of<iterator_facade_base, std::decay_t<T>>::value;
	}

	namespace iterator_facade_adl
	{
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
		//   void iterator::advance(difference_type)
		//   difference_type operator-(iterator const&, iterator const&)
		template<typename Derived>
		struct TC_EMPTY_BASES iterator_facade : iterator_facade_detail::iterator_facade_base {
			template<typename It = Derived>
			constexpr decltype(auto) operator->() const& noexcept(noexcept(*std::declval<It const&>())) {
				auto& self = tc::derived_cast<Derived>(*MSVC_WORKAROUND_THIS);

				using reference = decltype(*std::declval<It const&>());
				if constexpr (std::is_reference<reference>::value) {
					// There are no rvalue pointers; return a pointer even if operator* returns an rvalue reference.
					// std::addressof will not accept an rvalue reference argument
					return std::addressof(tc::as_lvalue(*self));
				} else {
					// If operator* returns a const or volatile object by value, it's not clear whether we should keep that qualifier
					static_assert(std::is_same<reference, std::remove_cv_t<reference>>::value, "Iterator dereferencing should not result in a const or volatile value type");

					// If operator* returns a value type (e.g. for transform_adaptor iterators) then operator-> returns a proxy instead of a pointer
					return iterator_facade_detail::value_wrapper<reference>(self);
				}
			}

			template<typename It = Derived>
			constexpr auto operator[](iterator_facade_detail::difference_type<It> index) const& return_decltype_allow_xvalue_MAYTHROW(
				*(tc::derived_cast<It>(*MSVC_WORKAROUND_THIS) + iterator_facade_detail::as_difference_type<It>(index))
			)
			template<typename It = Derived>
			constexpr auto operator[](iterator_facade_detail::difference_type<It> index) && return_decltype_allow_xvalue_MAYTHROW(
				*(tc::derived_cast<It>(*MSVC_WORKAROUND_THIS) += iterator_facade_detail::as_difference_type<It>(index))
			)
		};

		template<iterator_facade_detail::iterator_facade It>
		constexpr It operator++(It &it, int) noexcept(std::is_nothrow_copy_constructible<It>::value && noexcept(++std::declval<It&>())) {
			auto result = it;
			++it;
			return result;
		}

		template<iterator_facade_detail::iterator_facade It>
			requires requires(It& it) { --it; }
		constexpr It operator--(It& it, int) noexcept(std::is_nothrow_copy_constructible<It>::value && noexcept(--std::declval<It&>())) {
			auto result = it;
			--it;
			return result;
		}

		template<iterator_facade_detail::iterator_facade It>
		constexpr auto operator+=(It& it, iterator_facade_detail::difference_type<It> n) return_decltype_MAYTHROW(
			it.advance(iterator_facade_detail::as_difference_type<It>(n)), it
		)
		template<iterator_facade_detail::iterator_facade It>
		constexpr auto operator-=(It& it, iterator_facade_detail::difference_type<It> n) return_decltype_MAYTHROW(
			it.advance(iterator_facade_detail::as_difference_type<It>(-n)), it
		)

		// clang bug: We cannot use only requires to constrain the operator overload, need at least one enable_if.
		// (Otherwise, it complains about overloading it for non-class types.)
		template<typename It, typename ItDecay = std::decay_t<It>,
				 std::enable_if_t<iterator_facade_detail::iterator_facade<ItDecay>>* = nullptr>
			requires requires (ItDecay& it) { it += 1; }
		constexpr auto operator+(It&& lhs, iterator_facade_detail::difference_type<It> rhs) noexcept(std::is_nothrow_constructible<std::decay_t<It>, It&&>::value && noexcept(std::declval<ItDecay&>() += 1)) {
			auto result = tc_move_if_owned(lhs);
			result += iterator_facade_detail::as_difference_type<It>(rhs);
			return result;
		}
		template<iterator_facade_detail::iterator_facade It>
		constexpr auto operator+(iterator_facade_detail::difference_type<It> lhs, It&& rhs) return_decltype_MAYTHROW(
			tc_move_if_owned(rhs) + lhs
		)
		template<iterator_facade_detail::iterator_facade It>
		constexpr auto operator-(It&& lhs, iterator_facade_detail::difference_type<It> rhs) return_decltype_MAYTHROW(
			tc_move_if_owned(lhs) + iterator_facade_detail::as_difference_type<It>(-rhs)
		)

		template<iterator_facade_detail::iterator_facade It>
		constexpr auto operator<=>(const It& lhs, const It& rhs) return_decltype_noexcept(
			(lhs - rhs) <=> 0
		)
	}
	using iterator_facade_adl::iterator_facade;
}
