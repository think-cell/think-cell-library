
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"

#include "inherit_ctors.h"
#include "type_traits.h"
#include "return_decltype.h"

#include <utility>

#include <boost/fusion/include/pair.hpp>

namespace tc {
	//////////////////////////////////////////////////////////////////////////
	// make_integer_sequence/make_reverse_integer_sequence

	namespace offset_integer_sequence_impl {
		template<typename TIndex, TIndex IdxFrom, TIndex IdxTo, bool bIncreasing>
		struct offset_integer_sequence final {
		private:
			static_assert(IdxFrom <= IdxTo);

			template<TIndex IdxFirst, TIndex... Is>
			static constexpr std::integer_sequence<TIndex, (bIncreasing ? IdxFirst + Is : IdxFirst - Is)...> make(std::integer_sequence<TIndex, Is...>);
		public:
			using type = decltype(make<(bIncreasing ? IdxFrom : IdxTo - 1)>(std::make_integer_sequence<TIndex, IdxTo - IdxFrom>()));
		};
	}

	template<typename TIndex, TIndex IdxFrom, TIndex IdxTo>
	using make_integer_sequence = typename offset_integer_sequence_impl::offset_integer_sequence<TIndex, IdxFrom, IdxTo, /* bIncreasing */ true>::type;

	template<typename TIndex, TIndex IdxFrom, TIndex IdxTo>
	using make_reverse_integer_sequence = typename offset_integer_sequence_impl::offset_integer_sequence<TIndex, IdxFrom, IdxTo, /* bIncreasing */ false>::type;

	//////////////////////////////////////////////////////////////////////////
	// is_contiguous_integer_sequence

	namespace is_contiguous_integer_sequence_impl {
		template<typename TIndex, TIndex IFirst, TIndex... Is>
		constexpr std::is_same<std::integer_sequence<TIndex, IFirst, Is...>, tc::make_integer_sequence<TIndex, IFirst, IFirst + sizeof...(Is) + 1>> is_contiguous_integer_sequence(std::integer_sequence<TIndex, IFirst, Is...>);

		template<typename TIndex>
		constexpr std::true_type is_contiguous_integer_sequence(std::integer_sequence<TIndex>);

		constexpr std::false_type is_contiguous_integer_sequence(...);
	}

	template<typename IntSequence>
	using is_contiguous_integer_sequence = decltype(is_contiguous_integer_sequence_impl::is_contiguous_integer_sequence(std::declval<IntSequence>()));

	//////////////////////////////////////////////////////////////////////////
	// tagged_type

	template<typename TTag, typename T>
	struct tagged_type : boost::fusion::pair<TTag, T> {
		using base_ = typename tagged_type::pair;
		using this_type = tagged_type<TTag, T>;

		tagged_type() = default;
		INHERIT_CTORS_ASSIGN(tagged_type, base_)

		operator T&() & noexcept { return this->second; }
		operator T const&() const& noexcept { return this->second; }
	};

	//////////////////////////////////////////////////////////////////////////
	// next

	template<typename T>
	constexpr auto next(T&& x) noexcept(noexcept(++std::declval<std::decay_t<T&&>&>()) && std::is_nothrow_copy_constructible<T>::value) {
		auto t = std::forward<T>(x);
		++t;
		return t;
	}

	//////////////////////////////////////////////////////////////////////////
	// INTEGRAL_CONSTANT

	// TODO c++17
	// namespace tc {
	//	template<auto v>
	//	using integral_constant = std::integral_constant<std::remove_const_t<decltype(v)>, v>;
	// }
	//
	// USAGE:
	// tc::integral_constant<tc::break_> etc.
	#define INTEGRAL_CONSTANT(...) std::integral_constant<tc::decay_t<decltype(__VA_ARGS__)>, __VA_ARGS__>

	//////////////////////////////////////////////////////////////////////////
	// select_nth

	template<std::size_t n, typename Arg, typename... Args>
	[[nodiscard]] constexpr decltype(auto) select_nth(Arg&& arg, Args&&... args) noexcept {
		if constexpr( 0 == n ) {
			return std::forward<Arg>(arg);
		} else {
			return tc::select_nth<n - 1>(std::forward<Args>(args)...);
		}
	}

	//////////////////////////////////////////////////////////////////////////
	// distance

	namespace distance_impl
	{
		template<typename It, typename Enable = void>
		struct is_subtractible : std::false_type {};

		template<typename It>
		struct is_subtractible<It, std::void_t<decltype(std::declval<It>() - std::declval<It>())>> : std::true_type {};

		template<typename It, bool CanSubtract = is_subtractible<It>::value>
		struct distance_impl {
			static constexpr auto distance(It const& from, It const& to)
				return_decltype_NOEXCEPT(to - from) // std:: iterators might not have noexcept operator-, even if they can't throw
		};

		template<typename It>
		struct distance_impl<It, false> {
			using Difference = typename std::iterator_traits<It>::difference_type;
			static constexpr Difference distance(It from, It const& to) noexcept(noexcept(++from) && noexcept(!(from == to)))
			{
				static_assert(std::is_nothrow_constructible<Difference, int>::value);
				static_assert(noexcept(++std::declval<Difference&>()));
				Difference distance = 0;
				while (!(from == to)) {
					++from; // MAYTHROW
					++distance;
				}
				return distance;
			}
		};
	}

	// TODO C++20: Since std::distance is constexpr in C++20, use std::distance instead of tc::distance, and delete tc::distance.
	template<typename It>
	constexpr auto distance(It const& from, It const& to)
		return_decltype_MAYTHROW(distance_impl::distance_impl<It>::distance(from, to))
}

//////////////////////////////////////////////////////////////////////////
// swap
//
// Must be outside the tc namespace, so that it won't call tc::swap recursively.
// tc::swap could still be called via ADL, if a type is missing an ADL barrier namespace.
namespace tc_swap_impl
{
	using std::swap;

	template<typename T>
	constexpr void swap_impl(T& a, T& b) noexcept(noexcept(swap(a, b))) {
		if constexpr (std::is_trivially_copyable<T>::value && std::is_trivially_copy_assignable<T>::value && std::is_trivially_destructible<T>::value) {
			T temp = a;
			a = b;
			b = temp;
		} else {
			swap(a, b);
		}
	}

	namespace named_swap
	{
		// Note: Using two template arguments here ensures that if this function and std::swap are both visible (via "using namespace") then
		// std::swap is chosen because it is more specialized.
		// T1 and T2 still have to be the same type because swap_impl takes two parameters with the same type.
		template<typename T1, typename T2>
		constexpr void swap(T1& a, T2& b) noexcept(noexcept(swap_impl(a, b))) {
			swap_impl(a, b);
		}
	}
}

namespace tc
{
	// Introduces a tc::swap name in a way that is invisible to ADL.
	using tc_swap_impl::named_swap::swap;


	// TODO C++20 : std::pair and std::tuple cannot be constexpr swapped until C++20
	template <typename First, typename Second>
	constexpr void swap(std::pair<First, Second>& lhs, std::pair<First, Second>& rhs ) noexcept {
		tc::swap(lhs.first, rhs.first);
		tc::swap(lhs.second, rhs.second);
	}

	namespace swap_tuple_impl {
		template <typename Tuple, std::size_t... I>
		constexpr void swap_tuple_impl(Tuple& lhs, Tuple& rhs, std::index_sequence<I...>) noexcept {
			(tc::swap(std::get<I>(lhs), std::get<I>(rhs)), ...);
		}
	}

	template <typename... T>
	constexpr void swap(std::tuple<T...>& lhs, std::tuple<T...>& rhs) noexcept {
		swap_tuple_impl::swap_tuple_impl(lhs, rhs, std::make_index_sequence<sizeof...(T)>());
	}
}

//////////////////////////////////////////////////////////////////////////
// as_constexpr

namespace tc::as_constexpr_no_adl {
	namespace {
		template<typename TInit>
		struct SValueHolder final {
#ifndef __clang__
			static constexpr auto value = TInit::value();
#else
			static constexpr auto value = TInit::value()();
#endif
		};
	}
}

#if defined(__clang__) // https://bugs.llvm.org/show_bug.cgi?id=32766
#define as_constexpr(...) \
	([]() constexpr noexcept -> auto const& { \
		struct SConstexprInit final { \
			static constexpr auto value() noexcept { return []() constexpr noexcept { return __VA_ARGS__; }; } \
		}; \
		return tc::as_constexpr_no_adl::SValueHolder<SConstexprInit>::value; \
	}())
#else
#define as_constexpr(...) \
	([]() constexpr noexcept -> auto const& { \
		struct SConstexprInit final { \
			static constexpr auto value() noexcept { return __VA_ARGS__; } \
		}; \
		return tc::as_constexpr_no_adl::SValueHolder<SConstexprInit>::value; \
	}())
#endif

//////////////////////////////////////////////////////////////////////////
// cmp_equal/cmp_less/cmp_greater...

namespace tc { // TODO c++20: replace these functions with std versions
	template< class T, class U, std::enable_if_t<tc::is_actual_integer<T>::value && tc::is_actual_integer<U>::value>* = nullptr >
	constexpr bool cmp_equal( T t, U u ) noexcept
	{
	    if constexpr (std::is_signed_v<T> == std::is_signed_v<U>)
	        return t == u;
	    else if constexpr (std::is_signed_v<T>)
	        return t < 0 ? false : static_cast<std::make_unsigned_t<T>>(t) == u;
	    else
	        return u < 0 ? false : t == static_cast<std::make_unsigned_t<U>>(u);
	}
	 
	template< class T, class U>
	constexpr auto cmp_not_equal( T t, U u ) return_decltype_noexcept(
		!cmp_equal(t, u)
	)
	 
	template< class T, class U, std::enable_if_t<tc::is_actual_integer<T>::value && tc::is_actual_integer<U>::value>* = nullptr >
	constexpr bool cmp_less( T t, U u ) noexcept
	{
	    if constexpr (std::is_signed_v<T> == std::is_signed_v<U>)
	        return t < u;
	    else if constexpr (std::is_signed_v<T>)
	        return t < 0 ? true : static_cast<std::make_unsigned_t<T>>(t) < u;
	    else
	        return u < 0 ? false : t < static_cast<std::make_unsigned_t<U>>(u);
	}
	 
	template< class T, class U>
	constexpr auto cmp_greater( T t, U u ) return_decltype_noexcept(
	    cmp_less(u, t)
	)
	 
	template< class T, class U >
	constexpr auto cmp_less_equal( T t, U u ) return_decltype_noexcept(
	    !cmp_greater(t, u)
	)
	 
	template< class T, class U >
	constexpr auto cmp_greater_equal( T t, U u ) return_decltype_noexcept(
	    !cmp_less(t, u)
	)
}
