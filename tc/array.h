
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "base/assert_defs.h"
#include "base/const_forward.h"
#include "base/reference_or_value.h"
#include "base/type_traits.h"
#include "base/explicit_cast_fwd.h"
#include "base/tag_type.h"
#include "base/construction_restrictiveness.h"
#include "algorithm/compare.h"
#include "algorithm/equal.h"
#include "algorithm/empty.h"
#include "algorithm/quantifier.h"
#include "storage_for.h"
#include "range/transform.h"

#include <boost/iterator/indirect_iterator.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <cstdint>
#include <array>

namespace tc {
	namespace no_adl {
		template< typename T >
		struct empty_array_storage {
			constexpr operator T*() { return nullptr; }
			constexpr operator T const*() const { return nullptr; }
		};

		template< typename T, std::size_t N >
		struct array_storage {
			using type = T[N];
		};

		template< typename T >
		struct array_storage<T, 0> {
			using type = empty_array_storage<T>;
		};
	}

	namespace array_adl {
		template< typename T, std::size_t N >
		struct array;

		template< typename T, std::size_t N >
		struct array<T&, N> {
			static_assert( !std::is_reference<T>::value );
		private:
			typename no_adl::array_storage<T*, N>::type m_a;

		public:
			using iterator = boost::indirect_iterator<T* const*, tc::decay_t<T>, /*CategoryOrTraversal*/boost::use_default, T&>;

			static constexpr auto size() noexcept {
				return tc::least_uint_constant<N>{};
			}

		private:
			template<typename Func, std::size_t ...IndexPack>
			constexpr array(func_tag_t, Func func, std::index_sequence<IndexPack...>) MAYTHROW 
				: m_a{std::addressof(func(IndexPack))...} 
			{
				static_assert(tc::safely_constructible_from<T&, decltype(func(0))>);
			}
		public:
			template<typename Func>
			array(func_tag_t, Func func) MAYTHROW
				: array(tc::func_tag, func, std::make_index_sequence<N>())
			{}

			// make sure forwarding ctor has at least two parameters, so no ambiguity with copy/move ctors
			template <typename... Args> requires (tc::econstructionIMPLICIT==tc::elementwise_construction_restrictiveness<T&, Args&...>::value)
			constexpr array(tc::aggregate_tag_t, Args& ... args) MAYTHROW
				: m_a{std::addressof(args)...}
			{
				STATICASSERTEQUAL(sizeof...(Args), N, "array initializer list does not match number of elements");
			}

		private:
			DEFINE_NESTED_TAG_TYPE(range_tag)

			template<typename Iterator, std::size_t ...IndexPack>
			constexpr array(range_tag_t, Iterator it, Iterator itEnd, std::index_sequence<IndexPack...>) MAYTHROW
				: m_a{(_ASSERTE(itEnd!=it), std::addressof(*it)), (static_cast<void>(IndexPack), ++it, _ASSERTE(itEnd!=it), std::addressof(*it))...}
			{
				static_assert(tc::safely_constructible_from<T&, decltype(*it)>);
				STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
				_ASSERTE(itEnd==++it);
			}
		public:
			template<typename Rng> requires (0 != N) && (tc::econstructionIMPLICIT==tc::construction_restrictiveness<T&, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value)
			constexpr explicit array(Rng&& rng) MAYTHROW
				: array(range_tag, tc::begin(rng), tc::end(rng), std::make_index_sequence<N-1>())
			{}

#if 0
			template<typename T2> requires tc::safely_assignable_from<T&, T2 const&>
			array const& operator=(array<T2, N> const& rhs) const& noexcept(std::is_nothrow_assignable<T&, T2 const&>::value) {
				VERIFY(boost::copy(VERIFYINITIALIZED(rhs), begin())==end());
				return *this;
			}

			template<typename T2> requires tc::safely_assignable_from<T&, T2&&>
			array const& operator=(array<T2, N>&& rhs) const& noexcept(std::is_nothrow_assignable<T&, T2&&>::value) {
				auto it=tc::begin(rhs);
				auto const itEnd=tc::end(rhs);
				for(auto itOut=begin(); itEnd!=it; ++it, ++itOut) {
					*itOut=tc_move_if_owned(VERIFYINITIALIZED(*it)); // T2 may be lvalue-reference
				}
				return *this;
			}
#endif

			// iterators
			// reference semantics == no deep constness
			iterator begin() const& noexcept {
				return std::addressof(m_a[0]);
			}
			iterator end() const& noexcept {
				return std::addressof(m_a[0])+N;
					// std::addressof(m_a[N]); triggers warning in clang
			}

			// access (no rvalue-qualified overloads, must not move data out of a reference)
			// reference semantics == no deep constness
			[[nodiscard]] constexpr T& operator[](std::size_t const i) const& noexcept {
				_ASSERTDEBUG(i<N);
				return *m_a[i];
			}

#if defined(TC_PRIVATE) && defined(_DEBUG) && !defined(__clang__)
			friend constexpr bool check_initialized_impl(array const& a) noexcept {
				return tc::all_of(a, tc_fn(tc::check_initialized));
			}
#endif
		};

		template<typename Lhs, typename Rhs, std::size_t N>
		[[nodiscard]] constexpr bool operator==(array<Lhs, N> const& lhs, array<Rhs, N> const& rhs) noexcept {
			return tc::equal(VERIFYINITIALIZED(lhs), VERIFYINITIALIZED(rhs));
		}

		template<typename Lhs, std::size_t N, typename Rhs>
		[[nodiscard]] constexpr auto operator<=>(array<Lhs, N> const& lhs, array<Rhs, N> const& rhs) noexcept {
			return tc::lexicographical_compare_3way(VERIFYINITIALIZED(lhs), VERIFYINITIALIZED(rhs));
		}
	} // namespace array_adl
	using array_adl::array;

	/////////////////////////////////////////////////////
	// std::array

	namespace no_adl {
		template<typename T, std::size_t N>
		struct constexpr_size_impl<std::array<T, N>> : tc::least_uint_constant<N> {};
	}

	namespace explicit_convert_std_array_detail {
		template<typename T, std::size_t N, typename Func, std::size_t... IndexPack>
		constexpr std::array<T, N> with_func_tag_impl(tc::type::identity<std::array<T, N>>, Func func, std::index_sequence<IndexPack...>) MAYTHROW {
			return {{func(IndexPack)...}};
		}

		template<typename T, std::size_t N, std::size_t ...IndexPack, typename... Args>
		constexpr std::array<T, N> with_fill_tag_impl(tc::type::identity<std::array<T, N>>, std::index_sequence<IndexPack...>, Args&&... args) MAYTHROW {
			STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
			return { {
				(tc::discard(IndexPack), tc::explicit_cast<T>(tc::const_forward<Args>(args)...))...,
				tc::explicit_cast<T>(tc_move_if_owned(args)...)
			} };
		}

		template<typename T, typename Iterator, typename Dummy>
		constexpr std::array<T, 1> with_range_tag_impl(tc::type::identity<std::array<T, 1>>, Iterator it, Iterator itEnd, Dummy&&) MAYTHROW {
			return std::array<T, 1>{ {(_ASSERTE(itEnd != it), _ASSERTE(itEnd == tc_modified(it, ++_)), *it)} };
		}

		template<typename T, std::size_t N, typename Iterator, std::size_t... IndexPack> requires (1<N)
		constexpr std::array<T, N> with_range_tag_impl(tc::type::identity<std::array<T, N>>, Iterator it, Iterator itEnd, std::index_sequence<IndexPack...>) MAYTHROW {
			STATICASSERTEQUAL(N, sizeof...(IndexPack)+2);
			return std::array<T, N>{ {
				(_ASSERTE(itEnd != it), *it),
				(static_cast<void>(IndexPack), ++it, _ASSERTE(itEnd != it), *it)...,
				(++it, _ASSERTE(itEnd != it), _ASSERTE(itEnd == tc_modified(it, ++_)), *it)
			} };
		}
	}

	namespace explicit_convert_adl {
		template<typename T, std::size_t N, typename Func>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>> id, tc::func_tag_t, Func func) MAYTHROW {
			static_assert(tc::safely_constructible_from<T, decltype(func(N-1))>);
			return tc::explicit_convert_std_array_detail::with_func_tag_impl(id, tc_move(func), std::make_index_sequence<N>());
		}

		template<typename T, std::size_t N, typename... Args> requires (0!=N) && tc::explicit_castable_from<T, Args&&...>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>> id, tc::fill_tag_t, Args&& ... args) MAYTHROW {
			return tc::explicit_convert_std_array_detail::with_fill_tag_impl(id, std::make_index_sequence<N-1>(), tc_move_if_owned(args)...);
		}

		template<typename T, std::size_t N, typename... Args> requires (tc::explicit_castable_from<T, Args&&> && ...)
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>>, tc::aggregate_tag_t, Args&& ... args) MAYTHROW {
			STATICASSERTEQUAL(sizeof...(Args), N, "array initializer list does not match number of elements");
			return {{tc::explicit_cast<T>(tc_move_if_owned(args))...}};
		}

		template<typename T, std::size_t N>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>> id, auto&& rng) MAYTHROW
			requires (0 == N)
				|| (econstructionEXPLICIT <= tc::type::apply_t<tc::elementwise_construction_restrictiveness, tc::type::concat_t<tc::type::list<T>, tc::range_output_t<decltype(rng)>>>::value)
		{
			if constexpr( 0 == N ) {
				_ASSERTE(tc::empty(rng));
				return {};
			} else if constexpr( std::is_trivially_default_constructible<T>::value && std::is_trivially_destructible<T>::value ) {
				std::array<T, N> at;
				auto itOut = tc::begin(at); // MAYTHROW
				// cont_assign(at, transform(tc_move_if_owned(rng), tc_fn(tc::explicit_cast<T>))); without moving rng and avoiding dependency
				tc::for_each(tc_move_if_owned(rng), [&](auto&& t) MAYTHROW {
					tc::renew(*itOut, tc_move_if_owned(t)); // MAYTHROW
					++itOut;
				}); // MAYTHROW
				_ASSERTE(tc::end(at)==itOut);
				return at;
			} else if constexpr( 
				// The initialization of the C array inside std::array when writing std::array<T,1>{{...}} is
				// copy list initialization, not direct list initialization, so explicit constructors are not allowed.
				// int to double is considered narrowing, forbidden in list initialization (but double is already handled above)
				tc::safely_convertible_to<decltype(*tc::as_lvalue(tc::begin(rng))), T>
			) {
				return tc::explicit_convert_std_array_detail::with_range_tag_impl(id, tc::begin(rng), tc::end(rng), std::make_index_sequence<1==N ? 0 : N-2>());
			} else {
				tc_return_cast(tc::transform(tc_move_if_owned(rng), tc::fn_explicit_cast<T>()));
			}
		}
	}

	//////////////////////////////////////////////////////////////

	namespace no_adl {
		template <typename T, typename...>
		struct delayed_deduce final {
			using type = T;
		};

		template <typename... Ts>
		struct delayed_deduce<tc::deduce_tag, Ts...> final {
			using type = tc::common_type_t<Ts...>;
		};
	}

	template<typename TTarget, std::size_t N, typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::explicit_cast<std::array<TTarget, N>>(tc_move_if_owned(rng))
	)

	template<std::size_t N, typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::make_array<tc::range_value_t<Rng>, N>(tc_move_if_owned(rng))
	)

	template<typename TTarget, typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::make_array<TTarget, tc::constexpr_size<Rng>()>(tc_move_if_owned(rng))
	)

	template<typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::make_array<tc::constexpr_size<Rng>()>(tc_move_if_owned(rng))
	)

	template <typename T = tc::deduce_tag, typename... Ts> requires (!std::is_reference<T>::value)
	[[nodiscard]] constexpr auto make_array(tc::aggregate_tag_t, Ts&&... ts) noexcept {
		static_assert(!std::is_reference<typename no_adl::delayed_deduce<T, Ts...>::type>::value);
		return tc::explicit_cast<std::array<typename no_adl::delayed_deduce<T, Ts...>::type, sizeof...(Ts)>>(tc::aggregate_tag, tc_move_if_owned(ts)...);
	}

	// If T is a reference, force argument type T for all given arguments. That way, conversions
	// take place in the calling expression, and cases such as
	//
	//		tc::find_unique<tc::return_bool>(tc::make_array<Foo const&>(convertible_to_Foo), foo);
	//
	// will work as expected. With the usual variadic template + std::forward pattern, conversions
	// would take place inside the array constructor, resulting in a dangling reference.

	// Unfortunately, there seems to be no way to make this work in C++ without using macros
#define TC_MAKE_ARRAY_LVALUE_REF(z, n, d) \
	template <typename T> requires std::is_lvalue_reference<T>::value \
	[[nodiscard]] constexpr auto make_array(tc::aggregate_tag_t, BOOST_PP_ENUM_PARAMS(n, T t)) noexcept { \
		return tc::explicit_cast<tc::array<T, n>>(tc::aggregate_tag, BOOST_PP_ENUM_PARAMS(n, t)); \
	}

	BOOST_PP_REPEAT_FROM_TO(1, 20, TC_MAKE_ARRAY_LVALUE_REF, _)
#undef TC_MAKE_ARRAY_LVALUE_REF

	template<typename T>
	[[nodiscard]] constexpr auto single(T&& t) noexcept {
		if constexpr( std::is_reference<T>::value ) {
			return tc::counted(std::addressof(t),1);
		} else {
			// not tc::decay_t, we want to keep reference-like proxy objects as proxy objects
			// just like we preserve lvalue references.
			return tc::make_array<std::remove_cv_t<T> >(tc::aggregate_tag,tc_move_if_owned(t));
		}
	}
}
