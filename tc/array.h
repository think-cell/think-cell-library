
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "assert_defs.h"
#include "const_forward.h"
#include "compare.h"
#include "implements_compare.h"
#include "storage_for.h"
#include "reference_or_value.h"
#include "type_traits.h"
#include "explicit_cast.h"
#include "tag_type.h"
#include "transform.h"
#include "equal.h"
#include "cont_assign.h"
#include "construction_restrictiveness.h"

#include <boost/iterator/indirect_iterator.hpp>
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
		struct array : tc::implements_compare_partial<array<T, N>> {
			static_assert(!std::is_const<T>::value);
			static_assert(!std::is_volatile<T>::value);
		private:
			typename no_adl::array_storage<T, N>::type m_a;

		public:
			using value_type = T;
			using reference = value_type &;
			using const_reference = value_type const&;
			using pointer = value_type *;
			using const_pointer = value_type const*;
			using iterator = pointer;
			using const_iterator = const_pointer;
			using reverse_iterator = std::reverse_iterator<iterator>;
			using const_reverse_iterator = std::reverse_iterator<const_iterator>;
			using size_type = std::size_t;
			using difference_type = std::ptrdiff_t;

			constexpr T* data() & noexcept {
				return m_a;
			}
			constexpr T const* data() const& noexcept {
				return m_a;
			}

			operator decltype(m_a)& () & noexcept {
				static_assert(0 < N);
				return m_a;
			}

			operator decltype(m_a) const& () const& noexcept {
				static_assert(0 < N);
				return m_a;
			}

			// We cannot tell if *this is constructed using value-initialization syntax or default-initialization syntax. Therefore, we must value-initialize here.
			template<ENABLE_SFINAE,
				std::enable_if_t<std::is_default_constructible<SFINAE_TYPE(T)>::value>* =nullptr
			>
			constexpr array() noexcept(std::is_nothrow_constructible<T>::value) : m_a{} {}

			template<ENABLE_SFINAE,
				// std::is_default_constructible checks for value-initialization, instead of default-initialization. However, T cannot be const, so there should
				// be no observable difference
				std::enable_if_t<std::is_default_constructible<SFINAE_TYPE(T)>::value>* =nullptr
			>
			constexpr explicit array(boost::container::default_init_t) noexcept(std::is_nothrow_constructible<T>::value) {}

		private:
			template<typename Func, std::size_t ...IndexPack>
			constexpr array(func_tag_t, Func func, std::index_sequence<IndexPack...>) MAYTHROW
				: m_a{func(IndexPack)...}
			{}
		public:
			template<typename Func>
			constexpr array(func_tag_t, Func func) MAYTHROW
				: array(tc::func_tag, func, std::make_index_sequence<N>())
			{}

		private:
			template <std::size_t, typename... Args>
			static constexpr T explicit_cast_index(Args&&... args) noexcept {
				return tc::explicit_cast<T>( std::forward<Args>(args)... );
			}

			template<
				typename... Args,
				std::size_t ...IndexPack
				// Visual Studio compiler bug: https://developercommunity.visualstudio.com/t/guaranteed-copy-elision-fails-initializi/1404252
#if defined(_MSC_FULL_VER) && 191326128<=_MSC_FULL_VER // Copy-elision is required to initialize the array from function return value
				, ENABLE_SFINAE,
				std::enable_if_t<
					std::is_move_constructible<SFINAE_TYPE(T)>::value
				>* = nullptr
#endif
			>
			constexpr explicit array(fill_tag_t, std::index_sequence<IndexPack...>, Args&&... args) MAYTHROW
				: m_a{
					explicit_cast_index<IndexPack>(tc::const_forward<Args>(args)...)...,
					tc::explicit_cast<T>(std::forward<Args>(args)...)
				}
			{
				STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
			}
		public:
			template<
				typename... Args
#if defined(_MSC_FULL_VER) && 191326128<=_MSC_FULL_VER
				, ENABLE_SFINAE,
				std::enable_if_t<
					std::is_move_constructible<SFINAE_TYPE(T)>::value
				>* = nullptr
#endif
			>
			constexpr explicit array(fill_tag_t, Args&&... args) MAYTHROW
				: array(fill_tag, std::make_index_sequence<N-1>(), std::forward<Args>(args)...)
			{}
#if defined(_MSC_FULL_VER) && 191326128<=_MSC_FULL_VER
		private:
			template <std::size_t, typename Arg>
			static constexpr Arg&& identity_index(Arg&& args) noexcept {
				return std::forward<Arg>(args);
			}

			template<
				typename Arg,
				typename... Args,
				std::size_t ...IndexPack,
				ENABLE_SFINAE,
				std::enable_if_t<
					!std::is_move_constructible<SFINAE_TYPE(T)>::value
				>* = nullptr
			>
			constexpr explicit array(fill_tag_t, std::index_sequence<IndexPack...>, Arg&& arg, Args&&... args) MAYTHROW
				: m_a{
					T(identity_index<IndexPack>(tc::const_forward<Arg>(arg)), const_forward<Args>(args)...)...,
					T(std::forward<Arg>(arg), std::forward<Args>(args)...)
				}
			{
				STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
			}
		public:
			template<
				typename... Params,
				std::enable_if_t<
					0 < sizeof...(Params)
					&& !std::is_move_constructible<T>::value
				>* = nullptr
			>
			constexpr explicit array(fill_tag_t, Params&&... params) MAYTHROW
				: array(fill_tag, std::make_index_sequence<N-1>(), std::forward<Params>(params)...)
			{}
#endif

			template <typename... Args,
				std::enable_if_t<
					0 < sizeof...(Args) &&
					tc::econstructionIMPLICIT==tc::elementwise_construction_restrictiveness<T, Args...>::value
				>* = nullptr
			>
			constexpr array(tc::aggregate_tag_t, Args&& ... args) noexcept(std::conjunction<std::is_nothrow_constructible<T, Args&&>...>::value)
				: m_a{static_cast<T>(std::forward<Args>(args))...}
			{
				STATICASSERTEQUAL(sizeof...(Args), N, "array initializer list does not match number of elements");
			}

			template <typename... Args,
				std::enable_if_t<
					0 == sizeof...(Args) ||
					tc::econstructionEXPLICIT==tc::elementwise_construction_restrictiveness<T, Args...>::value
				>* = nullptr
			>
			constexpr explicit array(tc::aggregate_tag_t, Args&& ... args) MAYTHROW
				: m_a{tc::explicit_cast<T>(std::forward<Args>(args))...}
			{
				STATICASSERTEQUAL(sizeof...(Args), N, "array initializer list does not match number of elements");
			}

		private:
			template<typename Iterator, std::size_t ...IndexPack>
			constexpr array(range_tag_t, Iterator it, Iterator itEnd, std::index_sequence<IndexPack...>) MAYTHROW
				: m_a{static_cast<T>((_ASSERTE(itEnd!=it), *it)), static_cast<T>((static_cast<void>(IndexPack), ++it, _ASSERTE(itEnd!=it), *it))...} // static_cast because int to double considered narrowing, forbidden in list initialization
			{
				STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
				_ASSERTE(itEnd==++it);
			}
		public:
			template< typename Rng,
				std::enable_if_t< 0!=N
					&& tc::econstructionIMPLICIT==tc::construction_restrictiveness<T, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value
				>* = nullptr
			>
			constexpr explicit array(Rng&& rng) MAYTHROW
				: array(range_tag, tc::begin(rng), tc::end(rng), std::make_index_sequence<N-1>())
			{}

			template< typename Rng,
				std::enable_if_t< 0 == N
					&& tc::econstructionIMPLICIT == tc::construction_restrictiveness<T, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value
				>* = nullptr
			>
			constexpr explicit array(Rng&& rng) MAYTHROW
				: m_a{}
			{
				_ASSERTE(tc::empty(rng));
			}

			template< typename Rng,
				std::enable_if_t<
					tc::econstructionEXPLICIT==tc::construction_restrictiveness<T, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value
				>* = nullptr 
			>
			explicit array(Rng&& rng) MAYTHROW
				: array(tc::transform( std::forward<Rng>(rng), tc::fn_explicit_cast<T>()))
			{}
			
			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2 const&>::value>* =nullptr
			>
			array& operator=(array<T2, N> const& rhs) & MAYTHROW {
				for (std::size_t i = 0; i<N; ++i) {
					*(data() + i)=VERIFYINITIALIZED(tc_at_nodebug(rhs, i));
				}
				return *this;
			}

			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2&&>::value>* =nullptr
			>
			array& operator=(array<T2, N>&& rhs) & MAYTHROW {
				for (std::size_t i = 0; i<N; ++i) {
					// Use tc_move(rhs)[i] instead of tc_move(rhs[i]) here so we do not
					// need to specialize for the case that rhs is an array of reference.
					// Note that it is safe to call rhs::operator[]()&& on different indices.
					*(data() + i)=VERIFYINITIALIZED(tc_move_always(tc_at_nodebug(rhs, i)));
				}
				return *this;
			}

			// iterators
			constexpr const_iterator begin() const& noexcept {
				return data();
			}
			constexpr const_iterator end() const& noexcept {
				return data() + N;
			}
			constexpr iterator begin() & noexcept {
				return data();
			}
			constexpr iterator end() & noexcept {
				return data() + N;
			}

#ifdef _DEBUG
			friend void uninitialize_impl(array& a) noexcept {
				UNINITIALIZED(a.m_a);
			}
#endif
		};

		template< typename T, std::size_t N >
		struct array<T&, N> : tc::implements_compare_partial<array<T&, N>> {
			static_assert( !std::is_reference<T>::value );
		private:
			typename no_adl::array_storage<T*, N>::type m_a;

		public:
			using value_type = tc::decay_t<T>;
			using reference = T&;
			using const_reference = T&; // reference semantics == no deep constness
			using pointer = T *;
			using iterator = boost::indirect_iterator<T* const*, value_type, /*CategoryOrTraversal*/boost::use_default, reference>;
			using const_iterator = iterator; // reference semantics == no deep constness
			using reverse_iterator = std::reverse_iterator<iterator>;
			using const_reverse_iterator = std::reverse_iterator<const_iterator>;
			using size_type = std::size_t;
			using difference_type = std::ptrdiff_t;

			static constexpr std::size_t size() noexcept {
				return N;
			}

		private:
			template<typename Func, std::size_t ...IndexPack>
			constexpr array(func_tag_t, Func func, std::index_sequence<IndexPack...>) MAYTHROW 
				: m_a{std::addressof(func(IndexPack))...} 
			{
				static_assert(tc::is_safely_constructible<T&, decltype(func(0))>::value);
			}
		public:
			template<typename Func>
			array(func_tag_t, Func func) MAYTHROW
				: array(tc::func_tag, func, std::make_index_sequence<N>())
			{}

			// make sure forwarding ctor has at least two parameters, so no ambiguity with copy/move ctors
			template <typename... Args,
				std::enable_if_t<
					tc::econstructionIMPLICIT==tc::elementwise_construction_restrictiveness<T&, Args&...>::value
				>* =nullptr
			>
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
				static_assert(tc::is_safely_constructible<T&, decltype(*it)>::value);
				STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
				_ASSERTE(itEnd==++it);
			}
		public:
			template< typename Rng,
				std::enable_if_t< 0!=N
					&& tc::econstructionIMPLICIT==tc::construction_restrictiveness<T&, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value
				>* = nullptr
			>
			constexpr explicit array(Rng&& rng) MAYTHROW
				: array(range_tag, tc::begin(rng), tc::end(rng), std::make_index_sequence<N-1>())
			{}

			array const& operator=(array const& rhs) const& noexcept(std::is_nothrow_copy_assignable<T>::value) {
				tc::cont_assign(*this,rhs);
				return *this;
			}

			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2 const&>::value>* =nullptr
			>
			array const& operator=(array<T2, N> const& rhs) const& MAYTHROW {
				tc::cont_assign(*this,rhs);
				return *this;
			}

			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2&&>::value>* =nullptr
			>
			array const& operator=(array<T2, N>&& rhs) const& MAYTHROW {
				for (std::size_t i = 0; i<N; ++i) {
					// Use tc_move(rhs)[i] instead of tc_move(rhs[i]) here so we do not
					// need to specialize for the case that rhs is an array of reference.
					// Note that it is safe to call rhs::operator[]()&& on different indices.
					m_a[i]=tc_move_always(rhs)[i];
				}
				return *this;
			}

			// iterators
			// reference semantics == no deep constness
			iterator begin() const& noexcept {
				return std::addressof(m_a[0]);
			}
			iterator end() const& noexcept {
				return std::addressof(m_a[0]) + N;
			}

			// access (no rvalue-qualified overloads, must not move data out of a reference)
			// reference semantics == no deep constness
			[[nodiscard]] constexpr T& operator[](std::size_t i) const& noexcept {
				_ASSERTDEBUG(i<N);
				return *m_a[i];
			}

			void fill(T const& t) const& noexcept {
				std::fill_n(begin(), N, t);
			}
		};

		template<typename Lhs, typename Rhs, std::size_t N>
		[[nodiscard]] constexpr bool operator==(array<Lhs, N> const& lhs, array<Rhs, N> const& rhs) noexcept {
#ifdef _DEBUG
			tc::for_each(lhs, TC_FN(_ASSERTINITIALIZED));
			tc::for_each(rhs, TC_FN(_ASSERTINITIALIZED));
#endif
			return tc::equal(lhs, rhs);
		}

		template<typename Lhs, std::size_t N, typename Rhs>
		[[nodiscard]] constexpr tc::order compare_impl(array<Lhs, N> const& lhs, Rhs const& rhs) noexcept {
#ifdef _DEBUG
			tc::for_each(lhs, TC_FN(_ASSERTINITIALIZED));
			tc::for_each(rhs, TC_FN(_ASSERTINITIALIZED));
#endif
			return tc::lexicographical_compare_3way(lhs, rhs);
		}
	} // namespace array_adl
	using array_adl::array;

	namespace no_adl {
		template<typename T, std::size_t N>
		struct constexpr_size_base<tc::array<T, N>, void> : std::integral_constant<std::size_t, N> {};
	}

	/////////////////////////////////////////////////////
	// std::array

	namespace no_adl {
		template<typename T, std::size_t N>
		struct constexpr_size_base<std::array<T, N>, void> : std::integral_constant<std::size_t, N> {};
	}

	namespace explicit_convert_std_array_detail {
		template<typename T, std::size_t N, typename Func, std::size_t... IndexPack>
		constexpr std::array<T, N> with_func_tag_impl(tc::type::identity<std::array<T, N>>, Func func, std::index_sequence<IndexPack...>) MAYTHROW {
			return {{func(IndexPack)...}};
		}

		template<typename T, std::size_t, typename... Args>
		constexpr T explicit_cast_index(Args&&... args) noexcept {
			return tc::explicit_cast<T>( std::forward<Args>(args)... );
		}

		template<typename T, std::size_t N, std::size_t ...IndexPack, typename... Args>
		constexpr std::array<T, N> with_fill_tag_impl(tc::type::identity<std::array<T, N>>, std::index_sequence<IndexPack...>, Args&&... args) MAYTHROW {
			STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
			return { {
				explicit_cast_index<T, IndexPack>(tc::const_forward<Args>(args)...)...,
				tc::explicit_cast<T>(std::forward<Args>(args)...)
			} };
		}

		template<typename T, std::size_t N, typename Iterator, std::size_t... IndexPack>
		constexpr std::array<T, N> with_range_tag_impl(tc::type::identity<std::array<T, N>>, Iterator it, Iterator itEnd, std::index_sequence<IndexPack...>) MAYTHROW {
			STATICASSERTEQUAL(N, sizeof...(IndexPack)+1);
			std::array<T, N> a{ {static_cast<T>((_ASSERTE(itEnd != it), *it)), static_cast<T>((static_cast<void>(IndexPack), ++it, _ASSERTE(itEnd != it), *it))...} }; // static_cast because int to double considered narrowing, forbidden in list initialization
			_ASSERTE(itEnd==++it);
			return a;
		}
	}

	namespace explicit_convert_adl {
		template<typename T, std::size_t N, typename Func>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>> id, tc::func_tag_t, Func func) MAYTHROW {
			static_assert(tc::is_safely_constructible<T, decltype(func(N-1))>::value);
			return tc::explicit_convert_std_array_detail::with_func_tag_impl(id, tc_move(func), std::make_index_sequence<N>());
		}

		template<typename T, std::size_t N, typename... Args, std::enable_if_t<0!=N && tc::is_explicit_castable<T, Args&&...>::value>* = nullptr>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>> id, tc::fill_tag_t, Args&& ... args) MAYTHROW {
			return tc::explicit_convert_std_array_detail::with_fill_tag_impl(id, std::make_index_sequence<N-1>(), std::forward<Args>(args)...);
		}

		template<typename T, std::size_t N, typename... Args, std::enable_if_t<std::conjunction<tc::is_explicit_castable<T, Args&&>...>::value>* = nullptr>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>>, tc::aggregate_tag_t, Args&& ... args) MAYTHROW {
			STATICASSERTEQUAL(sizeof...(Args), N, "array initializer list does not match number of elements");
			return {{tc::explicit_cast<T>(std::forward<Args>(args))...}};
		}

		template<typename T, std::size_t N, typename Rng, std::enable_if_t<
			0!=N &&
			tc::is_safely_constructible<T, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value
		>* = nullptr>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>> id, Rng&& rng) MAYTHROW {
			return tc::explicit_convert_std_array_detail::with_range_tag_impl(id, tc::begin(rng), tc::end(rng), std::make_index_sequence<N-1>());
		}

		template<typename T, std::size_t N, typename Rng, std::enable_if_t<
			0!=N
			&& !tc::is_safely_constructible<T, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value
			&& tc::is_explicit_castable<T, decltype(*tc::as_lvalue(tc::begin(std::declval<Rng&>())))>::value
		>* = nullptr>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>>, Rng&& rng) MAYTHROW {
			return tc::explicit_cast<std::array<T, N>>(tc::transform(std::forward<Rng>(rng), tc::fn_explicit_cast<T>()));
		}

		template<typename T, std::size_t N, typename Rng, std::enable_if_t<0==N>* = nullptr>
		constexpr std::array<T, N> explicit_convert_impl(adl_tag_t, tc::type::identity<std::array<T, N>>, Rng&& rng) noexcept {
			_ASSERTE(tc::empty(rng));
			return {};
		}
	}

	//////////////////////////////////////////////////////////////

	namespace no_adl {
		struct deduce_tag;

		template <typename T, typename...>
		struct delayed_deduce final {
			using type = T;
		};

		template <typename... Ts>
		struct delayed_deduce<deduce_tag, Ts...> final {
			using type = tc::common_type_t<Ts...>;
		};
	}

	template<typename TTarget, std::size_t N, typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::explicit_cast<std::array<TTarget, N>>(std::forward<Rng>(rng))
	)

	template<std::size_t N, typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::make_array<tc::range_value_t<Rng>, N>(std::forward<Rng>(rng))
	)

	template<typename TTarget, typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::make_array<TTarget, tc::constexpr_size<Rng>::value>(std::forward<Rng>(rng))
	)

	template<typename Rng>
	[[nodiscard]] constexpr auto make_array(Rng&& rng) return_decltype_MAYTHROW(
		tc::make_array<tc::constexpr_size<Rng>::value>(std::forward<Rng>(rng))
	)

	template <typename T = no_adl::deduce_tag, typename... Ts, std::enable_if_t<!std::is_reference<T>::value>* = nullptr>
	[[nodiscard]] constexpr auto make_array(tc::aggregate_tag_t, Ts&&... ts) noexcept {
		static_assert(!std::is_reference<typename no_adl::delayed_deduce<T, Ts...>::type>::value);
		return tc::explicit_cast<std::array<typename no_adl::delayed_deduce<T, Ts...>::type, sizeof...(Ts)>>(tc::aggregate_tag, std::forward<Ts>(ts)...);
	}

	// If T is a reference, force argument type T for all given arguments. That way, conversions
	// take place in the calling expression, and cases such as
	//
	//		tc::find_unique<tc::return_bool>(tc::make_array<Foo const&>(convertible_to_Foo), foo);
	//
	// will work as expected. With the usual variadic template + std::forward pattern, conversions
	// would take place inside the array constructor, resulting in a dangling reference.

	// Unfortunately, there seems to be no way to make this work in C++ without using macros
#define MAKE_ARRAY_LVALUE_REF(z, n, d) \
	template <typename T,std::enable_if_t<std::is_lvalue_reference<T>::value>* = nullptr> \
	[[nodiscard]] constexpr auto make_array(tc::aggregate_tag_t, BOOST_PP_ENUM_PARAMS(n, T t)) noexcept { \
		return tc::explicit_cast<tc::array<T, n>>(tc::aggregate_tag, BOOST_PP_ENUM_PARAMS(n, t)); \
	}

	BOOST_PP_REPEAT_FROM_TO(1, 20, MAKE_ARRAY_LVALUE_REF, _)
#undef MAKE_ARRAY_LVALUE_REF

	template< typename T, std::enable_if_t<!std::is_reference<T>::value>* =nullptr >
	[[nodiscard]] constexpr auto single(T&& t) noexcept {
		// not tc::decay_t, we want to keep reference-like proxy objects as proxy objects
		// just like the reference overload tc::single preserves lvalue references.
		return tc::make_array<std::remove_cv_t<T> >(tc::aggregate_tag,std::forward<T>(t));
	}
}
