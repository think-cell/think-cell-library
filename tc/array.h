
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once
#include "range_defines.h"
#include "const_forward.h"
#include "compare.h"
#include "implements_compare.h"
#include "storage_for.h"
#include "reference_or_value.h"
#ifdef TC_PRIVATE
#include "Library/Persistence/types.h"
#endif
#include "type_traits.h"
#include "explicit_cast.h"
#include "tag_type.h"
#include "transform.h"
#include "equal.h"
#include "cont_assign.h"

#include <cstdint>
#include <boost/iterator/indirect_iterator.hpp>

namespace tc {
	DEFINE_TAG_TYPE(fill_tag)
	DEFINE_TAG_TYPE(func_tag)
	DEFINE_TAG_TYPE(range_tag)

	namespace array_adl {
		template< typename T, std::size_t N >
		struct array : tc::implements_compare_partial<array<T, N>> {
			static_assert(!std::is_const<T>::value && !std::is_volatile<T>::value);
		private:
			T m_a[N];

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

			T* data() & noexcept {
				return std::addressof(m_a[0]);
			}
			T const* data() const& noexcept {
				return std::addressof(m_a[0]);
			}
			constexpr static std::size_t size() noexcept {
				return N;
			}

			// We cannot tell if *this is constructed using value-initialization syntax or default-initialization syntax. Therefore, we must value-initialize here.
			array() MAYTHROW : m_a() {}
			array(boost::container::default_init_t) noexcept {}

		private:
			template<typename Func, std::size_t ...IndexPack>
			constexpr array(func_tag, Func func, std::index_sequence<IndexPack...>) MAYTHROW
				: m_a{func(IndexPack)...}
			{}
		public:
			template<typename Func>
			array(func_tag, Func func) MAYTHROW
				: array(tc::func_tag(), func, std::make_index_sequence<N>())
			{}

		private:
			template <std::size_t, typename... Args>
			constexpr static T explicit_cast_index(Args&&... args) noexcept {
				return tc::explicit_cast<T>( std::forward<Args>(args)... );
			}

			template<
				typename... Args,
				std::size_t ...IndexPack
#if defined(_MSC_FULL_VER) && 191326128<=_MSC_FULL_VER // Copy-elision is required to initialize the array from function return value
				, typename T_ = T,
				std::enable_if_t<
					std::is_move_constructible<T_>::value
				>* = nullptr
#endif
			>
			explicit constexpr array(fill_tag, std::index_sequence<IndexPack...>, Args&&... args) MAYTHROW
				: m_a{
					explicit_cast_index<IndexPack>(tc::const_forward<Args>(args)...)...,
					tc::explicit_cast<T>(std::forward<Args>(args)...)
				}
			{
				static_assert(N == sizeof...(IndexPack)+1);
			}
		public:
			template<
				typename... Args
#if defined(_MSC_FULL_VER) && 191326128<=_MSC_FULL_VER
				, typename T_ = T,
				std::enable_if_t<
					std::is_move_constructible<T_>::value
				>* = nullptr
#endif
			>
			explicit constexpr array(fill_tag, Args&&... args) MAYTHROW
				: array(fill_tag{}, std::make_index_sequence<N-1>(), std::forward<Args>(args)...)
			{}
#if defined(_MSC_FULL_VER) && 191326128<=_MSC_FULL_VER
		private:
			template <std::size_t, typename Arg>
			constexpr static Arg&& identity_index(Arg&& args) noexcept {
				return std::forward<Arg>(args);
			}

			template<
				typename Arg,
				typename... Args,
				std::size_t ...IndexPack,
				typename T_ = T,
				std::enable_if_t<
					!std::is_move_constructible<T_>::value
				>* = nullptr
			>
			constexpr array(fill_tag, std::index_sequence<IndexPack...>, Arg&& arg, Args&&... args) MAYTHROW
				: m_a{
					{identity_index<IndexPack>(tc::const_forward<Arg>(arg)), const_forward<Args>(args)...}...,
					{std::forward<Arg>(arg), std::forward<Args>(args)...}
				}
			{
				static_assert(N == sizeof...(IndexPack)+1);
			}
		public:
			template<
				typename... Params,
				std::enable_if_t<
					0 < sizeof...(Params)
					&& !std::is_move_constructible<T>::value
				>* = nullptr
			>
			constexpr array(fill_tag, Params&&... params) MAYTHROW
				: array(fill_tag{}, std::make_index_sequence<N-1>(), std::forward<Params>(params)...)
			{}
#endif

			template <typename... Args,
				std::enable_if_t<
					elementwise_construction_restrictiveness<T, Args...>::value == implicit_construction
				>* = nullptr
			>
			constexpr array(tc::aggregate_tag, Args&& ... args) MAYTHROW
				: m_a{static_cast<T>(std::forward<Args>(args))...}
			{
				static_assert(sizeof...(Args)==N, "array initializer list does not match number of elements");
			}

			template <typename... Args,
				std::enable_if_t<
					elementwise_construction_restrictiveness<T, Args...>::value == explicit_construction
				>* = nullptr
			>
			constexpr explicit array(tc::aggregate_tag, Args&& ... args) MAYTHROW
				: m_a{static_cast<T>(std::forward<Args>(args))...}
			{
				static_assert(sizeof...(Args)==N, "array initializer list does not match number of elements");
			}

		private:
			template<typename Iterator, std::size_t ...IndexPack>
			array(range_tag, Iterator it, Iterator itEnd, std::index_sequence<IndexPack...>) MAYTHROW
				: m_a{*it, (static_cast<void>(IndexPack), *++it)...}
			{
				_ASSERT(itEnd==++it);
			}
		public:
			template< typename Rng,
				std::enable_if_t< 0!=N
					&& construction_restrictiveness<T, decltype(tc_front(std::declval<Rng&>()))>::value == implicit_construction
				>* = nullptr
			>
			explicit array(Rng&& rng) MAYTHROW
				: array(range_tag(), tc::begin(rng), tc::end(rng), std::make_index_sequence<N-1>())
			{}

			template< typename Rng,
				std::enable_if_t< 0!=N 
					&& construction_restrictiveness<T, decltype(tc_front(std::declval<Rng&>()))>::value == explicit_construction
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
					*(data() + i)=VERIFYINITIALIZED(rhs[i]);
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
					*(data() + i)=VERIFYINITIALIZED(tc_move(rhs)[i]);
				}
				return *this;
			}

			// iterators
			const_iterator begin() const& noexcept {
				return data();
			}
			const_iterator end() const& noexcept {
				return data() + N;
			}
			iterator begin() & noexcept {
				return data();
			}
			iterator end() & noexcept {
				return data() + N;
			}

			// access
			T const& operator[](std::size_t i) const& noexcept {
				// Analysis of release mode assembly showed a significant overhead here:
				// Without _ASSERT, this function usually consist of only one or a few "lea" instructions that can easily be inlined.
				// With (non-debug) _ASSERT, the comparison, jump and call to ReportAssert dominate the run-time and prevent the function to get inlined.
				_ASSERTDEBUG(i<N);
				return *(data() + i);
			}
			T& operator[](std::size_t i) & noexcept {
				_ASSERTDEBUG(i<N);
				return *(data() + i);
			}
			T&& operator[](std::size_t i) && noexcept {
				_ASSERTDEBUG(i<N);
				return std::forward<T>(*(data() + i)); // forward instead of tc_move does the right thing if T is a reference
			}
			T const&& operator[](std::size_t i) const&& noexcept {
				return static_cast<T const&&>((*this)[i]);
			}

			void fill(T const& t) & noexcept {
				std::fill_n(data(), N, t);
			}

			friend tc::order compare(array const& lhs, array const& rhs) noexcept {
#ifdef _DEBUG
				for (std::size_t i=0; i<N; ++i) VERIFYINITIALIZED(rhs[i]);
				for (std::size_t i=0; i<N; ++i) VERIFYINITIALIZED(lhs[i]);
#endif
				return tc::lexicographical_compare_3way(lhs, rhs);
			}

			friend bool operator==(array const& lhs, array const& rhs) noexcept {
#ifdef _DEBUG
				for (std::size_t i=0; i<N; ++i) VERIFYINITIALIZED(rhs[i]);
				for (std::size_t i=0; i<N; ++i) VERIFYINITIALIZED(lhs[i]);
#endif
				return tc::equal(lhs, rhs);
			}

#ifdef TC_PRIVATE
			// persistence
			friend void LoadType(array& at, CXmlReader& loadhandler) noexcept {
				LoadRange(at, loadhandler);
			}
#endif
		};

		template< typename T, std::size_t N >
		struct array<T&, N> : tc::implements_compare_partial<array<T&, N>> {
			static_assert( !std::is_reference<T>::value );
		private:
			T* m_a[N];

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

			constexpr static std::size_t size() noexcept {
				return N;
			}

		private:
			template<typename Func, std::size_t ...IndexPack>
			constexpr array(func_tag, Func func, std::index_sequence<IndexPack...>) MAYTHROW 
				: m_a{std::addressof(func(IndexPack))...} 
			{
				static_assert(tc::creates_no_reference_to_temporary<decltype(func(0)), T&>::value, "func must return a reference to T or derived type");
			}
		public:
			template<typename Func>
			array(func_tag, Func func) MAYTHROW 
				: array(tc::func_tag(), func, std::make_index_sequence<N>())
			{}

			// make sure forwarding ctor has at least two parameters, so no ambiguity with copy/move ctors
			template <typename... Args,
				std::enable_if_t<
					elementwise_construction_restrictiveness<T&, Args&...>::value == implicit_construction
				>* =nullptr
			>
			constexpr array(tc::aggregate_tag, Args& ... args) MAYTHROW
				: m_a{std::addressof(args)...}
			{
				static_assert(sizeof...(Args)==N, "array initializer list does not match number of elements");
			}

		private:
			DEFINE_TAG_TYPE(range_tag)

			template<typename Iterator, std::size_t ...IndexPack>
			array(range_tag, Iterator it, Iterator itEnd, std::index_sequence<IndexPack...>) MAYTHROW
				: m_a{std::addressof(*it), (static_cast<void>(IndexPack), std::addressof(*++it))...}
			{
				static_assert(tc::creates_no_reference_to_temporary<decltype(*it), T&>::value, "*it must return a reference to T or derived type");
				_ASSERT(itEnd==++it);
			}
		public:
			template< typename Rng,
				std::enable_if_t< 0!=N
					&& construction_restrictiveness<T&, decltype(tc_front(std::declval<Rng&>()))>::value == implicit_construction
				>* = nullptr
			>
			explicit array(Rng&& rng) MAYTHROW
				: array(range_tag(), tc::begin(rng), tc::end(rng), std::make_index_sequence<N-1>())
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
			T& operator[](std::size_t i) const& noexcept {
				_ASSERTDEBUG(i<N);
				return *m_a[i];
			}

			void fill(T const& t) const& noexcept {
				std::fill_n(begin(), N, t);
			}

			friend tc::order compare(array const& lhs, array const& rhs) noexcept {
				return tc::lexicographical_compare_3way(lhs, rhs);
			}

			friend bool operator==(array const& lhs, array const& rhs) noexcept {
				return tc::equal(lhs, rhs);
			}
		};

		static_assert(has_mem_fn_size<array<int, 10>>::value);
		static_assert(has_mem_fn_size<array<int&, 10>>::value);

		static_assert(!std::is_convertible<array<int,10>, array<int, 9>>::value);
		static_assert(!std::is_convertible<array<int,9>, array<int, 10>>::value);

#ifdef TC_PRIVATE
		// error reporting
		template< typename T, std::size_t N, std::enable_if_t<!tc::is_char<std::remove_reference_t<T>>::value>* = nullptr >
		void append_to(tc::SReportStream& rs, array< T, N > const& arr) noexcept {
			tc::append(rs, "tc::array(");
			if constexpr( 0<N ) {
				for(std::size_t i=0;;){
					tc::append(rs, arr[i]);
					++i;
					if(i==N) break;
					tc::append(rs, ",");
				}
			}
			tc::append(rs, ")");
		}
#endif
	} // namespace array_adl
	using array_adl::array;

	namespace no_adl {
		template<typename T, std::size_t N>
		struct constexpr_size<tc::array<T,N>> : std::integral_constant<std::size_t, N> {};

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

	template <typename Rng>
	constexpr auto make_array(Rng&& rng) noexcept {
		return tc::array<tc::decay_t<decltype(tc_front(rng))>, tc::constexpr_size<tc::remove_cvref_t<Rng>>::value>(std::forward<Rng>(rng));
	}

	template <typename T = no_adl::deduce_tag, typename... Ts, std::enable_if_t<!std::is_reference<T>::value>* = nullptr>
	constexpr auto make_array(tc::aggregate_tag, Ts&&... ts) noexcept {
		static_assert(!std::is_reference<typename no_adl::delayed_deduce<T, Ts...>::type>::value);
		return tc::array<typename no_adl::delayed_deduce<T, Ts...>::type, sizeof...(Ts)>(tc::aggregate_tag(), std::forward<Ts>(ts)...);
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
	auto make_array(tc::aggregate_tag, BOOST_PP_ENUM_PARAMS(n, T t)) noexcept { \
		return tc::array<T, n>(tc::aggregate_tag(), BOOST_PP_ENUM_PARAMS(n, t)); \
	}

	BOOST_PP_REPEAT_FROM_TO(1, 20, MAKE_ARRAY_LVALUE_REF, _)
#undef MAKE_ARRAY_LVALUE_REF

	template< typename T, std::enable_if_t<!std::is_reference<T>::value>* =nullptr >
	decltype(auto) single(T&& t) noexcept {
		// not tc::decay_t, we want to keep reference-like proxy objects as proxy objects
		// just like the reference overload tc::single preserves lvalue references.
		return tc::make_array<std::remove_cv_t<T> >(tc::aggregate_tag(),std::forward<T>(t));
	}

	template <typename T, std::size_t N>
	struct decay<tc::array<T, N>> {
		using type = tc::array<tc::decay_t<T>, N>;
	};

	template <typename... Ts>
	constexpr std::size_t count_args(Ts&&...) { return sizeof...(Ts); }

	template <typename... Ts>
	tc::common_type_t<Ts&&...> declval_common_type(Ts&&...) noexcept;
}

#define MAKE_TYPED_CONSTEXPR_ARRAY(type, ...) \
	[]() noexcept -> tc::array<type, tc::count_args(__VA_ARGS__)> const& { \
		static constexpr tc::array<type, tc::count_args(__VA_ARGS__)> c_at(tc::aggregate_tag(), __VA_ARGS__); \
		return c_at; \
	}()

#define MAKE_CONSTEXPR_ARRAY(...) \
	MAKE_TYPED_CONSTEXPR_ARRAY(decltype(tc::declval_common_type(__VA_ARGS__)), __VA_ARGS__)
