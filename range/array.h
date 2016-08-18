//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once
#include "compare.h"
#include "implements_compare.h"
#include "storage_for.h"
#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
#include "Library/Persistence/types.h"
#endif
#include "type_traits.h"
#include "convert.h"
#include <cstdint>
#include <boost/iterator/indirect_iterator.hpp>

namespace tc {
	struct fill_tag final {};
	struct func_tag final {};
	struct transform_tag final {};

	namespace array_adl_barrier {
		template< typename T, std::size_t N >
		struct array : tc::implements_compare_partial<array<T, N>> {
			static_assert( tc::is_decayed<T>::value, "array shall be regular" );
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
			array() MAYTHROW : m_a{} {}
			array(boost::container::default_init_t) noexcept {}

		private:
			template<typename Func, std::size_t ...IndexPack>
			constexpr array(func_tag, Func func, std::index_sequence<IndexPack...>) MAYTHROW 
				: m_a{func(IndexPack)...} 
			{}
		public:
			template<typename Func>
			array(func_tag, Func func) MAYTHROW 
				: array(func_tag{}, func, std::make_index_sequence<N>()) 
			{}

		private:
			template<typename... Params, std::size_t ...IndexPack>
			static constexpr auto make_element(std::tuple<Params...> const& rhs, std::index_sequence<IndexPack...>) MAYTHROW 
				return_ctor(T, (std::get<IndexPack>(rhs)...))
		public:
			// TODO: When VS supports proper constexpr, make delegating ctors constexpr and the following private:
			template<typename... Params, std::size_t ...IndexPack>
			constexpr array(fill_tag, std::tuple<Params...>&& rhs, std::index_sequence<IndexPack...>) MAYTHROW 
				: m_a{(IndexPack, make_element(rhs, std::make_index_sequence<sizeof...(Params)>()))...} 
			{}

			template<typename... Rhs>
			array(fill_tag, Rhs&&... rhs) MAYTHROW 
				: array(fill_tag{}, std::forward_as_tuple(std::forward<Rhs>(rhs)...), std::make_index_sequence<N>()) 
			{}

			template<typename Func,typename T2>
			array(transform_tag, array<T2, N> const& arrOther, Func func) noexcept
				: array(func_tag{}, [&](std::size_t i)->T { // force return of T
					return func(VERIFYINITIALIZED(arrOther[i]));
				})
			{}

			template<typename Func,typename T2>
			array(transform_tag, array<T2, N>&& arrOther, Func func) noexcept
				: array(func_tag{}, [&](std::size_t i)->T { // force return of T
					// Use tc_move(arrOther)[i] instead of tc_move(arrOther[i]) here so we do not
					// need to specialize for the case that arrOther is an array of reference.
					// Note that it is safe to call arrOther::operator[]()&& on different indices.
					return func(VERIFYINITIALIZED(tc_move_always(arrOther)[i]));
				})
			{}

			// TODO: We inlined elementwise_construction_restrictiveness to fold_expression_bitwise_and<construction_restrictiveness...>,
			//       because it caused an internal compiler error with *.pot.cpp files on VS2015. Check if future compilers fix this!
			// make sure forwarding ctor has at least two parameters, so no ambiguity with copy/move ctors
			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					implicit_construction == fold_expression_bitwise_and<
						construction_restrictiveness<T, First>::value,
						construction_restrictiveness<T, Second>::value,
						construction_restrictiveness<T, Args>::value...
					>::value
					&& !std::is_same<tc::remove_cvref_t<First>, tc::fill_tag>::value
				>* =nullptr
			>
			constexpr array(First&& first, Second&& second, Args&& ... args) MAYTHROW
				: m_a{static_cast<T>(std::forward<First>(first)), static_cast<T>(std::forward<Second>(second)), static_cast<T>(std::forward<Args>(args))...}
			{
				static_assert(sizeof...(Args)==N-2, "array initializer list does not match number of elements");
			}

			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					explicit_construction == fold_expression_bitwise_and<
						construction_restrictiveness<T, First>::value,
						construction_restrictiveness<T, Second>::value,
						construction_restrictiveness<T, Args>::value...
					>::value
					&& !std::is_same<tc::remove_cvref_t<First>, tc::fill_tag>::value
				>* =nullptr
			>
			constexpr explicit array(First&& first, Second&& second, Args&& ... args) MAYTHROW 
				: m_a{static_cast<T>(std::forward<First>(first)), static_cast<T>(std::forward<Second>(second)), static_cast<T>(std::forward<Args>(args))...}
			{
				static_assert(sizeof...(Args)==N-2, "array initializer list does not match number of elements");
			}
			
			template <typename T2,
				std::enable_if_t<construction_restrictiveness<T, T2 const&>::value == implicit_construction>* =nullptr
			>
			array(array<T2, N> const& arrOther) MAYTHROW
				: array(transform_tag{}, arrOther, tc::identity())
			{}

			template <typename T2,
				std::enable_if_t<construction_restrictiveness<T, T2 const&>::value == explicit_construction>* =nullptr
			>
			explicit array(array<T2, N> const& arrOther) MAYTHROW
				: array(transform_tag{}, arrOther, fn_Convert<T>())
			{}

			template <typename T2,
				std::enable_if_t<construction_restrictiveness<T, T2&&>::value == implicit_construction>* =nullptr
			>
			array(array<T2, N>&& arrOther) MAYTHROW
				: array(transform_tag{}, tc_move(arrOther), tc::identity())
			{}

			template <typename T2,
				std::enable_if_t<construction_restrictiveness<T, T2&&>::value == explicit_construction>* =nullptr
			>
			explicit array(array<T2, N>&& arrOther) MAYTHROW
				: array(transform_tag{}, tc_move(arrOther), fn_Convert<T>())
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

			template<typename Func>
			array< tc::decayed_result_of_t< Func(T) >, N > transform(Func&& func) const& MAYTHROW {
				return array< tc::decayed_result_of_t< Func(T) >, N >(transform_tag{}, *this, std::forward<Func>(func));
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
				_ASSERT(i<N);
				return *(data() + i);
			}
			T& operator[](std::size_t i) & noexcept {
				_ASSERT(i<N);
				return *(data() + i);
			}
			T&& operator[](std::size_t i) && noexcept {
				_ASSERT(i<N);
				return std::forward<T>(*(data() + i)); // forward instead of tc_move does the right thing if T is a reference
			}
			T const&& operator[](std::size_t i) const&& noexcept = delete;

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

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE
			// persistence
			friend void LoadType(array& at, CXmlReader& loadhandler) noexcept {
				LoadRange(at, loadhandler);
			}

			// error reporting
			friend SReportStream& operator<<(SReportStream& rs, array const& at) noexcept {
				rs << "tc::array(";
#pragma warning( push ) 
#pragma warning( disable: 4127 ) // conditional expression is constant
				if (0<N) {
#pragma warning( pop ) 
					for (std::size_t i = 0;;) {
						rs << at[i];
						++i;
						if (i == N) break;
						rs << ',';
					}
				}
				return rs << ')';
			}
#endif
		};

		template< typename T, std::size_t N >
		struct array<T&, N> : tc::implements_compare_partial<array<T&, N>> {
			static_assert( !std::is_reference<T>::value, "" );
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
				: array(func_tag{}, func, std::make_index_sequence<N>()) {}

			template<typename Func,typename T2>
			array(transform_tag, array<T2, N> const& arrOther, Func func) noexcept
				: array(func_tag{}, [&](std::size_t i)->T& { // force return of T&
					static_assert(tc::creates_no_reference_to_temporary<decltype(func(arrOther[i])), T&>::value, "func must return a reference to T or derived type");
					return func(arrOther[i]);
				})
			{}

			template<typename Func,typename T2>
			array(transform_tag, array<T2, N> const&& arrOther, Func func) noexcept = delete; // would be ok if func maps to object which does not depend on arrOther lifetime, but there is no way to static_assert this

			// make sure forwarding ctor has at least two parameters, so no ambiguity with copy/move ctors
			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					implicit_construction == elementwise_construction_restrictiveness<T&, First&, Second&, Args&...>::value
				>* =nullptr
			>
			array(First& first, Second& second, Args& ... args) MAYTHROW 
				: m_a{std::addressof(first), std::addressof(second), std::addressof(args)...}
			{
				static_assert(sizeof...(Args)==N-2, "array initializer list does not match number of elements");
			}

			template <typename T2,
				std::enable_if_t<construction_restrictiveness<T&, T2 const&>::value == implicit_construction>* =nullptr
			>
			array(array<T2, N> const& arrOther) noexcept
				: array(transform_tag{}, arrOther, tc::identity()) 
			{}

			// allow construction from expiring array of (compatible) references, prevent construction from expiring array of values
			template <typename T2,
				std::enable_if_t<construction_restrictiveness<T&, T2&&>::value == implicit_construction>* =nullptr
			>
			array(array<T2, N> const&& arrOther) noexcept
				: array(transform_tag{}, arrOther, tc::identity()) 
			{}

			template <typename T2,
				std::enable_if_t<construction_restrictiveness<T&, T2&&>::value == forbidden_construction>* =nullptr
			>
			array(array<T2, N> const&& arrOther) noexcept = delete; // explicitly delete this constructor, otherwise array(array<T2, N> const& arrOther) above may be chosen.
			
			array const& operator=(array const& rhs) const& noexcept(std::is_nothrow_copy_assignable<T>::value) {
				boost::copy(rhs, begin());
				return *this;
			}

			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2 const&>::value>* =nullptr
			>
			array const& operator=(array<T2, N> const& rhs) const& MAYTHROW {
				boost::copy(rhs, begin());
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

			template<typename Func>
			array< tc::decayed_result_of_t< Func(T) >, N > transform(Func&& func) const& MAYTHROW {
				return array< tc::decayed_result_of_t< Func(T) >, N >(transform_tag{}, *this, std::forward<Func>(func));
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
				_ASSERT(i<N);
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

#ifndef RANGE_PROPOSAL_BUILD_STANDALONE

			// error reporting
			friend SReportStream& operator<<(SReportStream& rs, array const& at) noexcept {
				rs << "tc::array(";
#pragma warning( push ) 
#pragma warning( disable: 4127 ) // conditional expression is constant
				if (0<N) {
#pragma warning( pop ) 
					for (std::size_t i = 0;;) {
						rs << at[i];
						++i;
						if (i == N) break;
						rs << ',';
					}
				}
				return rs << ')';
			}
#endif
		};

		static_assert(has_mem_fn_size<array<int, 10>>::value, "");
		static_assert(has_mem_fn_size<array<int&, 10>>::value, "");
	}
	using array_adl_barrier::array;

	template <typename T, std::size_t N>
	struct decay<tc::array<T, N>> {
		using type = tc::array<tc::decay_t<T>, N>;
	};
}