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
		template< std::size_t N, typename T, typename First, typename Second, typename... Args>
		void uninitialized_init(tc::storage_for<T>* pt, First&& first, Second&& second, Args&& ... args) MAYTHROW {
			pt->ctor(Convert<T>(std::forward<First>(first)));
			try {
				uninitialized_init<N - 1>(pt + 1, std::forward<Second>(second), std::forward<Args>(args)...);
			}
			catch (...) {
				pt->dtor();
				throw;
			}
		}

		template< std::size_t N, typename T, typename First>
		void uninitialized_init(tc::storage_for<T>* pt, First&& first) noexcept {
			static_assert(1 == N, "array initializer list does not match number of elements");
			pt->ctor(Convert<T>(std::forward<First>(first)));
		}

		template< typename T, std::size_t N >
		struct array : tc::implements_compare_partial<array<T, N>> {
			static_assert( tc::is_decayed<T>::value, "array shall be regular" );
		private:
			using Array = tc::storage_for<T>[N];
			Array m_a;

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

			T* data() noexcept {
				return std::addressof(*m_a[0]);
			}
			T const* data() const noexcept {
				return std::addressof(*m_a[0]);
			}
			std::size_t size() const noexcept {
				return N;
			}

			array() MAYTHROW {
				std::size_t i = 0;
				try {
					for (; i<N; ++i) {
						m_a[i].ctor_value(); // We cannot tell if *this is constructed using value-initialization syntax or default-initialization syntax. Therefore, we must value-initialize here.
					}
				}
				catch (...) {
					while (0<i) {
						--i;
						m_a[i].dtor();
					}
					throw;
				}
			}

			template<typename Func>
			array(func_tag, Func func) MAYTHROW {
				std::size_t i = 0;
				try {
					for (; i<N; ++i) {
						m_a[i].ctor(func(i));
					}
				}
				catch (...) {
					while (0<i) {
						--i;
						m_a[i].dtor();
					}
					throw;
				}
			}

			template<typename Rhs>
			array(fill_tag, Rhs&& rhs) MAYTHROW {
				// std::unitialized_fill_n takes initial value by const reference
				static_assert(0<N, "");
				std::size_t i = 0;
				try {
					for (; i<N - 1; ++i) {
						m_a[i].ctor(rhs);
					}
					m_a[i].ctor(std::forward<Rhs>(rhs));
				}
				catch (...) {
					while (0<i) {
						--i;
						m_a[i].dtor();
					};
					throw;
				}
			}

			template<typename Func,typename T2>
			array(transform_tag, array<T2, N> const& arrOther, Func func) noexcept
				: array(func_tag{}, [&](std::size_t i)->T { // force return of T
					return func(arrOther[i]);
				})
			{}

			template<typename Func,typename T2>
			array(transform_tag, array<T2, N>&& arrOther, Func func) noexcept
				: array(func_tag{}, [&](std::size_t i)->T { // force return of T
					// Use tc_move(arrOther)[i] instead of tc_move(arrOther[i]) here so we do not
					// need to specialize for the case that arrOther is an array of reference.
					// Note that it is safe to call arrOther::operator[]()&& on different indices.
					return func(tc_move_always(arrOther)[i]);
				})
			{}

			// make sure forwarding ctor has at least two parameters, so no ambiguity with copy/move ctors
			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					implicit_initialization == accumulated_initialization_restrictiveness<T, First, Second, Args...>::value
				>* =nullptr
			>
			array(First&& first, Second&& second, Args&& ... args) MAYTHROW {
				uninitialized_init<N>(m_a, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...);
			}

			template< typename First, typename Second, typename... Args,
				std::enable_if_t<
					explicit_initialization == accumulated_initialization_restrictiveness<T, First, Second, Args...>::value
				>* =nullptr
			>
			explicit array(First&& first, Second&& second, Args&& ... args) MAYTHROW {
				uninitialized_init<N>(m_a, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...);
			}

			array(array&& rhs) noexcept(std::is_nothrow_move_constructible<T>::value) {
				// if ctors throw, all already constructed objects are destroyed and the exception is rethrown
				std::uninitialized_copy(
					std::make_move_iterator(boost::begin(rhs)),
					std::make_move_iterator(boost::end(rhs)),
					m_a[0].uninitialized_addressof()
				);
			}
			array(array const& rhs) noexcept(std::is_nothrow_copy_constructible<T>::value) {
				// if ctors throw, all already constructed objects are destroyed and the exception is rethrown
				std::uninitialized_copy(
					boost::begin(rhs),
					boost::end(rhs),
					m_a[0].uninitialized_addressof()
				);
			}

			template <typename T2,
				std::enable_if_t<initialization_restrictiveness<T, T2 const&>::value == implicit_initialization>* =nullptr
			>
			array(array<T2, N> const& arrOther) MAYTHROW
				: array(transform_tag{}, arrOther, fn_Convert<T>())
			{}

			template <typename T2,
				std::enable_if_t<initialization_restrictiveness<T, T2 const&>::value == explicit_initialization>* =nullptr
			>
			explicit array(array<T2, N> const& arrOther) MAYTHROW
				: array(transform_tag{}, arrOther, fn_Convert<T>())
			{}

			template <typename T2,
				std::enable_if_t<initialization_restrictiveness<T, T2&&>::value == implicit_initialization>* =nullptr
			>
			array(array<T2, N>&& arrOther) MAYTHROW
				: array(transform_tag{}, tc_move(arrOther), fn_Convert<T>())
			{}

			template <typename T2,
				std::enable_if_t<initialization_restrictiveness<T, T2&&>::value == explicit_initialization>* =nullptr
			>
			explicit array(array<T2, N>&& arrOther) MAYTHROW
				: array(transform_tag{}, tc_move(arrOther), fn_Convert<T>())
			{}
			
			array& operator=(array&& rhs) & noexcept(std::is_nothrow_move_assignable<T>::value) {
				std::move(boost::begin(rhs), boost::end(rhs), begin());
				return *this;
			}
			array& operator=(array const& rhs) & noexcept(std::is_nothrow_copy_assignable<T>::value) {
				std::copy(boost::begin(rhs), boost::end(rhs), begin());
				return *this;
			}

			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2 const&>::value>* =nullptr
			>
			array& operator=(array<T2, N> const& rhs) & MAYTHROW {
				for (std::size_t i = 0; i<N; ++i) {
					*(data() + i)=Convert<T>(rhs[i]);
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
					*(data() + i)=Convert<T>(tc_move(rhs)[i]);
				}
				return *this;
			}

			~array() {
				// destruction order like built-in array: http://stackoverflow.com/questions/1781802/order-of-destruction-for-array-of-objects
				for (std::size_t i = N; 0<i; ) {
					--i;
					m_a[i].dtor();
				}
			}

			template<typename Func>
			array< tc::decayed_result_of_t< Func(T) >, N > transform(Func&& func) const MAYTHROW {
				return array< tc::decayed_result_of_t< Func(T) >, N >(transform_tag{}, *this, std::forward<Func>(func));
			}

			// iterators
			const_iterator begin() const noexcept {
				return data();
			}
			const_iterator end() const noexcept {
				return data() + N;
			}
			iterator begin() noexcept {
				return data();
			}
			iterator end() noexcept {
				return data() + N;
			}

			// access
			T const& operator[](std::size_t i) const & noexcept {
				_ASSERT(i<N);
				return *(data() + i);
			}
			T & operator[](std::size_t i) & noexcept {
				_ASSERT(i<N);
				return *(data() + i);
			}
			T && operator[](std::size_t i) && noexcept {
				_ASSERT(i<N);
				return std::forward<T>(*(data() + i)); // forward instead of tc_move does the right thing if T is a reference
			}
			T const&& operator[](std::size_t i) const && noexcept = delete;

			void fill(T const& t) noexcept {
				std::fill_n(data(), N, t);
			}

			friend tc::order compare(array const& lhs, array const& rhs) noexcept {
				return tc::lexicographical_compare_3way(lhs, rhs);
			}

			friend bool operator==(array const& lhs, array const& rhs) noexcept {
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
			using Array = tc::storage_for<T&>[N];
			Array m_a;

		public:
			using value_type = tc::decay_t<T>;
			using reference = T&;
			using const_reference = T&; // reference semantics == no deep constness
			using pointer = T *;
			using iterator = boost::indirect_iterator<tc::storage_for<T&> const*, value_type, /*CategoryOrTraversal*/boost::use_default, reference>;
			using const_iterator = iterator; // reference semantics == no deep constness
			using reverse_iterator = std::reverse_iterator<iterator>;
			using const_reverse_iterator = std::reverse_iterator<const_iterator>;
			using size_type = std::size_t;
			using difference_type = std::ptrdiff_t;

			std::size_t size() const noexcept {
				return N;
			}

			template<typename Func>
			array(func_tag, Func func) MAYTHROW {
				std::size_t i = 0;
				try {
					for (; i<N; ++i) {
						static_assert(tc::creates_no_reference_to_temporary<decltype(func(i)), T&>::value, "func must return a reference to T or derived type");
						m_a[i].ctor(func(i));
					}
				}
				catch (...) {
					while (0<i) {
						--i;
						m_a[i].dtor();
					}
					throw;
				}
			}

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
					implicit_initialization == accumulated_initialization_restrictiveness<T&, First, Second, Args...>::value
				>* =nullptr
			>
			array(First&& first, Second&& second, Args&& ... args) MAYTHROW {
				uninitialized_init<N>(m_a, std::forward<First>(first), std::forward<Second>(second), std::forward<Args>(args)...);
			}
			
			array(array const& rhs) noexcept {
				for (std::size_t i = 0; i<N; ++i) {
					m_a[i].ctor(rhs[i]);
				}
			}

			template <typename T2,
				std::enable_if_t<initialization_restrictiveness<T&, T2 const&>::value == implicit_initialization>* =nullptr
			>
			array(array<T2, N> const& arrOther) noexcept {
				for (std::size_t i = 0; i<N; ++i) {
					m_a[i].ctor(arrOther[i]);
				}
			}

			// allow construction from expiring array of (compatible) references, prevent construction from expiring array of values
			template <typename T2,
				std::enable_if_t<initialization_restrictiveness<T&, T2&&>::value == implicit_initialization>* =nullptr
			>
			array(array<T2, N> const&& arrOther) noexcept {
				for (std::size_t i = 0; i<N; ++i) {
					m_a[i].ctor(arrOther[i]);
				}
			}

			template <typename T2,
				std::enable_if_t<initialization_restrictiveness<T&, T2&&>::value == forbidden_initialization>* =nullptr
			>
			array(array<T2, N> const&& arrOther) noexcept = delete; // explicitly delete this constructor, otherwise array(array<T2, N> const& arrOther) above may be chosen.
			
			array const& operator=(array const& rhs) const noexcept(std::is_nothrow_copy_assignable<T>::value) {
				std::copy(boost::begin(rhs), boost::end(rhs), begin());
				return *this;
			}

			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2 const&>::value>* =nullptr
			>
			array const& operator=(array<T2, N> const& rhs) const MAYTHROW {
				for (std::size_t i = 0; i<N; ++i) {
					*m_a[i]=Convert<T>(rhs[i]);
				}
				return *this;
			}

			template <typename T2,
				std::enable_if_t<tc::is_safely_assignable<T&, T2&&>::value>* =nullptr
			>
			array const& operator=(array<T2, N>&& rhs) const MAYTHROW {
				for (std::size_t i = 0; i<N; ++i) {
					// Use tc_move(rhs)[i] instead of tc_move(rhs[i]) here so we do not
					// need to specialize for the case that rhs is an array of reference.
					// Note that it is safe to call rhs::operator[]()&& on different indices.
					*m_a[i]=Convert<T>(tc_move_always(rhs)[i]);
				}
				return *this;
			}

			~array() {
				// destruction order like built-in array: http://stackoverflow.com/questions/1781802/order-of-destruction-for-array-of-objects
				for (std::size_t i = N; 0<i; ) {
					--i;
					m_a[i].dtor();
				}
			}

			template<typename Func>
			array< tc::decayed_result_of_t< Func(T) >, N > transform(Func&& func) const MAYTHROW {
				return array< tc::decayed_result_of_t< Func(T) >, N >(transform_tag{}, *this, std::forward<Func>(func));
			}

			// iterators
			// reference semantics == no deep constness
			iterator begin() const noexcept {
				return std::addressof(m_a[0]);
			}
			iterator end() const noexcept {
				return std::addressof(m_a[0]) + N;
			}

			// access (no rvalue-qualified overloads, must not move data out of a reference)
			// reference semantics == no deep constness
			T& operator[](std::size_t i) const noexcept {
				_ASSERT(i<N);
				return *m_a[i];
			}

			void fill(T const& t) const noexcept {
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
	}
	using array_adl_barrier::array;

	template <typename T, std::size_t N>
	struct decay<tc::array<T, N>> {
		using type = tc::array<tc::decay_t<T>, N>;
	};
}