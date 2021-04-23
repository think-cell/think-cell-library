
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "index_iterator.h"
#include "range_adaptor.h"
#include "storage_for.h"
#include "renew.h"
#include "tag_type.h"
#include "filter_inplace.h"
#include "append.h"

#include <boost/preprocessor/enum.hpp>

#include <algorithm>

MODIFY_WARNINGS_BEGIN(((disable)(4297))) // 'function' : function assumed not to throw an exception but does.

namespace tc {
	using static_vector_size_t = std::uint32_t; // fixed width integer for shared heap

	DEFINE_TAG_TYPE(constexpr_tag)

	namespace no_adl {
		template<typename T, tc::static_vector_size_t N> 
		struct static_vector_base {
			using size_type = tc::static_vector_size_t;
		protected:
			size_type m_iEnd = 0;
		public:
			// query state
			constexpr bool full() const& noexcept {
				return N == m_iEnd;
			}
			constexpr size_type size() const& noexcept {
				return m_iEnd;
			}
			static constexpr size_type capacity() noexcept {
				return N;
			}
		};

		// Inside element dtors, the element is already removed from the container.
		// Inside element ctors, the element is already in the container.
		template<typename T, tc::static_vector_size_t N>
		struct static_vector_base_nontrivial : static_vector_base<T, N> {
			using base = static_vector_base<T, N>;
			using typename base::size_type;
		private:
			static_assert(!std::is_trivially_destructible<T>::value || !std::is_trivially_default_constructible<T>::value);
		protected:
			std::conditional_t<std::is_trivially_destructible<T>::value, tc::storage_for_without_dtor<T>, tc::storage_for<T>> m_aot[N];
			constexpr T const& dereference(size_type n) const& noexcept { return *m_aot[n]; }
			constexpr T& dereference(size_type n) & noexcept { return tc::as_mutable(tc::as_const(*this).dereference(n)); }
		public:
			template<typename... Args>
			T& emplace_back(Args&& ... args) & noexcept(noexcept(m_aot[this->m_iEnd - 1].ctor_value(std::forward<Args>(args)...))) {
				_ASSERTE(!this->full());
				auto& ot=m_aot[this->m_iEnd];
				++this->m_iEnd;
				// Inside element ctors, the element is already in the container.
				try {
					ot.ctor_value(std::forward<Args>(args)...); // MAYTHROW
					return *ot;
				} catch (...) {
					--this->m_iEnd;
					throw;
				}
			}

			T* data() & noexcept {
				return m_aot[0].uninitialized_addressof();
			}
			T const* data() const& noexcept {
				return m_aot[0].uninitialized_addressof();
			}
		};

		// Inside element ctors, the element is already in the container.
		template<typename T, tc::static_vector_size_t N>
		struct static_vector_base_trivial : static_vector_base<T, N> {
			using base = static_vector_base<T, N>;
			using typename base::size_type;
		private:
			static_assert(std::is_trivially_destructible<T>::value && std::is_trivially_default_constructible<T>::value);
			T m_at[N];
		protected:
			constexpr T const& dereference(size_type n) const& noexcept { return m_at[n]; }
			constexpr T& dereference(size_type n) & noexcept { return tc::as_mutable(tc::as_const(*this).dereference(n)); }
		public:
			static_vector_base_trivial() noexcept {};

			constexpr static_vector_base_trivial(constexpr_tag_t) noexcept : m_at{} {};

			// Due to C++17 constexpr limitations this special purpose constructor is the only way
			// we have to initialize static_vector at compile time without generally sacrificing efficiency.
			template<typename Rng, std::enable_if_t<tc::is_safely_assignable<T&, tc::range_value_t<Rng const>>::value>* = nullptr>
			explicit constexpr static_vector_base_trivial(constexpr_tag_t, Rng const& rng) noexcept
				: m_at{} // C++17 constexpr requires immediate full initialization of arrays
			{
				// - cannot use tc::for_each(...) in Visual C++ because of a bug causing the compiler to claim that the statements
				//   in the lambda are non-constant when the static_vector to be initialized is a member of another constexpr struct.
				// - cannot use tc::for_each(...) in clang because evaluation of lambdas with captures is not yet supported in constexpr.
				for (auto it = tc::begin(rng); it != tc::end(rng); ++it) {
					_ASSERTE(this->m_iEnd < N);
					m_at[this->m_iEnd] = *it;
					++this->m_iEnd;
				}
			}

			// This specialization for trivially-assignable types is usable in constant expressions.
			// The normal version of emplace_back cannot be used in constant expressions, because it uses placement new.
			template<typename... Args>
			constexpr typename std::enable_if<std::is_trivially_assignable<T&, T>::value && noexcept(T(std::declval<Args&&>()...)), T&>::type emplace_back(Args&& ... args) & noexcept {
				_ASSERTE(!this->full());
				T& t = m_at[this->m_iEnd];
				++this->m_iEnd;
				t = T(std::forward<Args>(args)...);
				return t;
			}

			template<typename... Args>
			typename std::enable_if < !std::is_trivially_assignable<T&, T>::value || !noexcept(T(std::declval<Args&&>()...)), T&>::type emplace_back(Args&& ... args) & noexcept(noexcept(T(std::forward<Args>(args)...))) {
				_ASSERTE(!this->full());
				T& t = m_at[this->m_iEnd];
				++this->m_iEnd;
				// Inside element ctors, the element is already in the container.
				try {
					::new (static_cast<void*>(std::addressof(t))) T(std::forward<Args>(args)...); // MAYTHROW
					return t;
				} catch (...) {
					--this->m_iEnd;
					throw;
				}
			}

			constexpr T* data() & noexcept {
				return std::addressof(m_at[0]);
			}
			constexpr T const* data() const& noexcept {
				return std::addressof(m_at[0]);
			}
		};

		template<typename T, tc::static_vector_size_t N>
		struct static_vector_base_nontrivial_dtor : static_vector_base_nontrivial<T, N> {
			static_vector_base_nontrivial_dtor() = default;
			using static_vector_base_nontrivial<T, N>::static_vector_base_nontrivial;

			~static_vector_base_nontrivial_dtor() {
				shrink(0);
			}

			void pop_back() & noexcept {
				_ASSERTE( 0 < this->m_iEnd );
				--this->m_iEnd;
				// Inside element dtors, the element is already removed from the container.
				this->m_aot[this->m_iEnd].dtor();
			}

		protected:
			void shrink(tc::static_vector_size_t n) noexcept {
				_ASSERTE( n <= this->size() );
				while (n != this->size()) {
					pop_back();
				}
			}
		};

		template<typename T, tc::static_vector_size_t N>
		using static_vector_base_trivial_dtor_base = std::conditional_t<
			std::is_trivially_default_constructible<T>::value,
			tc::no_adl::static_vector_base_trivial<T, N>,
			tc::no_adl::static_vector_base_nontrivial<T, N>
		>;

		template<typename T, tc::static_vector_size_t N>
		struct static_vector_base_trivial_dtor : static_vector_base_trivial_dtor_base<T, N> {
			static_vector_base_trivial_dtor() = default;
			using static_vector_base_trivial_dtor_base<T, N>::static_vector_base_trivial_dtor_base;

			constexpr void pop_back() & noexcept {
				_ASSERTE( 0 < this->m_iEnd );
				--this->m_iEnd;
			}

		protected:
			constexpr void shrink(tc::static_vector_size_t n) & noexcept {
				_ASSERTE( n <= this->size() );
				this->m_iEnd = n;
			}
		};

	} // no_adl

	namespace static_vector_adl {
		template<typename T, tc::static_vector_size_t N>
		using static_vector_base_t = std::conditional_t<
			std::is_trivially_destructible<T>::value,
			tc::no_adl::static_vector_base_trivial_dtor<T, N>,
			tc::no_adl::static_vector_base_nontrivial_dtor<T, N>
		>;

		template< typename T, tc::static_vector_size_t N >
		struct [[nodiscard]] static_vector
			: static_vector_base_t<T, N>
			, tc::range_iterator_from_index<
				static_vector<T, N>,
				tc::static_vector_size_t // fixed width integer for shared heap
			>
		{
		private:
			using this_type = static_vector;
		public:
			using base = static_vector_base_t<T, N>;
			using typename this_type::range_iterator_from_index::index;
			using difference_type = std::make_signed_t<index>;
			using reference = T&;
			using value_type = T; // needed for has_mem_fn_emplace_back

			static constexpr bool c_bHasStashingIndex=false;

			static_vector() noexcept {}
			STATICASSERTEQUAL( std::is_trivially_destructible<base>::value, std::is_trivially_destructible<T>::value );

			explicit constexpr static_vector(constexpr_tag_t) : base(constexpr_tag) {}

			// Due to C++17 constexpr limitations this special purpose constructor is the only way
			// we have to initialize static_vector at compile time without generally sacrificing efficiency.
			template<
				typename Rng,
				std::enable_if_t<
					std::is_trivially_destructible<T>::value &&
					std::is_trivially_default_constructible<T>::value && 
					tc::is_safely_assignable<T&, tc::range_value_t<Rng const>>::value
				>* = nullptr
			>
			constexpr static_vector(constexpr_tag_t tag, Rng const& rng) noexcept
				: base(tag, rng)
			{}

			template <typename... Args,
				std::enable_if_t<
					0 < sizeof...(Args) &&
					tc::econstructionIMPLICIT==tc::elementwise_construction_restrictiveness<T, Args...>::value
				>* = nullptr
			>
			constexpr static_vector(tc::aggregate_tag_t, Args&& ... args) noexcept(std::conjunction<std::is_nothrow_constructible<T, Args&&>...>::value) {
				static_assert(sizeof...(Args)<=N, "vector initializer list contains too many elements");
				(this->emplace_back(std::forward<Args>(args)), ...);
			}

			template <typename... Args,
				std::enable_if_t<
					0 == sizeof...(Args) ||
					tc::econstructionEXPLICIT==tc::elementwise_construction_restrictiveness<T, Args...>::value
				>* = nullptr
			>
			constexpr explicit static_vector(tc::aggregate_tag_t, Args&& ... args) MAYTHROW {
				static_assert(sizeof...(Args)<=N, "vector initializer list contains too many elements");
				(tc::cont_emplace_back(*this, std::forward<Args>(args)), ...); // cont_emplace_back for lazy explicit_cast
			}

			static_vector(static_vector const& vec) noexcept(std::is_nothrow_copy_constructible<T>::value) {
				tc::append(*this, vec);
			}

			static_vector(static_vector&& vec) noexcept(std::is_nothrow_move_constructible<T>::value) {
				tc::append(*this, tc_move_always(vec));
			}

			constexpr static_vector& operator=(static_vector const& vec) & noexcept(std::is_nothrow_copy_assignable<T>::value) {
				if( std::addressof(vec)!=this ) {
					assign(vec);
				}
				return *this;
			}

			constexpr static_vector& operator=(static_vector&& vec) & noexcept(std::is_nothrow_move_assignable<T>::value) {
				_ASSERTE( std::addressof(vec)!=this ); // self assignment from rvalues should not happen, rvalues must be expiring
				NOEXCEPT( assign( tc_move_always(vec) ) );
				return *this;
			}
		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> index { return 0; }
			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> index { return this->m_iEnd; }
			STATIC_FINAL_MOD(constexpr, increment_index)(index& idx) const& noexcept -> void { ++idx; }
			STATIC_FINAL_MOD(constexpr, decrement_index)(index& idx) const& noexcept -> void { --idx; }
			STATIC_FINAL_MOD(constexpr, advance_index)(index& idx, difference_type d) const& noexcept -> void { idx += static_cast<index>(d); } /* static_cast suppresses C4308 warning (negative integral constant converted to unsigned) when an iterator is decremented in a constant expression */
			STATIC_FINAL_MOD(constexpr, distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> difference_type { return idxRhs - idxLhs; }
			STATIC_FINAL_MOD(constexpr, middle_point)( index & idxBegin, index const& idxEnd ) const& noexcept -> void {
				this->advance_index(idxBegin,this->distance_to_index(idxBegin,idxEnd)/2);
			}
			STATIC_FINAL_MOD(constexpr, dereference_index)(index idx) & noexcept -> T& { return this->dereference(idx); }
			STATIC_FINAL_MOD(constexpr, dereference_index)(index idx) const& noexcept -> T const& { return this->dereference(idx); }
		public:
			constexpr void clear() & noexcept {
				this->shrink(0);
			}

			template<typename Rng>
			constexpr void assign(Rng&& rng) & noexcept {
				clear();
				tc::append( *this, std::forward<Rng>(rng) );
			}

			constexpr void resize(tc::static_vector_size_t n) & noexcept(noexcept(this->emplace_back())) {
				if (this->size() < n) {
					do {
						this->emplace_back();
					} while (n != this->size());
				} else {
					this->shrink(n);
				}
			}

			template<typename It>
			constexpr void take_inplace( It const& it ) & noexcept {
				this->shrink(it.get_index());
			}

			template<typename It>
			constexpr void drop_inplace(It&& it) & noexcept {
				auto iSrc=it.get_index();
				_ASSERTE(iSrc<=this->m_iEnd);
				if (iSrc!=0) {
					index iDst=0;
					for (; iSrc!=this->m_iEnd; ++iDst, ++iSrc) {
						this->dereference(iDst)=tc_move_always(this->dereference(iSrc));
					}
					this->shrink(iDst);
				}
			}
		};
	} // static_vector_adl
	using static_vector_adl::static_vector;

	template<tc::static_vector_size_t N, typename Rng>
	auto make_static_vector(Rng&& rng) MAYTHROW {
		return tc::explicit_cast<tc::static_vector<tc::range_value_t<Rng>, N>>(std::forward<Rng>(rng));
	}

	template<tc::static_vector_size_t N, typename... Rng>
	auto make_static_vector(Rng&&... rng) MAYTHROW {
		return make_static_vector<N>(tc::concat(std::forward<Rng>(rng)...));
	}

	template< typename T, tc::static_vector_size_t N >
	struct range_filter_by_move_element<tc::static_vector<T,N>> : std::true_type {};

	template<typename Char>
	using codepoint = tc::static_vector<Char, tc::char_limits<Char>::c_nMaxCodeUnitsPerCodePoint>;
}

MODIFY_WARNINGS_END
