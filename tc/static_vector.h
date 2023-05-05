
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

// #include "index_iterator.h"
#include "storage_for.h"
#include "base/renew.h"
#include "base/tag_type.h"
#include "algorithm/filter_inplace.h"
#include "algorithm/append.h"

#include <boost/preprocessor/enum.hpp>
#include <boost/container/container_fwd.hpp>
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
			[[nodiscard]] constexpr bool full() const& noexcept {
				return N == m_iEnd;
			}
			[[nodiscard]] constexpr size_type size() const& noexcept {
				return m_iEnd;
			}
			[[nodiscard]] static constexpr size_type capacity() noexcept {
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
			constexpr T& emplace_back(Args&& ... args) & noexcept(noexcept(m_aot[this->m_iEnd - 1].ctor_value(std::forward<Args>(args)...))) {
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

			[[nodiscard]] constexpr T* data() & noexcept {
				return m_aot[0].uninitialized_addressof();
			}
			[[nodiscard]] constexpr T const* data() const& noexcept {
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
			struct SValueInitIfConstexpr final {
				// Leave array uninitialized at runtime
				SValueInitIfConstexpr() noexcept {}
				// All slots in constexpr static_vector must be initialized (for clang and if materialized as a static variable). 
				consteval SValueInitIfConstexpr(constexpr_tag_t) : m_at{} {} // Value init implies zero init because of is_trivially_default_constructible.
				T m_at[N];
			} m_a;
		protected:
			constexpr T const& dereference(size_type n) const& noexcept { return m_a.m_at[n]; }
			constexpr T& dereference(size_type n) & noexcept { return m_a.m_at[n]; }
		public:
			constexpr static_vector_base_trivial() noexcept
				: m_a([]() noexcept {
					if( std::is_constant_evaluated() ) {
						return SValueInitIfConstexpr(constexpr_tag);
					} else {
						return SValueInitIfConstexpr();
					}
				}())
			{}

			// This specialization for trivially-assignable types is usable in constant expressions.
			// The normal version of emplace_back cannot be used in constant expressions, because it uses placement new.
			template<typename... Args>
			constexpr typename std::enable_if<std::is_trivially_assignable<T&, T>::value && noexcept(T(std::declval<Args&&>()...)), T&>::type emplace_back(Args&& ... args) & noexcept {
				_ASSERTE(!this->full());
				T& t = m_a.m_at[this->m_iEnd];
				++this->m_iEnd;
				t = T(std::forward<Args>(args)...);
				return t;
			}

			template<typename... Args>
			typename std::enable_if < !std::is_trivially_assignable<T&, T>::value || !noexcept(T(std::declval<Args&&>()...)), T&>::type emplace_back(Args&& ... args) & noexcept(noexcept(T(std::forward<Args>(args)...))) {
				_ASSERTE(!this->full());
				T& t = m_a.m_at[this->m_iEnd];
				++this->m_iEnd;
				// Inside element ctors, the element is already in the container.
				try {
					tc::ctor(t, std::forward<Args>(args)...); // MAYTHROW
					return t;
				} catch (...) {
					--this->m_iEnd;
					throw;
				}
			}

			[[nodiscard]] constexpr T* data() & noexcept {
				return std::addressof(m_a.m_at[0]);
			}
			[[nodiscard]] constexpr T const* data() const& noexcept {
				return std::addressof(m_a.m_at[0]);
			}
		};

		template<typename T, tc::static_vector_size_t N>
		struct static_vector_base_nontrivial_dtor : static_vector_base_nontrivial<T, N> {
			constexpr static_vector_base_nontrivial_dtor() = default;
			using static_vector_base_nontrivial<T, N>::static_vector_base_nontrivial;

			constexpr ~static_vector_base_nontrivial_dtor() {
				shrink(0);
			}

			constexpr void pop_back() & noexcept {
				_ASSERTE( 0 < this->m_iEnd );
				--this->m_iEnd;
				// Inside element dtors, the element is already removed from the container.
				this->m_aot[this->m_iEnd].dtor();
			}

		protected:
			constexpr void shrink(tc::static_vector_size_t n) noexcept {
				_ASSERTE( n <= this->size() );
				while (n < this->size()) {
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
			constexpr static_vector_base_trivial_dtor() = default;
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
			using typename this_type::range_iterator_from_index::tc_index;

			using difference_type = std::make_signed_t<tc_index>;
			using reference = T&;
			using value_type = T;

			static constexpr bool c_bHasStashingIndex=false;

			STATICASSERTEQUAL( std::is_trivially_destructible<base>::value, std::is_trivially_destructible<T>::value );

			constexpr static_vector() noexcept {}

			template <typename... Args> requires
				(0 < sizeof...(Args)) &&
				(tc::econstructionIMPLICIT==tc::elementwise_construction_restrictiveness<T, Args...>::value)
			constexpr static_vector(tc::aggregate_tag_t, Args&& ... args) noexcept(std::conjunction<std::is_nothrow_constructible<T, Args&&>...>::value) {
				static_assert(sizeof...(Args)<=N, "vector initializer list contains too many elements");
				(this->emplace_back(std::forward<Args>(args)), ...);
			}

			template <typename... Args> requires
				(0 == sizeof...(Args)) ||
				(tc::econstructionEXPLICIT==tc::elementwise_construction_restrictiveness<T, Args...>::value)
			constexpr explicit static_vector(tc::aggregate_tag_t, Args&& ... args) MAYTHROW {
				static_assert(sizeof...(Args)<=N, "vector initializer list contains too many elements");
				(tc::cont_emplace_back(*this, std::forward<Args>(args)), ...); // cont_emplace_back for lazy explicit_cast
			}

			constexpr static_vector(static_vector const& vec) noexcept(std::is_nothrow_copy_constructible<T>::value) {
				tc::append(*this, vec);
			}

			constexpr static_vector(static_vector&& vec) noexcept(std::is_nothrow_move_constructible<T>::value) {
				tc::append(*this, tc_move(vec));
			}

			constexpr static_vector& operator=(static_vector const& vec) & noexcept(std::is_nothrow_copy_assignable<T>::value) {
				if( std::addressof(vec)!=this ) {
					assign(vec);
				}
				return *this;
			}

			constexpr static_vector& operator=(static_vector&& vec) & noexcept(std::is_nothrow_move_assignable<T>::value) {
				_ASSERTE( std::addressof(vec)!=this ); // self assignment from rvalues should not happen, rvalues must be expiring
				NOEXCEPT( assign( tc_move(vec) ) );
				return *this;
			}
		private:
			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index { return 0; }
			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index { return this->m_iEnd; }
			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void { ++idx; }
			STATIC_FINAL_MOD(constexpr, decrement_index)(tc_index& idx) const& noexcept -> void { --idx; }
			STATIC_FINAL_MOD(constexpr, advance_index)(tc_index& idx, difference_type d) const& noexcept -> void { idx += static_cast<tc_index>(d); } /* static_cast suppresses C4308 warning (negative integral constant converted to unsigned) when an iterator is decremented in a constant expression */
			STATIC_FINAL_MOD(constexpr, distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept -> difference_type { return idxRhs - idxLhs; }
			STATIC_FINAL_MOD(constexpr, middle_point)( tc_index & idxBegin, tc_index const& idxEnd ) const& noexcept -> void {
				this->advance_index(idxBegin,this->distance_to_index(idxBegin,idxEnd)/2);
			}
			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index idx) & noexcept -> T& { return this->dereference(idx); }
			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index idx) const& noexcept -> T const& { return this->dereference(idx); }
			STATIC_FINAL_MOD(constexpr, index_to_address)(const tc_index& idx)& noexcept ->  T* { return this->data() + idx; }
			STATIC_FINAL_MOD(constexpr, index_to_address)(const tc_index& idx) const& noexcept ->  const T* { return this->data() + idx; }
		public:
			constexpr void clear() & noexcept {
				this->shrink(0);
			}

			template<typename Rng>
			constexpr void assign(Rng&& rng) & noexcept {
				clear();
				tc::append( *this, std::forward<Rng>(rng) );
			}

			constexpr void resize(tc::static_vector_size_t const n, boost::container::default_init_t) & noexcept {
				if (this->size() < n) {
					static_assert(std::is_trivially_default_constructible<T>::value);
					this->m_iEnd = n;
				} else {
					this->shrink(n);
				}
			}

			constexpr void resize(tc::static_vector_size_t const n) & noexcept(noexcept(this->emplace_back())) {
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
					tc_index iDst=0;
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
	constexpr auto make_static_vector(Rng&& rng) MAYTHROW {
		return tc::explicit_cast<tc::static_vector<tc::range_value_t<Rng>, N>>(std::forward<Rng>(rng));
	}

	template<tc::static_vector_size_t N, typename... Rng>
	constexpr auto make_static_vector(Rng&&... rng) MAYTHROW {
		return make_static_vector<N>(tc::concat(std::forward<Rng>(rng)...));
	}

	template< typename T, tc::static_vector_size_t N >
	struct range_filter_by_move_element<tc::static_vector<T,N>> : tc::constant<true> {};

	template<typename Char>
	using codepoint = tc::static_vector<Char, tc::char_limits<Char>::c_nMaxCodeUnitsPerCodePoint>;
}

MODIFY_WARNINGS_END
