
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "casts.h"
#include "../range/subrange.h"
#include "../array.h"
#include <boost/range/algorithm/copy.hpp>

//-----------------------------------------------------------------------------------------------------------------------------

namespace tc {

	//--------------------------------------------------------------------------------------------------------------------------
	// as_blob
	// reinterprets a range of items as a range of bytes

	// We use unsigned char for uninterpreted memory.
	// - The type used must support aliasing, so the only candidates are char, signed char and unsigned char.
	// - char is bad because we interpret char as UTF-8 and char* as zero-terminated range.
	// - unsigned char is better than signed char because the binary representation of signs may vary between platforms.
	// - char is bad because it is either signed or unsigned, so it has the same problem.
	// - unsigned char is better than std::uint8_t because the latter must be 8 bit, but we mean the smallest addressable unit, which is char and may be larger (or smaller?) on other platforms.

	static_assert(!tc::range_with_iterators< void const* >);

	template<tc::contiguous_range Rng>
	[[nodiscard]] auto range_as_blob(Rng&& rng) noexcept {
		using cv_value_type = std::remove_pointer_t<decltype(tc::ptr_begin(rng))>;
		static_assert( std::is_trivially_copyable< cv_value_type >::value, "as_blob only works on std::is_trivially_copyable types" );
		if constexpr(tc::safely_constructible_from<tc::span<cv_value_type>, Rng>) {
			return tc::make_iterator_range(
				reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>(tc::ptr_begin(rng)),
				reinterpret_cast<same_cvref_t<unsigned char, cv_value_type>*>(tc::ptr_end(rng))
			);
		} else {
			// not inside blob_range_t because templates cannot be declared inside of a local class
			static auto constexpr as_blob_ptr=[](auto ptr) noexcept {
				return reinterpret_cast<same_cvref_t<unsigned char, std::remove_pointer_t<decltype(ptr)>>*>(ptr);
			};
			struct blob_range_t final {
				explicit blob_range_t(Rng&& rng) noexcept : m_rng(tc_move(rng)) {}
				auto begin() & noexcept { return as_blob_ptr(tc::ptr_begin(m_rng)); }
				auto begin() const& noexcept { return as_blob_ptr(tc::ptr_begin(m_rng)); }
				auto end() & noexcept { return as_blob_ptr(tc::ptr_end(m_rng)); }
				auto end() const& noexcept { return as_blob_ptr(tc::ptr_end(m_rng)); }
			private:
				static_assert(!std::is_reference<Rng>::value);
				std::remove_cv_t<Rng> m_rng;
			};
			return blob_range_t(tc_move(rng));
		}
	}

	namespace no_adl {
		template<typename Sink>
		struct range_as_blob_sink { // no final: verify_sink_result_impl derives
		private:
			// range_as_blob_sink is only used inline in range_as_blob below, and m_sink is only passed to tc::for_each, so holding by lvalue reference ok
			Sink& m_sink;

		public:
			explicit range_as_blob_sink(Sink& sink) noexcept: m_sink(sink) {}

			// chunk must be defined before operator() - otherwise MSVC will not allow it to occur in the return type of operator()
			template<tc::contiguous_range Rng>
			auto chunk(Rng&& rng) const& return_decltype_MAYTHROW (
				tc::for_each(tc::range_as_blob(tc_move_if_owned(rng)), m_sink)
			)

			template<typename T>
			auto operator()(T&& t) const& return_decltype_MAYTHROW (
				chunk(tc::single(/* no tc_move_if_owned */ t))
			)
		};
	}

	template<typename Rng>
	[[nodiscard]] auto range_as_blob(Rng&& rng) noexcept {
		return [rng=tc::make_reference_or_value(tc_move_if_owned(rng))](auto&& sink) MAYTHROW {
			return tc::for_each(*rng, no_adl::range_as_blob_sink<std::remove_reference_t<decltype(sink)>>(sink));
		};
	}

	template<typename T> requires std::is_trivially_copyable<std::remove_reference_t<T>>::value
	[[nodiscard]] auto as_blob(T&& t) noexcept {
		return tc::range_as_blob( tc::single( tc_move_if_owned(t) ) );
	}

	namespace assert_no_overlap_impl {
		void assert_no_overlap(auto const& lhs, auto const& rhs) noexcept {
			_ASSERT(
				reinterpret_cast<std::size_t>(tc::ptr_end(lhs)) <= reinterpret_cast<std::size_t>(tc::ptr_begin(rhs)) ||
				reinterpret_cast<std::size_t>(tc::ptr_end(rhs)) <= reinterpret_cast<std::size_t>(tc::ptr_begin(lhs))
			);
		}
	}
	template< typename Lhs, typename Rhs>
	void assert_no_overlap(Lhs const& lhs, Rhs const& rhs) noexcept {
		assert_no_overlap_impl::assert_no_overlap(tc::single(lhs), tc::single(rhs));
		if constexpr( tc::contiguous_range<Lhs> && tc::contiguous_range<Rhs> ) {
			assert_no_overlap_impl::assert_no_overlap(lhs, rhs);
		}
	}

	/////////////////////////////////////////////
	// bit_cast

	namespace no_adl {
		struct any_ptr_ref final: tc::nonmovable {
		private:
			void* m_pv;
		public:
			explicit any_ptr_ref(void* pv) noexcept: m_pv(pv) {}

			template<typename T> requires std::same_as<std::remove_cvref_t<T>, T> && std::is_trivially_copyable<T>::value
			operator T() && noexcept {
				T t;
				std::memcpy(std::addressof(t), m_pv, sizeof(t));
				return t;
			}
		};
	}

	namespace any_ptr_adl {
		struct any_ptr final {
		private:
			void* m_pv;
		public:
			any_ptr( void* pv ) noexcept
			:	m_pv(pv)
			{}

			explicit operator bool() const& noexcept {
				return m_pv;
			}

			auto operator*() const& noexcept {
				return tc::no_adl::any_ptr_ref(m_pv);
			}

			template<typename T> requires std::is_pointer<T>::value || std::is_member_pointer<T>::value
			operator T() const& noexcept {
				STATICASSERTEQUAL(sizeof(T), sizeof(void*));
				T t;
				std::memcpy( std::addressof(t), std::addressof(m_pv), sizeof(t) ); // bit_cast to allow cast to member function pointers
				return t;
			}
		};
	}
	using any_ptr_adl::any_ptr;

	namespace no_adl {
		template <typename T>
		struct type {
		private:
			same_cvref_t<unsigned char,T>* m_pb;
		public:
			explicit type(same_cvref_t<unsigned char,T>* pb) noexcept
			: m_pb(pb)
			{}
			explicit type(tc::any_ptr p) noexcept
			: m_pb(p)
			{}
			operator std::remove_cv_t<T>() const& noexcept {
				std::remove_cv_t<T> t;
				std::memcpy( std::addressof(t), m_pb, sizeof(t) );
				return t;
			}
			type const& operator=( std::remove_cv_t<T> const& rhs ) const& noexcept {
				boost::copy( tc::as_blob(rhs), m_pb );
				return *this;
			}
		};

		template <typename T, bool bPreventSlicing>
		struct decay<no_adl::type<T>, bPreventSlicing> {
			using type=typename tc::decay<T, bPreventSlicing>::type; // recursive
		};
	}

	template<typename T>
	struct aliasing_ref final {
		static_assert( !std::is_reference<T>::value );
		static_assert( std::is_trivially_copyable<T>::value );
		using type=no_adl::type<T>;
		static type construct(same_cvref_t<unsigned char,T>* pb) noexcept {
			return type(pb);
		}
	};

	template<typename T>
	struct aliasing_ref<T const> final {
		static_assert( !std::is_reference<T>::value );
		static_assert( std::is_trivially_copyable<T>::value );
		using type = std::remove_cv_t<T>;
		static type construct(same_cvref_t<unsigned char,T const>* pb) noexcept {
			type t;
			std::memcpy( std::addressof(t), pb, sizeof(t) );
			return t;
		}
	};

	template< typename T>
	struct aliasing_ptr final {
		static_assert( !std::is_reference<T>::value );
		static_assert( std::is_trivially_copyable<T>::value );
		struct type {
		private:
			same_cvref_t<unsigned char,T>* m_pb;
		public:
			type() noexcept {}
			explicit type(T* pt) noexcept
			: m_pb(reinterpret_cast<same_cvref_t<unsigned char,T>*>(pt))
			{}
			explicit operator bool() const& noexcept {
				return m_pb;
			}
			type& operator=(std::nullptr_t) & noexcept {
				m_pb=nullptr;
				return *this;
			}
			typename aliasing_ref<T>::type operator*() const& noexcept {
				return aliasing_ref<T>::construct(m_pb);
			}
			type& operator+=( std::ptrdiff_t n ) & noexcept {
				m_pb+=n*static_cast<std::ptrdiff_t>(sizeof(T)); // cast to signed ptrdiff_t to silence UB sanitizer
				return *this;
			}
			type& operator-=( std::ptrdiff_t n ) & noexcept {
				return *this+=-n;
			}
			template< typename Offset > friend type operator+( type ptr, Offset n ) noexcept {
				return ptr+=n;
			}
			template< typename Offset > friend type operator-( type ptr, Offset n ) noexcept {
				return ptr-=n;
			}
			typename aliasing_ref<T>::type operator[]( std::ptrdiff_t n ) const& noexcept {
				return *(*this+n);
			}
		};
	};

	template< typename T > requires std::is_function<T>::value
	struct aliasing_ptr<T> final {
		using type = T*;
	};

	template<>
	struct aliasing_ptr<char> final {
		using type = char*;
	};

	template<>
	struct aliasing_ptr<char const> final {
		using type = char const*;
	};

	template<>
	struct aliasing_ptr<unsigned char> final {
		using type = unsigned char*;
	};

	template<>
	struct aliasing_ptr<unsigned char const> final {
		using type = unsigned char const*;
	};

	template<typename Dst, tc::contiguous_range Src>
	[[nodiscard]] Dst bit_cast_range(Src const& src) noexcept {
		tc_auto_cref(rng, tc::range_as_blob(src));
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		_ASSERTEQUAL(tc::size(rng), sizeof(Dst));
		static_assert(std::is_trivially_copyable< Dst >::value);
		Dst dst;
		std::memcpy(std::addressof(dst), tc::ptr_begin(rng), sizeof(dst));
		return dst;
	}

	// no danger of aliasing
	template<typename Dst, typename Src>
	[[nodiscard]] constexpr Dst bit_cast(Src const& src) noexcept {
		static_assert( !std::is_pointer<Src>::value || !std::is_pointer<Dst>::value );
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst );
		STATICASSERTEQUAL(sizeof(Dst), sizeof(Src), "bit_cast source and destination must be same size");
		static_assert(std::is_trivially_copyable<Dst>::value && std::is_trivially_copyable<Src>::value);
		// Visual Studio use __builtin_bit_cast to implement std::bit_cast.
		// clang has __builtin_bit_cast but does not support std::bit_cast (Xcode 13).
		return __builtin_bit_cast(Dst, src);
	}

	// danger of aliasing
	template<typename Dst, typename Src>
	[[nodiscard]] typename aliasing_ptr<std::remove_pointer_t<Dst>>::type aliasing_cast(Src const& src) noexcept {
		static_assert( std::is_pointer<Src>::value );
		static_assert( std::is_pointer<Dst>::value );
		STATICASSERTSAME(std::remove_cvref_t<Dst>, Dst);
		return typename aliasing_ptr<std::remove_pointer_t<Dst>>::type(reinterpret_cast<Dst>(src));
	}
}
