
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/explicit_cast.h"
#include "../base/integer.h"
#include "../base/type_traits.h"
#include "../base/generic_macros.h"
#include "../base/tag_type.h"
#include "../base/renew.h"
#include "../container/container_traits.h"
#include "../range/meta.h"

#include <limits>

namespace tc {
	// forward definitions
	namespace size_proxy_adl {
		template< typename T > struct size_proxy;
	}
	using size_proxy_adl::size_proxy;

	template< typename T >
	[[nodiscard]] constexpr auto make_size_proxy(T t) noexcept;

	////////////////////////////////
	// tc::size_proxy in ADL barrier
	namespace size_proxy_adl {
		template< typename T >
		struct size_proxy final {
			static_assert(tc::actual_integer<T> && tc::decayed<T>);
		private:
			constexpr void AssertInvariant() & noexcept {
				// npos should only be represented by a signed number. Otherwise casts to (larger) unsigned types may not preserve npos as static_cast<T>(-1)
				_ASSERTE( static_cast<T>(-1)==m_t ? std::is_signed<T>::value : 0<=m_t );
			}
		public:
			T m_t;
			constexpr size_proxy() noexcept : m_t() {
				AssertInvariant();
			}
			constexpr explicit size_proxy(T t) noexcept : m_t(t) {
				AssertInvariant();
			}

			template<typename S>
			constexpr explicit size_proxy(size_proxy<S> const& other) noexcept
				: tc_member_init( m_t, other.m_t )
			{
				AssertInvariant();
			}

			template< typename S >
			size_proxy& operator=(S&& s) & noexcept {
				tc::assign_explicit_cast(m_t,tc_move_if_owned(s));
				AssertInvariant();
				return *this;
			}

#pragma push_macro("CONVERT")
#define CONVERT( S ) \
			constexpr operator S() const& noexcept { \
				_ASSERTE(  /*good idea to require signed?*/ std::is_signed<S>::value && static_cast<T>(-1)==m_t || tc::as_unsigned(m_t)<=tc::as_unsigned(std::numeric_limits<S>::max()) ); \
				return static_cast<S>(m_t); \
			}
			
			CONVERT(signed char)
			CONVERT(unsigned char)
			CONVERT(short)
			CONVERT(unsigned short)
			CONVERT(int)
			CONVERT(unsigned int)
			CONVERT(long)
			CONVERT(unsigned long)
			CONVERT(long long)
			CONVERT(unsigned long long)
#pragma pop_macro("CONVERT")
			operator double() const& noexcept {
				_ASSERT(0 <= m_t || static_cast<T>(-1) == m_t);
				tc_return_cast(m_t);
			}

			template< typename U >
			operator size_proxy<U>() const& noexcept {
				return size_proxy<U>(tc::implicit_cast<U>(*this));
			}

#pragma push_macro("COMPARISON_OPERATOR")
#define COMPARISON_OPERATOR( return_type, op ) \
			template< tc::actual_integer S > \
			friend constexpr return_type operator op( size_proxy const& lhs, S const& rhs ) noexcept { \
				return tc::prepare_argument< T,S >(lhs.m_t) op tc::prepare_argument< T,S >(rhs); \
			} \
			template< typename S > \
			friend constexpr return_type operator op( size_proxy const& lhs, size_proxy<S> const& rhs ) noexcept { \
				return lhs op rhs.m_t; \
			}

			COMPARISON_OPERATOR(bool, ==) // Xcode13 doesn't compile with auto even though static_assert says the return type is bool
			COMPARISON_OPERATOR(auto, <=>)
#pragma pop_macro("COMPARISON_OPERATOR")
		};

#pragma push_macro("operator_size_proxy")
#define operator_size_proxy( op ) \
		template< \
			typename Lhs, tc::actual_integer Rhs \
		> constexpr auto operator op( size_proxy<Lhs> const& lhs, Rhs const& rhs ) \
		return_decltype_MAYTHROW( \
			make_size_proxy( lhs.m_t op rhs ) \
		) \
		template< \
			tc::actual_arithmetic Lhs, typename Rhs \
		> constexpr auto operator op( Lhs const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype_MAYTHROW( \
			tc::explicit_cast<Lhs>(lhs op rhs.m_t) \
		) \
		template< \
			typename Lhs, typename Rhs, \
			std::enable_if_t<!tc::actual_arithmetic<Lhs>>* = nullptr \
		> constexpr auto operator op( Lhs const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype_MAYTHROW( \
			lhs op rhs.m_t \
		) \
		template< typename Lhs, typename Rhs > constexpr auto operator op( size_proxy<Lhs> const& lhs, size_proxy<Rhs> const& rhs ) \
		return_decltype_MAYTHROW( \
			make_size_proxy( lhs.m_t op rhs.m_t ) \
		)
		
		operator_size_proxy(+)
		operator_size_proxy(-)
		operator_size_proxy(*)
		operator_size_proxy(/ )
		operator_size_proxy(%)
#pragma pop_macro("operator_size_proxy")
		
#pragma push_macro("assign_operator_size_proxy")
#define assign_operator_size_proxy( assign_op, op ) \
		template< typename Lhs, typename Rhs> \
		constexpr Lhs& operator assign_op ( Lhs& lhs, size_proxy<Rhs> const& rhs ) noexcept { \
			if constexpr( std::integral<Lhs> ) { \
				lhs=tc:: op (tc::as_const(lhs),rhs.m_t); \
			} else { \
				lhs assign_op rhs.m_t; \
			} \
			return lhs; \
		}

		assign_operator_size_proxy(+=,add)
		assign_operator_size_proxy(-=,sub)
#pragma pop_macro("assign_operator_size_proxy")

		template< tc::actual_integer Lhs, typename Rhs >
		Lhs& operator %= ( Lhs& lhs, size_proxy<Rhs> const& rhs ) noexcept {
			lhs%=rhs.m_t;
			return lhs;
		}
	}

	template< typename T >
	[[nodiscard]] constexpr auto make_size_proxy(T t) noexcept {
		if constexpr( tc::actual_integer<T> ) {
			return size_proxy<T>(t);
		} else {
			static_assert( tc::instance<T, size_proxy> );
			return t;
		}
	}

	template< typename T >
	[[nodiscard]] constexpr auto unmake_size_proxy(T t) noexcept {
		if constexpr( tc::instance<T, size_proxy> ) {
			return t.m_t;
		} else {
			static_assert( tc::actual_integer<T> );
			return t;
		}
	}

	namespace no_adl {
		template <typename T0, typename T1>
		using size_proxy_common_type = tc::size_proxy<tc::common_type_t<T0, T1>>;

		template <tc::actual_integer T0, tc::actual_integer T1>
		struct common_type_decayed_impl<tc::size_proxy<T0>, tc::size_proxy<T1>> : boost::mp11::mp_defer<size_proxy_common_type, T0, T1> {};

		template <tc::actual_integer T0, tc::actual_integer T1>
		struct common_type_decayed_impl<tc::size_proxy<T0>, T1> {
			using type = T1;
		};

		template <tc::actual_integer T0, tc::actual_integer T1>
		struct common_type_decayed_impl<T0, tc::size_proxy<T1>> {
			using type = T0;
		};
	}

	////////////////////////////////
	// tc::constexpr_size
	namespace no_adl {
		template<typename Rng>
		struct constexpr_size_impl;

		// Rng has a size function that returns an integral_constant.
		template <typename Rng> requires requires { decltype(std::declval<Rng&>().size())::value; }
		struct constexpr_size_impl<Rng> : decltype(std::declval<Rng&>().size()) {};

		template<typename T, std::size_t N>
		struct constexpr_size_impl<T[N]> : tc::least_uint_constant<N - (tc::char_type<T> ? 1 : 0)> {};

		template<typename T, T... t>
		struct constexpr_size_impl<std::integer_sequence<T, t...>> : tc::least_uint_constant<sizeof...(t)> {};
	}

	template <typename Rng> requires has_constexpr_size<Rng>
	constexpr auto constexpr_size = []() noexcept {
		using type = no_adl::constexpr_size_impl<std::remove_cvref_t<Rng>>;
		static_assert(std::derived_from<type, tc::least_uint_constant<type::value>>);
		return tc::least_uint_constant<type::value>{};
	}();

	////////////////////////////////
	// tc::size
	namespace size_raw_internal {
		// tc::size() requires a range with either:
		//  - a constexpr_size_impl specialization which provide size as a compile time constant
		//  - a size member function, which is assumed to run in O(1)
		//  - random_access iterators, which are assumed to be able to calculate size in O(1)
		// TODO: inline when clang supports lambdas in unevaluated contexts
		template<typename Rng> requires
			has_constexpr_size<Rng> || has_mem_fn_size<Rng> || (random_access_range<Rng&> && tc::common_range<Rng>)
		constexpr auto size_raw(Rng&& rng) noexcept {
			if constexpr( has_constexpr_size<Rng> ) {
				return [&]() return_decltype_noexcept(constexpr_size<Rng>());
			} else if constexpr( has_mem_fn_size<Rng> ) {
				return [&]() return_MAYTHROW(tc_move_if_owned(rng).size()); // .size() may throw for files
			} else {
				// Do not use boost::size. It always uses std::distance, which is O(n) for
				// ranges with boost::iterators::random_access_traversal_tag but not std::random_access_iterator_tag,
				// e.g., boost::transform_iterator
				return [&]() return_MAYTHROW(tc::as_unsigned(tc::end(rng) - tc::begin(rng)));
			}
		}
	}

	template<typename Rng>
	[[nodiscard]] constexpr auto size_raw(Rng&& rng) return_decltype_MAYTHROW(
		size_raw_internal::size_raw(tc_move_if_owned(rng))()
	)

	// Note: This overload is only necessary for the assertion - the size is otherwise computed the same by constexpr_size.
	template<tc::char_type T, std::size_t N>
	[[nodiscard]] constexpr auto size_raw(T const (&ach)[N]) noexcept {
		_ASSERTE(tc::strlen(ach)==N-1); // VERIFYEQUAL is not constexpr
		return N-1;
	}

	template<typename T>
	[[nodiscard]] constexpr auto size(T&& t) return_decltype_MAYTHROW(
		make_size_proxy(tc::size_raw(tc_move_if_owned(t)))
	)

	TC_HAS_EXPR(size, (T), size_raw(std::declval<T>()))
	DEFINE_FN2(tc::size_raw, fn_size_raw)

	template <auto Fn, typename ... Rng>
	[[nodiscard]] constexpr auto compute_range_adaptor_size(Rng&&... rng) MAYTHROW {
		if constexpr ((tc::has_constexpr_size<Rng> && ...)) {
			tc::actual_unsigned_integer auto constexpr value = Fn(tc::constexpr_size<Rng>()...);
			return tc::least_uint_constant<value>{};
		} else {
			tc::actual_unsigned_integer auto const value = Fn(tc::size_raw(tc_move_if_owned(rng))...);
			return value;
		}
	}
}

