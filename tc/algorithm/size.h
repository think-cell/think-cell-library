
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
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
#include "../container/container_traits.h"

#include <boost/range/traversal.hpp>

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
			static_assert(tc::actual_integer<T>);
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
				: tc_member_init_cast( m_t, other.m_t )
			{
				AssertInvariant();
			}

			template< typename S >
			size_proxy& operator=(S&& s) & noexcept {
				tc::assign_explicit_cast(m_t,std::forward<S>(s));
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

	namespace detail {
		template<typename Rng, typename Required>
		concept has_traversal = tc::range_with_iterators<Rng> && std::convertible_to<typename boost::range_traversal<Rng>::type, Required>;
	}

	template<typename Rng>
	concept bidirectional_range = detail::has_traversal<Rng, boost::iterators::bidirectional_traversal_tag>;
	template<typename Rng>
	concept random_access_range = detail::has_traversal<Rng, boost::iterators::random_access_traversal_tag>;

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
		template<typename T0, typename T1, typename Enable=void>
		struct common_type_decayed_size_proxy_base {};

		template<typename T0, typename T1>
		struct common_type_decayed_size_proxy_base<T0, T1, tc::void_t<tc::common_type_decayed_t<T0, T1>>> {
			using type = tc::size_proxy<tc::common_type_decayed_t<T0,T1>>;
		};

		template<typename T0, typename T1>
		struct common_type_decayed<tc::size_proxy<T0>, tc::size_proxy<T1>>: common_type_decayed_size_proxy_base<T0, T1> {};

		template<typename T0, typename T1>
		struct common_type_decayed<tc::size_proxy<T0>, T1> {
			using type = T1;
		};

		template<typename T0, typename T1>
		struct common_type_decayed<T0, tc::size_proxy<T1>> : tc::common_type_decayed<tc::size_proxy<T1>, T0> {};
	}

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
				return [&]() return_decltype_noexcept(constexpr_size<Rng>::value);
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
}

