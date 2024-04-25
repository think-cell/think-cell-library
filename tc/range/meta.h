
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/type_traits_fwd.h"
#include "../base/functors.h"
#include "../base/tag_type.h"
#include "../base/casts.h"
#include "../container/string.h"
#include "../tuple.h"

#include <boost/range/has_range_iterator.hpp>
#include <type_traits>
#include <iterator>
#include <ranges>
#include <boost/range/traversal.hpp>

namespace tc {
	namespace begin_end_adl {
		DEFINE_ADL_TAG_TYPE(begin_end_tag);
	}

	template <typename Rng>
	concept borrowed_range_impl = (std::is_lvalue_reference<Rng>::value || std::ranges::enable_borrowed_range<std::remove_cvref_t<Rng>>);

	template<tc::borrowed_range_impl Rng>
	[[nodiscard]] constexpr auto begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member begin
		rng.begin()
	)

	template<tc::borrowed_range_impl Rng>
	[[nodiscard]] constexpr auto begin(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free begin found by tag
		begin(begin_end_adl::begin_end_tag, rng)
	)

	template<tc::borrowed_range_impl Rng>
	[[nodiscard]] constexpr auto end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has member end
		rng.end()
	)

	template<tc::borrowed_range_impl Rng>
	[[nodiscard]] constexpr auto end(Rng&& rng) return_decltype_MAYTHROW(
		// Rng has free end found by tag
		end(begin_end_adl::begin_end_tag, rng)
	)

	template<typename Rng>
	using iterator_t = decltype(tc::begin(std::declval<Rng&>()));
	template<typename Rng>
	using sentinel_t = decltype(tc::end(std::declval<Rng&>()));

	template<typename Rng>
	concept range_with_iterators = requires {
		typename tc::iterator_t<Rng>;
		typename tc::sentinel_t<Rng>;
	};

	template<typename Rng>
	concept common_range = tc::range_with_iterators<Rng> && std::same_as<tc::iterator_t<Rng>, tc::sentinel_t<Rng>>;

	// `Rng` models `tc::borrowed_range` iff the validity of iterators of `Rng` is not tied to the lifetime of the `Rng` object:
	// * references to containers (not containers themselves)
	// * tc::span
	// * ...
	template<typename Rng>
	concept borrowed_range = tc::range_with_iterators<Rng> && tc::borrowed_range_impl<Rng>;

	// For convenience, you can specialize `tc::enable_borrowed_range` instead of `std::ranges::enable_borrowed_range` which doesn't require opening up a separate namespace.
	// (There is a specialization of `std::ranges::enable_borrowed_range` at the end of the file.)
	template<typename Rng>
	constexpr auto enable_borrowed_range = nullptr; // primary is "disabled"

	namespace detail {
		template<typename Rng, typename Required>
		concept has_traversal = tc::range_with_iterators<Rng> && std::convertible_to<typename boost::range_traversal<Rng>::type, Required>;
	}

	template<typename Rng>
	concept bidirectional_range = detail::has_traversal<Rng, boost::iterators::bidirectional_traversal_tag>;
	template<typename Rng>
	concept random_access_range = detail::has_traversal<Rng, boost::iterators::random_access_traversal_tag>;

	// Note that we cannot use std::contiguous_iterator yet as it is broken on our version of libcxx.
	// So instead we just check the iterator category as a proxy.
#ifdef _LIBCPP_VERSION
	namespace contiguous_range_detail {
		template <typename It>
		constexpr auto compute_iterator_concept() {
			using traits = std::iterator_traits<It>;

			// This algorithm isn't precisely the one specified for ITER_CONCEPT in the standard,
			// but that requires detecting whether or not traits have been specialized, which a non-standard library cannot do.
			// So instead we approximate it.
			if constexpr (requires { typename It::iterator_concept; }) {
				return typename It::iterator_concept();
			} else if constexpr (requires { typename traits::iterator_concept; }) {
				return typename traits::iterator_concept();
			} else if constexpr (requires { typename It::iterator_category; }) {
				return typename It::iterator_category();
			} else {
				return typename traits::iterator_category();
			}
		}
		template <typename It>
		using iterator_concept = decltype(compute_iterator_concept<It>());

		template <typename It>
		concept contiguous_iterator = std::is_pointer_v<It> || std::is_base_of_v<std::contiguous_iterator_tag, iterator_concept<It>>;
	}
	template <typename Rng>
	concept contiguous_range = tc::range_with_iterators<Rng> && contiguous_range_detail::contiguous_iterator<tc::iterator_t<Rng>>;
#else
	template <typename Rng>
	concept contiguous_range = tc::range_with_iterators<Rng> && std::contiguous_iterator<tc::iterator_t<Rng>>;
#endif

	template <typename Rng>
	concept prefers_for_each = !tc::range_with_iterators<Rng> || Rng::c_bPrefersForEach;

	template <typename ... Rng>
	concept ranges_with_common_reference
		= (... && tc::range_with_iterators<Rng>) && requires { typename tc::common_reference_t<std::iter_reference_t<tc::iterator_t<Rng>>...>; };

	namespace range_output_t_adl {
		DEFINE_ADL_TAG_TYPE(adl_tag);

		template<typename T, T... t>
		auto range_output_t_impl(adl_tag_t, std::integer_sequence<T, t...> const&) -> boost::mp11::mp_list<T>; // declaration only
	}

	namespace range_output_no_adl {
		template<typename Rng>
		struct from_iter_reference {};

		template<typename Rng> requires (!std::is_void<std::iter_reference_t<tc::iterator_t<Rng>>>::value)
		struct from_iter_reference<Rng> {
			using type = boost::mp11::mp_list<tc::remove_rvalue_reference_t<std::iter_reference_t<tc::iterator_t<Rng>>>>;
		};

		template<typename Rng>
		struct from_adl_with_tag : from_iter_reference<Rng> {};

		template<typename Rng> requires requires { /*adl*/range_output_t_impl(tc::range_output_t_adl::adl_tag, std::declval<Rng>()); }
		struct from_adl_with_tag<Rng> {
			using type = decltype(/*adl*/range_output_t_impl(tc::range_output_t_adl::adl_tag, std::declval<Rng>()));
			static_assert(tc::decayed<type> && tc::instance<type, boost::mp11::mp_list>);
			static_assert(!boost::mp11::mp_any_of<type, std::is_rvalue_reference>::value);
		};

		template<typename Rng>
		struct from_adl final : from_adl_with_tag<Rng> {};

		template<typename Rng> requires requires { /*adl*/range_output_t_impl(std::declval<Rng>()); }
		struct from_adl<Rng> {
			using type = decltype(/*adl*/range_output_t_impl(std::declval<Rng>()));
			static_assert(tc::decayed<type> && tc::instance<type, boost::mp11::mp_list>);
			static_assert(!boost::mp11::mp_any_of<type, std::is_rvalue_reference>::value);
		};
	}
	template<typename Rng>
	using range_output_t = typename range_output_no_adl::from_adl<Rng>::type;

	namespace make_lazy_adl {
		template<typename Func>
		auto range_output_t_impl(make_lazy<Func> const&) noexcept -> tc::range_output_t<decltype(std::declval<Func const&>()())>; // declaration only
	}

	namespace no_adl {
		template<typename T>
		struct value_type_impl {
			using type = T;
		};
	}
	template<typename T>
	using value_t = typename tc::no_adl::value_type_impl<tc::decay_t<T>>::type;

	namespace range_value_no_adl {
		template<typename Rng>
		struct from_range_output final { using type = void; };

		template<typename Rng> requires requires { typename boost::mp11::mp_apply<tc::common_type_t, tc::mp_transform<tc::value_t, tc::range_output_t<Rng>>>; }
		struct from_range_output<Rng> final {
			using type = boost::mp11::mp_apply<tc::common_type_t, tc::mp_transform<tc::value_t, tc::range_output_t<Rng>>>;
		};

		template<typename Rng>
		struct from_cont final { using type = void; };

		template<typename Cont> requires requires { typename std::remove_reference_t<Cont>::value_type; }
		struct from_cont<Cont> final {
			using type = typename std::remove_reference_t<Cont>::value_type;
		};

		template<typename Rng, typename ValueTypeFromOutput = typename from_range_output<Rng>::type, typename ValueTypeFromCont = typename from_cont<Rng>::type>
		struct range_value final {
			// STATICASSERTSAME(ValueTypeFromOutput, ValueTypeFromCont); fires for Rng=std::array<tc::tuple<int&>, 1> (tuple<int> or tuple<int&>) or Rng=boost::filesystem::path (path or wchar_t?).
			// In both cases, not defining range_value_t is sensible.
		};

		template<typename Rng, typename ValueType>
		struct range_value<Rng, ValueType, ValueType> final {
			using type = ValueType;
		};

		template<typename Rng, typename ValueTypeFromOutput>
		struct range_value<Rng, ValueTypeFromOutput, void> final {
			using type = ValueTypeFromOutput;
		};

		template<typename Rng, typename ValueTypeFromCont>
		struct range_value<Rng, void, ValueTypeFromCont> final {
			using type = ValueTypeFromCont;
		};

		template<typename Rng>
		struct range_value<Rng, void, void> final {}; // range_value_t is not void
	}
	template<typename Rng>
	using range_value_t = typename tc::range_value_no_adl::range_value<Rng>::type;

	template<typename Rng>
	concept has_range_value = requires { typename tc::range_value_t<Rng>; };

	namespace no_adl {
		template<typename Rng>
		struct constexpr_size_impl;
	}
	template<typename Rng>
	concept has_constexpr_size = requires { no_adl::constexpr_size_impl<std::remove_cvref_t<Rng>>::value; };

	template<typename T>
	[[nodiscard]] constexpr std::size_t strlen( T const* pt ) noexcept {
		_ASSERTNORETURN(pt);
		return std::char_traits<T>::length(pt);
	}

	namespace zero_termination_sentinel_adl {
		struct zero_termination_sentinel final {};

		template<typename T>
		constexpr bool operator==(zero_termination_sentinel, T* p) noexcept {
			return !*VERIFY(p);
		}
	}
	using zero_termination_sentinel_adl::zero_termination_sentinel;

	namespace nullptr_or_zero_termination_sentinel_adl {
		struct nullptr_or_zero_termination_sentinel final {};

		template<tc::char_type Char>
		constexpr bool operator==(nullptr_or_zero_termination_sentinel, Char const* psz) noexcept {
			return !psz || static_cast<Char>(0) == *psz;
		}
	}
	using nullptr_or_zero_termination_sentinel_adl::nullptr_or_zero_termination_sentinel;

	namespace begin_end_adl {
		template<typename T, std::size_t N>
		constexpr T* begin(begin_end_tag_t, T (&at)[N]) noexcept {
			return at;
		}
		template<typename T, std::size_t N>
		constexpr T const* begin(begin_end_tag_t, T const (&at)[N]) noexcept {
			return at;
		}
		template<typename T, std::size_t N>
		constexpr T* end_array_impl(T (&at)[N]) noexcept {
			if constexpr (tc::char_type<T>) {
				_ASSERTE( tc::strlen(at) == N - 1 );
				return at + N - 1;
			} else {
				return at + N;
			}
		}
		template<typename T, std::size_t N>
		constexpr T* end(begin_end_tag_t, T (&at)[N]) noexcept {
			return end_array_impl(at);
		}
		template<typename T, std::size_t N>
		constexpr T const* end(begin_end_tag_t, T const (&at)[N]) noexcept {
			return end_array_impl(at);
		}

		template<typename It, typename Enable = decltype(*++std::declval<It&>())>
		constexpr It begin(begin_end_tag_t, std::pair<It,It> const& pairit) noexcept {
			return pairit.first;
		}
		template<typename It>
		constexpr It end(begin_end_tag_t, std::pair<It,It> const& pairit) noexcept {
			return pairit.second;
		}
	}

	template <typename It>
	constexpr auto enable_borrowed_range<std::pair<It, It>> = true;
}

namespace boost {
	template<typename Rng> requires std::same_as<Rng, std::remove_cvref_t<Rng>> && requires { typename tc::iterator_t<Rng>; }
	struct range_mutable_iterator<Rng> {
		using type = tc::iterator_t<Rng>;
	};

	template<typename Rng> requires std::same_as<Rng, std::remove_cvref_t<Rng>> && requires { typename tc::iterator_t<std::add_const_t<Rng>>; }
	struct range_const_iterator<Rng> {
		using type = tc::iterator_t<std::add_const_t<Rng>>;
	};
}

#pragma push_macro("CHAR_RANGE")
#define CHAR_RANGE( xchar ) \
namespace tc { \
	namespace begin_end_adl { \
		/* Note: We cannot use overloading to differentiate xchar* and xchar(&)[N]. */ \
		template<typename CharPtrConvertible> requires \
			tc::safely_convertible_to<CharPtrConvertible, xchar*> \
				&& (!tc::has_constexpr_size<CharPtrConvertible>) \
		constexpr xchar* begin(begin_end_tag_t, CharPtrConvertible& pchc) noexcept { \
			return pchc; \
		} \
		template<typename CharPtrConvertible> requires \
			tc::safely_convertible_to<CharPtrConvertible, xchar const*> \
				&& (!tc::safely_convertible_to<CharPtrConvertible, xchar*>) \
				&& (!tc::has_constexpr_size<CharPtrConvertible>) \
		constexpr xchar const* begin(begin_end_tag_t, CharPtrConvertible& pchc) noexcept { \
			return pchc; \
		} \
		template<typename CharPtrConvertible> requires \
			tc::safely_convertible_to<CharPtrConvertible, xchar const*> \
				&& (!tc::has_constexpr_size<CharPtrConvertible>) \
		constexpr tc::zero_termination_sentinel end(begin_end_tag_t, CharPtrConvertible& pchc) noexcept { \
			_ASSERT( tc::implicit_cast<xchar const*>(pchc) ); \
			return {}; \
		} \
	} \
	template <> \
	inline constexpr auto enable_borrowed_range<xchar*> = true; \
	template <> \
	inline constexpr auto enable_borrowed_range<xchar const*> = true; \
}

CHAR_RANGE(char)
CHAR_RANGE(wchar_t)
CHAR_RANGE(char8_t)
CHAR_RANGE(char16_t)
CHAR_RANGE(char32_t)

#pragma pop_macro("CHAR_RANGE")

namespace tc {
	namespace no_adl {
		template<typename TTarget>
		struct curried_safely_convertible_to {
			template<typename TSource>
			using fn = tc::constant<tc::safely_convertible_to<TSource, TTarget>>;
		};
	}

	template<typename Rng, typename T>
	concept range_of = boost::mp11::mp_all_of<tc::range_output_t<Rng>, no_adl::curried_safely_convertible_to<T>::template fn>::value;
}

namespace tc_begin_end_no_adl {
	template< typename Rng >
	auto adl_begin(Rng& rng) return_decltype_MAYTHROW(
		// Rng has free begin found by ADL
		begin( rng )
	)

	template< typename Rng >
	auto adl_end(Rng& rng) return_decltype_MAYTHROW(
		// Rng has free end found by ADL
		end( rng )
	)
}

namespace tc {
	template <typename Rng>
	auto cyclic_next(tc::iterator_t<Rng> it, Rng& rng) noexcept {
		++it;
		return tc::end(rng)==it ? tc::begin(rng) : it;
	}
}

//////////////////////////////////////////////
// Boost.Range compatiblity

#ifdef BOOST_RANGE_BEGIN_HPP
#error
#endif
#ifdef BOOST_RANGE_END_HPP
#error
#endif
#define BOOST_RANGE_BEGIN_HPP
#define BOOST_RANGE_END_HPP

namespace boost {
	template< typename Rng >
	auto begin(Rng&& rng) return_decltype_MAYTHROW(
		tc::begin( rng )
	)

	template< typename Rng >
	auto end(Rng&& rng) return_decltype_MAYTHROW(
		tc::end( rng )
	)

	template< typename Rng >
	auto const_begin(Rng const& rng) return_decltype_MAYTHROW(
		tc::begin( rng )
	)

	template< typename Rng >
	auto const_end(Rng const& rng) return_decltype_MAYTHROW(
		tc::end( rng )
	)
}

//////////////////////////////////////////////
// std.range compatiblity
namespace std::ranges {
	template <typename Rng> requires (std::same_as<decltype(tc::enable_borrowed_range<Rng>), const bool>)
	constexpr bool enable_borrowed_range<Rng> = tc::enable_borrowed_range<Rng>;
}

//////////////////////////////////////////////
// range customizations for tuple_like types
namespace tc {
	namespace no_adl {
		template<tuple_like Tuple> requires (!requires { tc::begin(std::declval<Tuple>()); })
		struct constexpr_size_impl<Tuple>: tc::least_uint_constant<std::tuple_size<Tuple>::value> {};
	}

	namespace tuple_detail {
		template<std::size_t I, tuple_like... Tuple>
		constexpr auto zip_get(Tuple&&... tuple) noexcept {
			// Tuple elements are tc::apply_cvref_t<std::tuple_element_t<I, std::remove_cvref_t<Tuple>>, Tuple>... unless tuple is tc::tuple and element types is empty.
			return tc::tuple<std::conditional_t<
				std::is_rvalue_reference<std::tuple_element_t<I, std::remove_cvref_t<Tuple>>>::value,
				decltype(tc::get<I>(std::declval<Tuple>())),
				tc::remove_rvalue_reference_t<decltype(tc::get<I>(std::declval<Tuple>()))>
			>...>{{
				{tc::get<I>(tc_move_if_owned(tuple))}...
			}};
		}

		template<std::size_t... I, tuple_like... Tuple>
		constexpr auto zip_impl(std::index_sequence<I...>, Tuple&&... tuple) noexcept {
			return tc::tuple<decltype(tuple_detail::zip_get<I>(tc_move_if_owned(tuple)...))...>{{ // tc::make_tuple without extra copy
				{tuple_detail::zip_get<I>(tc_move_if_owned(tuple)...)}...
			}};
		}
	}

	template<tuple_like Tuple0, tuple_like... Tuple>
		requires (!tc::range_with_iterators<Tuple0> || ... || !tc::range_with_iterators<Tuple>) // Prefer zip_adaptor for zip(std::array...)
	[[nodiscard]] constexpr auto zip(Tuple0&& tuple0, Tuple&&... tuple) noexcept {
		static_assert( ((std::tuple_size<std::remove_reference_t<Tuple0>>::value == std::tuple_size<std::remove_reference_t<Tuple>>::value) && ...) );
		return tuple_detail::zip_impl(
			std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple0>>::value>(),
			tc_move_if_owned(tuple0),
			tc_move_if_owned(tuple)...
		);
	}

	template<typename TIndex, TIndex... Is>
	[[nodiscard]] consteval tc::tuple<tc::constant<Is>...> index_sequence_as_tuple(std::integer_sequence<TIndex, Is...>) {
		return {};
	}

	template<tuple_like Tuple> requires (!tc::range_with_iterators<Tuple>)
	[[nodiscard]] constexpr auto enumerate(Tuple&& tuple) noexcept {
		return tc::zip(
			tc::index_sequence_as_tuple(std::make_integer_sequence<int, std::tuple_size<std::remove_reference_t<Tuple>>::value>()),
			tc_move_if_owned(tuple)
		);
	}
}

//////////////////////////////////////////////
// range customizations for boost::mp11::mp_list
namespace tc {
	namespace no_adl {
		template<typename ... T>
		struct constexpr_size_impl<boost::mp11::mp_list<T...>>: tc::least_uint_constant<sizeof...(T)> {};
	}

	// Returns an mp_list<mp_list<...>, ...>, tc::for_each turns it into a range of tc::tuple<std::type_identity<...>, ...>.
	template<typename ... Lists> requires (tc::instance<std::remove_reference_t<Lists>, boost::mp11::mp_list> && ...)
	[[nodiscard]] constexpr auto zip(Lists&&...) noexcept {
		return tc::mp_zip<Lists...>{};
	}

	// Returns an mp_list<mp_list<tc::constant<0>, ...>, ...>, tc::for_each turns it into a range of tc::tuple<tc::constant<N>, std::type_identity<...>>.
	template<typename List> requires tc::instance<std::remove_reference_t<List>, boost::mp11::mp_list>
	[[nodiscard]] constexpr auto enumerate(List&&) noexcept {
		return tc::mp_enumerate<List>{};
	}
}

