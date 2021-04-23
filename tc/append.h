
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "meta.h"
#include "container_traits.h"
#include "insert.h"
#include "transform.h"
#include "convert_enc.h"
#include "cont_reserve.h"
#include "subrange.h"
#include "concat_adaptor.h"
#include "container.h"
#include "construction_restrictiveness.h"
#include "try_finally.h"

namespace tc {

	// in general, do not use Cont::insert() or Cont(it, it)
	// iterators are slower than for_each in many cases (eg. filter ranges)

	namespace append_no_adl {
		template< typename TTarget, typename Rng, typename Enable=void >
		struct is_conv_enc_needed final: std::false_type {};

		template< typename TTarget, typename Rng >
		struct is_conv_enc_needed<TTarget, Rng, std::enable_if_t<
			tc::is_range_with_iterators<Rng>::value
		>> final:
			std::integral_constant<bool,
				tc::is_char<TTarget>::value &&
				tc::is_char<tc::range_value_t<Rng>>::value &&
				!std::is_same<TTarget, tc::range_value_t<Rng>>::value
			>
		{};

		template< typename Cont, typename Rng, typename Enable=void >
		struct is_range_insertable final: std::false_type {};

		template< typename Cont, typename Rng >
		struct is_range_insertable<Cont, Rng, std::enable_if_t<
			!is_conv_enc_needed<tc::range_value_t<Cont>, Rng>::value &&
			has_mem_fn_reserve<Cont>::value &&
			!tc::is_concat_range<tc::remove_cvref_t<Rng>>::value && // it might be more efficient to append by ranges than by iterators
			std::is_convertible<
				typename std::iterator_traits<std::remove_reference_t<decltype(tc::begin(std::declval<Rng>()))>>::iterator_category,
				std::random_access_iterator_tag
			>::value &&
			tc::econstructionIMPLICIT==tc::construction_restrictiveness<tc::range_value_t<Cont>, tc::range_reference_t<Rng>>::value
		>> final: std::true_type {};

		template< typename Cont, bool bReserve = has_mem_fn_reserve<Cont>::value>
		struct [[nodiscard]] appender_type;

		template< typename Cont>
		struct [[nodiscard]] appender_type<Cont, /*bReserve*/false> {
			using sink_value_type = tc::range_value_t<Cont>;
			using guaranteed_break_or_continue = INTEGRAL_CONSTANT(tc::continue_);
			constexpr explicit appender_type(Cont& cont) noexcept: m_cont(cont) {}

			Cont& m_cont;

			template< typename T, typename Enable=decltype(tc::cont_emplace_back(m_cont, std::declval<T>())) >
			constexpr void operator()(T&& t) const& noexcept(noexcept(tc::cont_emplace_back(m_cont, std::forward<T>(t)))) {
				tc::cont_emplace_back(m_cont, std::forward<T>(t)); // MAYTHROW
			}

			// If appending random-access iterator range, use Cont::insert() to give insert the opportunity for optimizations
			// must leave std::enable_if_t in parameter list because
			// https://stackoverflow.com/questions/51933397/sfinae-method-completely-disables-base-classs-template-method-in-clang
			template< typename Rng >
			void chunk(Rng&& rng, std::enable_if_t<is_range_insertable<Cont, Rng>::value>* = nullptr) const& noexcept(noexcept(
				m_cont.insert(tc::end(m_cont), tc::begin(rng), tc::end(rng))
			)) {
				NOBADALLOC(m_cont.insert(tc::end(m_cont), tc::begin(rng), tc::end(rng)));
			}

			template< typename Rng >
			void chunk(Rng&& rng, std::enable_if_t<is_conv_enc_needed<tc::range_value_t<Cont>, Rng>::value>* = nullptr) const& return_MAYTHROW(
				tc::implicit_cast<void>(tc::for_each(tc::must_convert_enc<tc::range_value_t<Cont>>(std::forward<Rng>(rng)), *this))
			)
		};

		template< typename Cont >
		struct [[nodiscard]] appender_type<Cont, /*bReserve*/true> /*final*/: appender_type<Cont, /*bReserve*/false> {
			using base_ = appender_type<Cont, /*bReserve*/false>;
			using base_::base_;
			using base_::chunk;

			template< typename Rng, ENABLE_SFINAE, std::enable_if_t<
				!is_conv_enc_needed<tc::range_value_t<Cont>, Rng>::value &&
				tc::has_size<Rng>::value &&
				!is_range_insertable<Cont, Rng>::value
			>* = nullptr>
			constexpr auto chunk(Rng&& rng) const& return_decltype_MAYTHROW(
				tc::cont_reserve(this->m_cont, this->m_cont.size()+tc::size(rng)),
				tc::implicit_cast<void>(tc::for_each(std::forward<Rng>(rng), tc::base_cast</*SFINAE_TYPE to workaround clang bug*/SFINAE_TYPE(base_)>(*this)))
			)
		};
	}
	using append_no_adl::appender_type;

	namespace appender_default {
		template<typename Cont>
		constexpr auto appender_impl(Cont& cont) noexcept {
			return tc::appender_type<Cont>(cont);
		}
	}
	DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(appender)

	template<typename Cont>
	using appender_t = decltype(tc::appender(std::declval<Cont>()));

	namespace no_adl {
		template<typename Cont, typename... Rng>
		// Hard error, if concat or appender fails.
		// Also maybe hard error, inside has_for_each, if range evaluation not sfinae-friendly.
		// true, false, otherwise
		struct is_appendable : std::bool_constant<tc::has_for_each<decltype(tc::concat(std::declval<Rng>()...)), tc::appender_t<Cont>>::value> {};
	}
	using no_adl::is_appendable;

	// Disallow 0 == sizeof...(Rngs), so that overload taking single argument tc::tuple<Cont, Rng...> is rejected
	template< typename RangeReturn = tc::return_void, typename Cont, typename... Rngs, std::enable_if_t<std::conjunction<
		std::bool_constant<0 < sizeof...(Rngs)>,
		tc::is_appendable<Cont&, Rngs...>
	>::value>* = nullptr >
	constexpr decltype(auto) append(Cont&& cont, Rngs&&... rngs) MAYTHROW {
		static_assert( !std::is_const<Cont>::value, "Cannot append to const container" );
		static_assert( !tc::is_range_with_iterators<Cont>::value || std::is_lvalue_reference<Cont>::value, "Append to rvalue intentional?" );

		if constexpr( !tc::is_range_with_iterators<Cont>::value || (
			std::is_same<RangeReturn, tc::return_void>::value && 
			noexcept(tc::for_each(tc::concat(std::forward<Rngs>(rngs)...), tc::appender(cont)))
		) ) {
			static_assert( std::is_same<RangeReturn, tc::return_void>::value, "RangeReturn not supported, if appending to stream." );

			tc::for_each(tc::concat(std::forward<Rngs>(rngs)...), tc::appender(cont));
		} else if constexpr( tc::is_random_access_range<Cont>::value || has_mem_fn_reserve<Cont>::value ) {
			auto const nOffset = tc::size_raw(cont);
			try {
				tc::for_each(tc::concat(std::forward<Rngs>(rngs)...), tc::appender(cont));
				if constexpr( !std::is_same<RangeReturn, tc::return_void>::value ) {
					return RangeReturn::pack_border(
						tc::begin_next<tc::return_border>(cont, nOffset),
						cont
					);
				}
			} catch (...) {
				tc::take_first_inplace(cont, nOffset);
				throw;
			}
		} else {
			// assume iterators are stable to get iterator to first inserted element
			auto const it = tc::back<tc::return_element_or_null>(cont);
			auto FirstAppendedElement = [&]() noexcept {
				return it ? modified(it, ++_) : tc::begin(cont);
			};
			try {
				tc::for_each(tc::concat(std::forward<Rngs>(rngs)...), tc::appender(cont)); // MAYTHROW
				if constexpr( !std::is_same<RangeReturn, tc::return_void>::value ) {
					return RangeReturn::pack_border(FirstAppendedElement(), cont);
				}
			} catch (...) {
				tc::take_inplace(cont, FirstAppendedElement());
				throw;
			}
		}
	}

	namespace explicit_convert_to_container_detail {
		template<typename TTarget, typename Rng0, typename... RngN>
		using use_ctor=std::integral_constant<bool,
			tc::is_base_of<TTarget, tc::remove_cvref_t<Rng0> >::value &&
			(
				0==sizeof...(RngN) ||
			 	(!std::is_reference<Rng0>::value && !std::is_const<Rng0>::value)
			)
		>;
	}

	namespace explicit_convert_adl {
		template<
			typename TTarget,
			typename Rng0, typename... RngN,
			std::enable_if_t<has_mem_fn_push_back<TTarget>::value || has_mem_fn_emplace_back<TTarget>::value>* = nullptr,
			std::enable_if_t<explicit_convert_to_container_detail::use_ctor<TTarget, Rng0, RngN...>::value>* = nullptr,
			std::enable_if_t<tc::is_appendable<TTarget&, RngN...>::value>* = nullptr
		>
		TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, Rng0&& rng0, RngN&&... rngN) MAYTHROW {
			if constexpr(0<sizeof...(RngN)) {
				TTarget cont=std::forward<Rng0>(rng0);
 				tc::append(cont, std::forward<RngN>(rngN)...);
				return cont;
			} else {
				return std::forward<Rng0>(rng0);
			}
		}

		template<
			typename TTarget,
			typename Rng0, typename... RngN,
			std::enable_if_t<has_mem_fn_push_back<TTarget>::value || has_mem_fn_emplace_back<TTarget>::value>* = nullptr,
			std::enable_if_t<!explicit_convert_to_container_detail::use_ctor<TTarget, Rng0, RngN...>::value>* = nullptr,
			std::enable_if_t<tc::is_appendable<TTarget&, Rng0, RngN...>::value>* = nullptr
		>
		TTarget explicit_convert_impl(adl_tag_t, tc::type::identity<TTarget>, Rng0&& rng0, RngN&&... rngN) MAYTHROW {
			TTarget cont;
 			tc::append(cont, std::forward<Rng0>(rng0), std::forward<RngN>(rngN)...);
			return cont;
		}
	}

	template< typename... Rng >
	[[nodiscard]] auto make_vector(Rng&&... rng) MAYTHROW {
		static_assert(0 < sizeof...(Rng));
		return tc::explicit_cast<tc::vector<tc::range_value_t<decltype(tc::concat(std::forward<Rng>(rng)...))>>>(std::forward<Rng>(rng)...);
	}

	template< typename Char, typename... Rng >
	[[nodiscard]] auto make_str(Rng&&... rng) MAYTHROW {
		static_assert(0 < sizeof...(Rng));
		return tc::explicit_cast<std::basic_string<Char>>(std::forward<Rng>(rng)...);
	}

	template< typename... Rng >
	[[nodiscard]] auto make_str(Rng&&... rng) MAYTHROW {
		static_assert(0 < sizeof...(Rng));
		return tc::make_str<tc::range_value_t<decltype(tc::concat(std::forward<Rng>(rng)...))>>(std::forward<Rng>(rng)...);
	}

	template< typename T, typename Rng >
	[[nodiscard]] auto make_unique_unordered_set(Rng&& rng) MAYTHROW {
		tc::unordered_set<T> set;
		tc::cont_try_insert_range(set, std::forward<Rng>(rng));
		return set;
	}

	template< typename Rng >
	[[nodiscard]] auto make_unique_unordered_set(Rng&& rng) MAYTHROW {
		return make_unique_unordered_set<tc::range_value_t<Rng>>(std::forward<Rng>(rng));
	}

	template< typename T, typename Rng >
	[[nodiscard]] auto make_unordered_set(Rng&& rng) MAYTHROW {
		tc::unordered_set<T> set;
		tc::cont_must_insert_range(set, std::forward<Rng>(rng));
		return set;
	}

	template< typename Rng >
	[[nodiscard]] auto make_unordered_set(Rng&& rng) MAYTHROW {
		return make_unordered_set<tc::range_value_t<Rng>>(std::forward<Rng>(rng));
	}
}
