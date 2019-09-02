
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "meta.h"
#include "container_traits.h"
#include "insert.h"
#include "transform.h"
#include "convert_enc.h"
#include "cont_reserve.h"
#include "sub_range.h"
#include "concat_adaptor.h"
#include "container.h"
#include "construction_restrictiveness.h"

namespace tc {

	// in general, do not use Cont::insert() or Cont(it, it)
	// iterators are slower than for_each in many cases (eg. filter ranges)

	namespace appender_adl {
		DEFINE_ADL_TAG_TYPE(appender_tag);
	}

	namespace append_no_adl {
		template< typename Cont, typename Enable=void >
		struct has_mem_fn_appender final: std::false_type {};

		template< typename Cont >
		struct has_mem_fn_appender<Cont, tc::void_t<decltype(std::declval<Cont&>().appender())>> final: std::true_type {};

		template< typename Cont, typename Enable=void >
		struct has_adl_appender final: std::false_type {};

		template< typename Cont >
		struct has_adl_appender<Cont, decltype(appender(tc::appender_adl::appender_tag, std::declval<Cont&>()), void())> final: std::true_type {};	

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

		template< typename Cont >
		struct appender_type_no_reserve {
			using sink_value_type = tc::range_value_t<Cont>;
			constexpr explicit appender_type_no_reserve(Cont& cont) noexcept: m_cont(cont) {}

			Cont& m_cont;

			template< typename T, typename Enable=decltype(tc::cont_emplace_back(m_cont, std::declval<T>())) >
			void operator()(T&& t) const& MAYTHROW {
				tc::cont_emplace_back(m_cont, std::forward<T>(t)); // MAYTHROW
			}

			// must leave std::enable_if_t in parameter list because
			// https://stackoverflow.com/questions/51933397/sfinae-method-completely-disables-base-classs-template-method-in-clang
			template< typename Rng >
			auto chunk(Rng&& rng, std::enable_if_t<is_range_insertable<Cont, Rng>::value>* = nullptr) const& MAYTHROW {
				NOBADALLOC(m_cont.insert(tc::end(m_cont), tc::begin(rng), tc::end(rng)));
				return INTEGRAL_CONSTANT(tc::continue_)();
			}

			template< typename Rng >
			auto chunk(Rng&& rng, std::enable_if_t<is_conv_enc_needed<tc::range_value_t<Cont>, Rng>::value>* = nullptr) const& MAYTHROW return_decltype(
				tc::for_each(tc::must_convert_enc<tc::range_value_t<Cont>>(std::forward<Rng>(rng)), *this)
			)
		};

		template< typename Cont >
		struct appender_type /*final*/: appender_type_no_reserve<Cont> {
			using base_ = appender_type_no_reserve<Cont>;
			using base_::base_;

			using base_::chunk;

			template< typename Rng, ENABLE_SFINAE, std::enable_if_t<
				!is_conv_enc_needed<tc::range_value_t<Cont>, Rng>::value &&
				has_mem_fn_reserve<SFINAE_TYPE(Cont)>::value &&
				tc::has_size<Rng>::value &&
				!is_range_insertable<Cont, Rng>::value
			>* = nullptr>
			auto chunk(Rng&& rng) const& MAYTHROW code_return_decltype(
				tc::cont_reserve(this->m_cont, this->m_cont.size()+tc::size(rng));,
				tc::for_each(std::forward<Rng>(rng), tc::base_cast</*SFINAE_TYPE to workaround clang bug*/SFINAE_TYPE(base_)>(*this))
			)
		};

		template<typename Derived, typename Value>
		struct appender_type_base {
			using sink_value_type = Value;

			template<typename T, typename Derived2=Derived>
			auto operator()(T const& val) const& MAYTHROW return_decltype(
				tc::derived_cast<Derived2>(*this).chunk(tc::single(val))
			)

			// must leave std::enable_if_t in parameter list because
			// https://stackoverflow.com/questions/51933397/sfinae-method-completely-disables-base-classs-template-method-in-clang
			template<typename Rng>
			auto chunk(Rng&& rng, std::enable_if_t<is_conv_enc_needed<sink_value_type, Rng>::value>* = nullptr) const& MAYTHROW return_decltype(
				tc::for_each(tc::must_convert_enc<sink_value_type>(std::forward<Rng>(rng)), tc::derived_cast<Derived>(*this))
			)
		};
	}
	using append_no_adl::appender_type;
	using append_no_adl::appender_type_base;

	template<typename Cont, std::enable_if_t<!tc::append_no_adl::has_mem_fn_appender<Cont>::value && !tc::append_no_adl::has_adl_appender<Cont>::value>* = nullptr>
	auto make_appender(Cont& cont) noexcept {
		return tc::appender_type<Cont>(cont);
	}

	template<typename Cont, std::enable_if_t<tc::append_no_adl::has_mem_fn_appender<Cont>::value>* = nullptr>
	auto make_appender(Cont& cont) noexcept {
		return cont.appender();
	}

	template<typename Cont, std::enable_if_t<tc::append_no_adl::has_adl_appender<Cont>::value>* = nullptr>
	auto make_appender(Cont& cont) noexcept {
		return appender(tc::appender_adl::appender_tag, cont);
	}

	namespace no_adl {
		template<typename Cont, typename Rng, typename Enable=void>
		struct is_appendable_impl: std::false_type {};

		template<typename Cont, typename Rng>
		struct is_appendable_impl<Cont, Rng, decltype(tc::for_each(std::declval<Rng>(), tc::make_appender(std::declval<Cont>())), void())>: std::true_type {};

		// tc::void_t<decltype()...> is not working correctly with Visual Studio 15.8.0
		// alias is not working correctly with Visual Studio 15.8.0
		template<typename Cont, typename... Rng>
		struct is_appendable: std::conjunction<no_adl::is_appendable_impl<Cont, Rng>...> {};
	}
	using no_adl::is_appendable;

	namespace append_detail {
		template<typename Cont, typename Rng0, typename ... Rng>
		void append_impl(Cont& cont, Rng0&& rng0, Rng&& ... rng) MAYTHROW {
			if constexpr (0==sizeof...(Rng)) {
				tc::for_each(std::forward<Rng0>(rng0), tc::make_appender(cont));
			} else {
				tc::for_each(tc::concat(std::forward<Rng0>(rng0), std::forward<Rng>(rng)...), tc::make_appender(cont)); // tc::concat is not SFINAE friendly
			}
		}
	}

	// append for target containers without reserve() member:
	// just run a for_each over the input
	// assume iterators are stable to get iterator to first inserted element
	template< template<typename> class RangeReturn = tc::return_void, typename Cont, typename... Rng, std::enable_if_t<tc::is_appendable<Cont&, Rng&&...>::value && tc::is_range_with_iterators<Cont>::value && !has_mem_fn_reserve<Cont>::value>* = nullptr >
	decltype(auto) append(Cont& cont, Rng&&... rng) MAYTHROW {
		std::optional<typename boost::range_iterator<Cont>::type> oit;
		if (!tc::empty(cont)) {
			oit = tc::end_prev(cont);
		}
		try {
			append_detail::append_impl(cont, std::forward<Rng>(rng)...);
			return RangeReturn<Cont&>::pack_border(
				oit ? boost::next(*oit) : tc::begin(cont),
				cont
			);
		} catch(...) {
			tc::take_inplace(cont, oit ? boost::next(*oit) : tc::begin(cont));
			throw;
		}
	}

	// append for target containers with reserve() member.
	// If appending random-access iterator range, use Cont::insert() to give insert the opportunity for optimizations
	template< template<typename> class RangeReturn = tc::return_void, typename Cont, typename... Rng, std::enable_if_t<tc::is_appendable<Cont&, Rng&&...>::value && tc::is_range_with_iterators<Cont>::value && has_mem_fn_reserve<Cont>::value>* = nullptr >
	decltype(auto) append(Cont& cont, Rng&&... rng) MAYTHROW {
		auto const nOffset = cont.size();
		try {
			append_detail::append_impl(cont, std::forward<Rng>(rng)...);
			return RangeReturn<Cont&>::pack_border(
				tc::begin_next(cont,nOffset),
				cont
			);
		} catch(...) {
			tc::take_first_inplace(cont, nOffset);
			throw;
		}
	}

	// append for non-range cont
	template< typename Cont, typename... Rng, std::enable_if_t<tc::is_appendable<Cont&, Rng&&...>::value && !tc::is_range_with_iterators<Cont>::value>* = nullptr >
	void append(Cont&& cont, Rng&&... rng) MAYTHROW {
		append_detail::append_impl(cont, std::forward<Rng>(rng)...);
	}

	namespace no_adl {
		template<typename TTarget>
		struct SConversions<TTarget, std::enable_if_t<has_mem_fn_push_back<TTarget>::value || has_mem_fn_emplace_back<TTarget>::value>> final {
		private:
			template<typename Rng0, typename... RngN>
			using use_ctor=std::integral_constant<bool,
				tc::is_base_of<TTarget, tc::remove_cvref_t<Rng0> >::value &&
				(
					0==sizeof...(RngN) ||
				 	(!std::is_reference<Rng0>::value && !std::is_const<Rng0>::value)
				)
			>;

		public:
			template<typename... T, std::enable_if_t<tc::is_safely_constructible<TTarget, tc::aggregate_tag_t, T&&...>::value>* = nullptr>
			static TTarget fn(tc::aggregate_tag_t, T&&... t) MAYTHROW {
				return TTarget(tc::aggregate_tag, std::forward<T&&>(t)...);
			}

			template<
				typename Rng0,
				typename... RngN,
				std::enable_if_t<use_ctor<Rng0,RngN...>::value>* = nullptr,
				std::enable_if_t<tc::is_appendable<TTarget&, RngN&&...>::value>* = nullptr
			>
			static TTarget fn(Rng0&& rng0, RngN&&... rngN) MAYTHROW {
				if constexpr(0<sizeof...(RngN)) {
					TTarget cont=std::forward<Rng0>(rng0);
 					tc::append(cont, std::forward<RngN>(rngN)...);
					return cont;
				} else {
					return std::forward<Rng0>(rng0);
				}
			}

			template<
				typename Rng0,
				typename... RngN,
				std::enable_if_t<!use_ctor<Rng0,RngN...>::value>* = nullptr,
				std::enable_if_t<tc::is_appendable<TTarget&, Rng0&&, RngN&&...>::value>* = nullptr
			>
			static TTarget fn(Rng0&& rng0, RngN&&... rngN) MAYTHROW {
				TTarget cont;
 				tc::append(cont, std::forward<Rng0>(rng0), std::forward<RngN>(rngN)...);
				return cont;
			}
		};
	}

	template< typename Rng >
	auto make_vector(Rng&& rng) MAYTHROW {
		return tc::explicit_cast<tc::vector<tc::range_value_t<Rng>>>(std::forward<Rng>(rng));
	}

	template< typename... Rng >
	auto make_vector(Rng&&... rng) MAYTHROW {
		return make_vector(tc::concat(std::forward<Rng>(rng)...));
	}

	DEFINE_FN(make_vector);

	template< typename Rng >
	auto make_str(Rng&& rng) MAYTHROW {
		return tc::explicit_cast<std::basic_string<tc::range_value_t<Rng>>>(std::forward<Rng>(rng));
	}

	template< typename... Rng >
	auto make_str(Rng&&... rng) MAYTHROW {
		return make_str(tc::concat(std::forward<Rng>(rng)...));
	}

	template< typename Char, typename ... Rng >
	auto make_str(Rng&& ... rng) MAYTHROW {
		return tc::explicit_cast<std::basic_string<Char>>(std::forward<Rng>(rng)...);
	}

	template< typename T, typename Rng >
	auto make_unique_unordered_set(Rng&& rng) MAYTHROW {
		tc::unordered_set<T> set;
		tc::cont_try_insert_range(set, std::forward<Rng>(rng));
		return set;
	}

	template< typename Rng >
	auto make_unique_unordered_set(Rng&& rng) MAYTHROW {
		return make_unique_unordered_set<tc::range_value_t<Rng>>(std::forward<Rng>(rng));
	}
}
