
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/construction_restrictiveness.h"
#include "../base/inside_unwinding.h"

#include "../container/container_traits.h"
#include "../container/insert.h"
#include "../container/cont_reserve.h"
#include "../container/container.h"
#include "../container/string.h"
#include "../string/convert_enc.h"

#include "../range/subrange.h"
#include "../range/transform.h"
#include "../range/repeat_n.h"
#include "../range/concat_adaptor.h"

#include <boost/range/algorithm/copy.hpp>

namespace tc {
	namespace append_detail {
		template<typename Rng, typename TTarget>
		concept conv_enc_needed =
			tc::char_type<TTarget> &&
			tc::range_with_iterators<Rng> &&
			tc::char_type<tc::range_value_t<Rng>> &&
			!std::is_same<TTarget, tc::range_value_t<Rng>>::value;

		// in general, do not use Cont::insert() or Cont(it, it)
		// iterators are slower than for_each in many cases (eg. filter ranges)
		template<typename Rng, typename Cont>
		concept range_insertable =
			(!conv_enc_needed<Rng, tc::range_value_t<Cont>>) &&
			has_mem_fn_reserve<Cont> &&
			tc::common_range<Rng> &&
			std::convertible_to<
				typename std::iterator_traits<tc::iterator_t<Rng>>::iterator_category,
				std::random_access_iterator_tag
			> &&
			(!tc::prefers_for_each<Rng>) && // it might be more efficient to append by ranges than by iterators
			tc::econstructionIMPLICIT==tc::construction_restrictiveness<tc::range_value_t<Cont>, std::iter_reference_t<tc::iterator_t<Rng>>>::value;
	}

	namespace append_no_adl {
		template< typename Cont, bool bReserve = has_mem_fn_reserve<Cont>>
		struct [[nodiscard]] appender_type;

		template< typename Cont>
		struct [[nodiscard]] appender_type<Cont, /*bReserve*/false> {
			using guaranteed_break_or_continue = tc::constant<tc::continue_>;
			constexpr explicit appender_type(Cont& cont) noexcept: m_cont(cont) {}

			Cont& m_cont;

			template<typename T>
			constexpr void operator()(T&& t) const& noexcept(noexcept(tc::cont_emplace_back(m_cont, tc_move_if_owned(t))))
				requires
					(!tc::char_like<tc::range_value_t<Cont>> || tc::safely_convertible_to<T&&, tc::range_value_t<Cont>>) &&
					requires { tc::cont_emplace_back(std::declval<Cont&>(), tc_move_if_owned(t)); }
			{
				tc::cont_emplace_back(m_cont, tc_move_if_owned(t)); // MAYTHROW
			}

			// If appending random-access iterator range, use Cont::insert() to give insert the opportunity for optimizations.
			void chunk(append_detail::range_insertable<Cont> auto&& rng) const& noexcept(noexcept(
				m_cont.insert(tc::end(m_cont), tc::begin(rng), tc::end(rng))
			)) {
				NOBADALLOC(m_cont.insert(tc::end(m_cont), tc::begin(rng), tc::end(rng)));
			}

			void chunk(append_detail::conv_enc_needed<tc::range_value_t<Cont>> auto&& rng) const& return_MAYTHROW(
				tc::implicit_cast<void>(tc::for_each(tc::convert_enc<tc::range_value_t<Cont>>(tc_move_if_owned(rng)), *this))
			)

			template<ENABLE_SFINAE> requires std::is_same<tc::range_value_t<SFINAE_TYPE(Cont)&>, unsigned char>::value
			auto write_offset(std::size_t n) const& noexcept {
				struct stream_pos_writer final {
					Cont& m_cont;
					decltype(tc::size_raw(m_cont)) m_pos;

					explicit stream_pos_writer(Cont& cont) noexcept
						: m_cont(cont)
						, m_pos(tc::size_raw(m_cont))
					{}

					void mark() & noexcept {
						// The offset is counted from _after_ the written offset.
						boost::copy(tc::as_blob(tc::explicit_cast<std::uint32_t>(tc::size_raw(m_cont)-m_pos-sizeof(std::uint32_t))), tc::begin_next(m_cont, m_pos));
						m_pos+=sizeof(std::uint32_t);
					}

					void mark_null() & noexcept {
						boost::copy(tc::as_blob(std::numeric_limits<std::uint32_t>::max()), tc::begin_next(m_cont, m_pos));
						m_pos+=sizeof(std::uint32_t);
					}
				};
				stream_pos_writer ow(m_cont);
				tc::for_each(tc::repeat_n(sizeof(std::uint32_t)*n, tc::explicit_cast<unsigned char>(0)), *this);
				return ow;
			}
		};

		template< typename Cont >
		struct [[nodiscard]] appender_type<Cont, /*bReserve*/true> /*final*/: appender_type<Cont, /*bReserve*/false> {
			using base_ = appender_type<Cont, /*bReserve*/false>;
			using base_::base_;
			using base_::chunk;

			// We use int = 0 in parameter list because variation of
			// https://stackoverflow.com/questions/51933397/sfinae-method-completely-disables-base-classs-template-method-in-clang
			template< typename Rng, ENABLE_SFINAE, std::enable_if_t<
				!append_detail::conv_enc_needed<Rng, tc::range_value_t<Cont>> &&
				tc::has_size<Rng> &&
				!append_detail::range_insertable<Rng, Cont>
			>* = nullptr>
			constexpr auto chunk(Rng&& rng, int = 0) const& return_decltype_MAYTHROW(
				tc::cont_reserve(this->m_cont, this->m_cont.size()+tc::size(rng)),
				tc::implicit_cast<void>(tc::for_each(tc_move_if_owned(rng), tc::base_cast</*SFINAE_TYPE to workaround clang bug*/SFINAE_TYPE(base_)>(*this)))
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

	template<typename Rng, typename Cont>
	concept appendable = tc::has_for_each<Rng, tc::appender_t<Cont>>;

	// Disallow 0 == sizeof...(Rng), so that overload taking single argument tc::tuple<Cont, Rng...> is rejected
	template< typename RangeReturn = tc::return_void, typename Cont, tc::appendable<Cont&> Rng>
	constexpr decltype(auto) append(Cont&& cont, Rng&& rng) MAYTHROW {
		static_assert( !std::is_const<Cont>::value, "Cannot append to const container" );
		static_assert( !tc::range_with_iterators<Cont> || std::is_lvalue_reference<Cont>::value, "Append to rvalue intentional?" );

		if constexpr( !tc::range_with_iterators<Cont> || (
			std::is_same<RangeReturn, tc::return_void>::value &&
			noexcept(tc::for_each(tc_move_if_owned(rng), tc::appender(cont)))
		) ) {
			static_assert( std::is_same<RangeReturn, tc::return_void>::value, "RangeReturn not supported, if appending to stream." );

			tc::for_each(tc_move_if_owned(rng), tc::appender(cont));
		} else if constexpr( tc::random_access_range<Cont> || has_mem_fn_reserve<Cont> ) {
			auto const nOffset = tc::size_raw(cont);
			try {
				tc::for_each(tc_move_if_owned(rng), tc::appender(cont));
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
			auto const FirstAppendedElement = [&]() noexcept {
				return it ? tc_modified(it, ++_) : tc::begin(cont);
			};
			try {
				tc::for_each(tc_move_if_owned(rng), tc::appender(cont)); // MAYTHROW
				if constexpr( !std::is_same<RangeReturn, tc::return_void>::value ) {
					return RangeReturn::pack_border(FirstAppendedElement(), cont);
				}
			} catch (...) {
				tc::take_inplace(cont, FirstAppendedElement());
				throw;
			}
		}
	}

	template< typename RangeReturn = tc::return_void, typename Cont, tc::appendable<Cont&>... Rng> requires (1 < sizeof...(Rng))
	constexpr decltype(auto) append(Cont&& cont, Rng&&... rng) MAYTHROW {
		return tc::append<RangeReturn>(tc_move_if_owned(cont), tc::concat(tc_move_if_owned(rng)...));
	}

	namespace no_adl {
		template<typename Cont, typename Rng>
		struct append_on_dtor_t final : tc::noncopyable, tc::inside_unwinding {
			tc::optional<Cont&> m_ocont;
			tc::reference_or_value<Rng> m_rng;

			append_on_dtor_t(Cont& cont, Rng&& rng) noexcept
			: m_ocont(cont), m_rng(tc::aggregate_tag, tc_move_if_owned(rng))
			{}

			append_on_dtor_t(append_on_dtor_t&& other) noexcept
			: m_ocont(tc_move(other).m_ocont), m_rng(tc_move(other).m_rng)
			{
				other.m_ocont = std::nullopt;
			}
			ASSIGN_BY_RENEW(append_on_dtor_t, append_on_dtor_t&&);

			~append_on_dtor_t() MAYTHROW {
				if(m_ocont && !inside_stack_unwinding()) {
					tc::append(*m_ocont, *tc_move(m_rng)); // MAYTHROW
				}
			}
		};
	}

	template<typename Cont, tc::appendable<Cont&> Rng>
	constexpr decltype(auto) make_append_on_dtor(Cont& cont, Rng&& rng) MAYTHROW {
		return no_adl::append_on_dtor_t<Cont, Rng>(cont, tc_move_if_owned(rng));
	}

	namespace explicit_convert_to_container_detail {
		template<typename TTarget, typename Rng0, typename... RngN>
		using use_ctor=tc::constant<
			tc::derived_from<std::remove_cvref_t<Rng0>, TTarget> &&
			(
				0==sizeof...(RngN) ||
			 	(!std::is_reference<Rng0>::value && !std::is_const<Rng0>::value)
			)
		>;
	}

	namespace explicit_convert_adl {
		template<typename Cont>
		concept appendable_container = has_mem_fn_push_back<Cont> || has_emplace_back<Cont, tc::range_value_t<Cont>>::value;

		template<appendable_container TTarget, typename Rng0, tc::appendable<TTarget&>... RngN>
			requires explicit_convert_to_container_detail::use_ctor<TTarget, Rng0, RngN...>::value
		constexpr TTarget explicit_convert_impl(adl_tag_t, std::type_identity<TTarget>, Rng0&& rng0, RngN&&... rngN) MAYTHROW {
			if constexpr(0<sizeof...(RngN)) {
				TTarget cont=tc_move_if_owned(rng0);
 				tc::append(cont, tc_move_if_owned(rngN)...);
				return cont;
			} else {
				return tc_move_if_owned(rng0);
			}
		}

		template<appendable_container TTarget, tc::appendable<TTarget&> Rng0, tc::appendable<TTarget&>... RngN>
			requires (!explicit_convert_to_container_detail::use_ctor<TTarget, Rng0, RngN...>::value)
		constexpr TTarget explicit_convert_impl(adl_tag_t, std::type_identity<TTarget>, Rng0&& rng0, RngN&&... rngN) MAYTHROW {
			TTarget cont;
 			tc::append(cont, tc_move_if_owned(rng0), tc_move_if_owned(rngN)...);
			return cont;
		}
	}

	template< typename... Rng >
	[[nodiscard]] auto make_vector(Rng&&... rng) MAYTHROW {
		static_assert(0 < sizeof...(Rng));
		return tc::explicit_cast<tc::vector<tc::range_value_t<decltype(tc::concat(tc_move_if_owned(rng)...))>>>(tc_move_if_owned(rng)...);
	}

	template< typename Char, typename... Rng >
	[[nodiscard]] auto make_str(Rng&&... rng) MAYTHROW {
		static_assert(0 < sizeof...(Rng));
		return tc::explicit_cast<tc::string<Char>>(tc_move_if_owned(rng)...);
	}

	template< typename... Rng >
	[[nodiscard]] auto make_str(Rng&&... rng) MAYTHROW {
		static_assert(0 < sizeof...(Rng));
		return tc::make_str<tc::range_value_t<decltype(tc::concat(tc_move_if_owned(rng)...))>>(tc_move_if_owned(rng)...);
	}

	template< typename T, typename Rng >
	[[nodiscard]] auto make_unique_unordered_set(Rng&& rng) MAYTHROW {
		tc::unordered_set<T> set;
		tc::cont_try_insert_range(set, tc_move_if_owned(rng));
		return set;
	}

	template< typename Rng >
	[[nodiscard]] auto make_unique_unordered_set(Rng&& rng) MAYTHROW {
		return make_unique_unordered_set<tc::range_value_t<Rng>>(tc_move_if_owned(rng));
	}

	template< typename T, typename Rng >
	[[nodiscard]] auto make_unordered_set(Rng&& rng) MAYTHROW {
		tc::unordered_set<T> set;
		tc::cont_must_insert_range(set, tc_move_if_owned(rng));
		return set;
	}

	template< typename Rng >
	[[nodiscard]] auto make_unordered_set(Rng&& rng) MAYTHROW {
		return make_unordered_set<tc::range_value_t<Rng>>(tc_move_if_owned(rng));
	}
}
