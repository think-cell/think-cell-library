
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "return_decltype.h"
#include "tag_type.h"
#include "utility.h"
#include "explicit_cast_fwd.h"

#include <tuple>
#include <type_traits>

namespace tc {
	namespace invoke_no_adl {
		//////////////////////////////////////////////////////////////////////////
		// expanded

		template<typename Arg>
		struct expanded final {
			using arguments = tc::type::list<Arg>;

			template<std::size_t I>
			static constexpr decltype(auto) select(Arg&& arg) noexcept {
				static_assert(0 == I);
				return tc_move_if_owned(arg);
			}
		};

		template<tuple_like Tuple, typename IndexSeq>
		struct expanded_tuple;

		template<tuple_like Tuple, std::size_t... I>
		struct expanded_tuple<Tuple, std::index_sequence<I...>> {
			using arguments = tc::type::list<decltype(tc::get<I>(std::declval<Tuple>()))...>;
		};

		template<tuple_like Tuple>
		struct expanded<Tuple> final : expanded_tuple<Tuple, std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple>>::value>> {
			template<std::size_t I>
			static constexpr decltype(auto) select(Tuple&& tpl) noexcept {
				return tc::get<I>(tc_move_if_owned(tpl));
			}
		};

		struct expanded_argument_source_index final {
			std::size_t m_nArg;
			std::size_t m_nElement;
		};

		template<std::size_t c_nExpandedTotal>
		struct expanded_argument_source_indices_value final {
			expanded_argument_source_index m_asrcidx[c_nExpandedTotal] = {};

			constexpr explicit expanded_argument_source_indices_value(std::size_t const anExpandedCount[]) noexcept {
				expanded_argument_source_index srcidx = {0, 0};

				for( std::size_t i = 0; i < c_nExpandedTotal; ++i, ++srcidx.m_nElement ) {
					while( anExpandedCount[srcidx.m_nArg] <= srcidx.m_nElement ) {
						srcidx.m_nElement -= anExpandedCount[srcidx.m_nArg++];
					}
					m_asrcidx[i] = srcidx;
				}
			}
		};

		template<>
		struct expanded_argument_source_indices_value<0> final {
			constexpr explicit expanded_argument_source_indices_value(std::size_t const /*anExpandedCount*/[]) noexcept {};
		};

		template<std::size_t... nExpandedCount>
		struct expanded_argument_source_indices final {
#ifndef _MSC_VER
			static constexpr std::size_t c_nExpandedTotal = (nExpandedCount + ...);
#else
			static constexpr std::size_t variadic_sum_workaround() noexcept {
				return (nExpandedCount + ...);
			}
			static constexpr std::size_t c_nExpandedTotal = variadic_sum_workaround();
#endif
		private:
			static constexpr std::size_t c_anExpandedCount[sizeof...(nExpandedCount)] = { nExpandedCount... };
		public:
			static constexpr expanded_argument_source_indices_value<c_nExpandedTotal> value{c_anExpandedCount};
		};

		template<>
		struct expanded_argument_source_indices<> final {
			static constexpr std::size_t c_nExpandedTotal = 0;
		};

		template<typename... Args>
		struct expanded_arguments final {
		private:
			using source_indices = expanded_argument_source_indices<tc::type::size<typename expanded<Args>::arguments>::value...>;
			template<std::size_t nExpandedIndex, typename Arg>
			static constexpr decltype(auto) select_elem(Arg&& arg) noexcept {
				return expanded<Arg>::template select<source_indices::value.m_asrcidx[nExpandedIndex].m_nElement>(tc_move_if_owned(arg));
			}

		public:
			using index_sequence = std::make_index_sequence<source_indices::c_nExpandedTotal>;

			template<std::size_t nExpandedIndex>
			static constexpr decltype(auto) select(Args&&... args) noexcept {
				return select_elem<nExpandedIndex>(tc::select_nth<source_indices::value.m_asrcidx[nExpandedIndex].m_nArg>(tc_move_if_owned(args)...));
			}
		};

		//////////////////////////////////////////////////////////////////////////
		// invoker

		template <typename, typename Func, typename... Args>
		struct is_directly_invocable final : tc::constant<false> {};

		template <typename Func, typename... Args>
		struct is_directly_invocable<tc::void_t<decltype(std::declval<Func>()(std::declval<Args>()...))>, Func, Args...> final : tc::constant<true> {};
	}

	template <typename Func, typename... Args, std::enable_if_t<invoke_no_adl::is_directly_invocable<void, Func, Args...>::value>* = nullptr>
	constexpr auto invoke(Func&& func, Args&&... args) return_decltype_xvalue_by_ref_MAYTHROW(
		// We do not care for pointer to member function and pointer to data member, so we skip std::invoke.
		tc_move_if_owned(func)(tc_move_if_owned(args)...) // MAYTHROW
	)

	namespace expanding_invoke_adl {
		DEFINE_ADL_TAG_TYPE(expand_tag);
	}

	template <
		typename Func,
		typename... Args,
		std::enable_if_t<
			!invoke_no_adl::is_directly_invocable<void, Func, Args...>::value
			&& !std::is_same<tc::type::list<Args...>, tc::type::concat_t<typename invoke_no_adl::expanded<Args>::arguments...>>::value
		>* = nullptr
	>
	constexpr auto invoke(Func&& func, Args&&... args) return_decltype_xvalue_by_ref_MAYTHROW(
		expanding_invoke_impl( // use ADL to delay lookup of expanding_invoke_impl to point of instantiation
			expanding_invoke_adl::expand_tag_t(),
			typename invoke_no_adl::expanded_arguments<Args...>::index_sequence(),
			tc_move_if_owned(func),
			tc_move_if_owned(args)...
		) // recursive MAYTHROW
	)

	namespace expanding_invoke_adl {
		template <std::size_t... nExpandedIndex, typename Func, typename... Args>
		static constexpr auto expanding_invoke_impl( expand_tag_t, std::index_sequence<nExpandedIndex...>, Func&& func, Args&&... args) return_decltype_xvalue_by_ref_MAYTHROW(
			tc::invoke(
				tc_move_if_owned(func),
				invoke_no_adl::expanded_arguments<Args...>::template select<nExpandedIndex>(tc_move_if_owned(args)...)...
			) // recursive MAYTHROW
		)
	}

	//////////////////////////////////////////////////////////////////////////
	// concepts
	template <typename Func, typename... Args>
	concept invocable = requires(Func&& f, Args&&... args) { tc::invoke(tc_move_if_owned(f), tc_move_if_owned(args)...); };
	template <typename Func, typename... Args>
	concept nothrow_invocable = tc::invocable<Func, Args...> && noexcept(tc::invoke(std::declval<Func>(), std::declval<Args>()...));

	template <typename Func, typename T>
	concept predicate = tc::invocable<Func const&, std::remove_cvref_t<T> const&>
		&& requires(Func const& f, std::remove_cvref_t<T> const& t) { tc::explicit_cast<bool>(tc::invoke(f, t)); };
	template <typename Func, typename T>
	concept nothrow_predicate = tc::predicate<Func, T> && tc::nothrow_invocable<Func const&, std::remove_cvref_t<T> const&>;

	template <typename Func, typename T>
	concept constant_predicate = tc::predicate<Func, T>
		&& requires(Func const& f, std::remove_cvref_t<T> const& t) { tc::explicit_cast<bool>(decltype(tc::invoke(f, t))::value); };

	template <typename Func, typename T>
	concept constant_predicate_true = tc::constant_predicate<Func, T>
		&& tc::explicit_cast<bool>(decltype(tc::invoke(std::declval<Func const&>(), std::declval<std::remove_cvref_t<T> const&>()))::value);
	template <typename Func, typename T>
	concept constant_predicate_false = tc::constant_predicate<Func, T>
		&& !tc::explicit_cast<bool>(decltype(tc::invoke(std::declval<Func const&>(), std::declval<std::remove_cvref_t<T> const&>()))::value);

	template <typename Func, typename T>
	concept runtime_predicate = tc::predicate<Func, T> && (!tc::constant_predicate<Func, T>);
}
