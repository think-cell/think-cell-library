
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/utility.h"

#include "../range/index_range.h"
#include "../range/meta.h"
#include "../tuple.h"
#include "../base/modified.h"

#include "break_or_continue.h"


namespace tc {
	namespace no_adl {
		template<typename... T>
		struct common_type_break_or_continue;

		template<>
		struct common_type_break_or_continue<> final {
			using type = tc::constant<tc::continue_>;
		};

		template<typename... T>
		struct common_type_break_or_continue<tc::constant<tc::continue_>, T...> {
			using type = typename common_type_break_or_continue<T...>::type;
		};

		template<typename... T>
		struct common_type_break_or_continue<tc::constant<tc::break_>, T...> {
			using type = tc::constant<tc::break_>;
		};

		template<typename... T>
		struct common_type_break_or_continue<tc::break_or_continue, T...> {
			using type = std::conditional_t<
				std::is_same<tc::constant<tc::break_>, typename common_type_break_or_continue<T...>::type>::value,
				tc::constant<tc::break_>,
				tc::break_or_continue
			>;
		};
	}
	template<typename... T>
	using common_type_break_or_continue_t = typename no_adl::common_type_break_or_continue<T...>::type;

	template<typename Sink, typename Rng>
	concept has_mem_fn_chunk = requires { std::declval<Sink>().chunk(std::declval<Rng>()); };

	DEFINE_MEM_FN(chunk);

	namespace void_generator_type_check_no_adl {
		template<typename BreakOrContinue, tc::break_or_continue>
		struct check_sink_result;

		template<typename BreakOrContinue>
		struct check_sink_result<BreakOrContinue, tc::continue_> {
			static_assert(
				!std::is_same<BreakOrContinue, tc::break_or_continue>::value &&
				!std::is_same<BreakOrContinue, tc::constant<tc::break_>>::value,
				"Functor may return break_, but range does not support it."
			);
		};

		template<typename BreakOrContinue>
		struct check_sink_result<BreakOrContinue, tc::break_> {
			static_assert(
				std::is_same<BreakOrContinue, tc::constant<tc::break_>>::value,
				"Functor does not always break, but range does (broken range implementation)."
			);
		};

		template<tc::break_or_continue boc, typename Sink>
		struct verify_sink_result_impl final : Sink {
			static_assert(tc::decayed<Sink>);
			using guaranteed_break_or_continue = tc::constant<boc>;

			template<typename... Args, typename R = decltype(std::declval<Sink const&>()(std::declval<Args>()...))>
			constexpr tc::constant<boc> operator()(Args&&... args) const& noexcept(noexcept(
				tc::base_cast<Sink>(*this)(std::declval<Args&&>()...)
			)) {
				check_sink_result<R, boc>();
				tc::base_cast<Sink>(*this)(std::forward<Args>(args)...); // MAYTHROW
				return {};
			}

			template<typename Rng> requires tc::has_mem_fn_chunk<Sink const&, Rng>
			constexpr tc::constant<boc> chunk(Rng&& rng) const& noexcept(noexcept(
				tc::base_cast<Sink>(*this).chunk(std::forward<Rng>(rng))
			)) {
				check_sink_result<decltype(tc::base_cast<Sink>(*this).chunk(std::forward<Rng>(rng))), boc>();
				tc::base_cast<Sink>(*this).chunk(std::forward<Rng>(rng)); // MAYTHROW
				return {};
			}
		};

		template<typename Sink, typename = void>
		struct guaranteed_break_or_continue final {
			using type = tc::break_or_continue;
		};

		template<typename Sink>
		struct guaranteed_break_or_continue<Sink, tc::void_t<typename std::remove_reference_t<Sink>::guaranteed_break_or_continue>> final {
			using type = typename std::remove_reference_t<Sink>::guaranteed_break_or_continue;
		};
	}
	template<typename Sink>
	using guaranteed_break_or_continue_t = typename void_generator_type_check_no_adl::guaranteed_break_or_continue<Sink>::type;

	template<typename BreakOrContinue, typename Sink>
		requires std::is_same<BreakOrContinue, tc::break_or_continue>::value
			|| std::is_same<BreakOrContinue, tc::guaranteed_break_or_continue_t<Sink>>::value
	constexpr decltype(auto) verify_sink_result(Sink&& sink) noexcept {
		return std::forward<Sink>(sink);
	}

	template<typename BreakOrContinue, typename Sink>
	constexpr void_generator_type_check_no_adl::verify_sink_result_impl<BreakOrContinue::value, tc::decay_t<Sink>> verify_sink_result(Sink&& sink) noexcept {
		static_assert( std::is_same<guaranteed_break_or_continue_t<Sink>, tc::break_or_continue>::value, "Mismatch between range and sink result types (broken range implementation)." );
		return {std::forward<Sink>(sink)};
	}

	//-------------------------------------------------------------------------------------------------------------------------
	// for_each

	namespace for_each_adl {
		DEFINE_ADL_TAG_TYPE(adl_tag)
	}
	template<typename> \
	void for_each_impl() noexcept; /*TODO c++20: workaround for c++17*/

	namespace for_each_detail {
		TC_DEFINE_ENUM(EOverload, eoverload, (CHUNK)(ADL)(INVOKERNG)(ADLTAG)(INDEX)(ITERATOR)(NONE))

		namespace no_adl {
			template<typename BreakOrContinue>
			struct make_break_or_continue;
			template<> struct make_break_or_continue<tc::constant<tc::continue_>> { using type = tc::constant<tc::continue_>; };
			template<> struct make_break_or_continue<tc::constant<tc::break_>> { using type = tc::constant<tc::break_>; };
			template<> struct make_break_or_continue<tc::break_or_continue> { using type = tc::break_or_continue; };
			template<> struct make_break_or_continue<void> { using type = tc::constant<tc::continue_>; };
		}
		template<typename BreakOrContinue>
		using make_break_or_continue_t = typename no_adl::make_break_or_continue<BreakOrContinue>::type;

		namespace no_adl {
			template<typename Rng, typename Sink, int nPriority = 0>
			struct select_overload : select_overload<Rng, Sink, nPriority + 1> {};

#pragma push_macro("SELECT_OVERLOAD_IF")
#define SELECT_OVERLOAD_IF(priority, eoverload, requires_clause) \
			template<typename Rng, typename Sink> requires_clause \
			struct select_overload<Rng, Sink, priority> : tc::constant<eoverload> {};

			SELECT_OVERLOAD_IF(0, eoverloadCHUNK, TC_FWD(requires tc::has_mem_fn_chunk<tc::decay_t<Sink> const&, Rng>))
			SELECT_OVERLOAD_IF(1, eoverloadADL, TC_FWD(requires requires { for_each_impl(std::declval<Rng>(), std::declval<Sink>()); }))
			SELECT_OVERLOAD_IF(1, eoverloadINVOKERNG, TC_FWD(requires requires { typename make_break_or_continue_t<decltype(std::declval<Rng>()(std::declval<Sink>()))>; }))
			SELECT_OVERLOAD_IF(2, eoverloadADLTAG, TC_FWD(requires requires { for_each_impl(for_each_adl::adl_tag, std::declval<Rng>(), std::declval<Sink>()); }))
			SELECT_OVERLOAD_IF(2, eoverloadINDEX, requires tc::has_index<std::remove_reference_t<Rng>>)
			SELECT_OVERLOAD_IF(3, eoverloadITERATOR, requires tc::range_with_iterators<Rng>)
			SELECT_OVERLOAD_IF(4, eoverloadNONE, /*none*/)
#pragma pop_macro("SELECT_OVERLOAD_IF")
		}
		using no_adl::select_overload;
	}

	template<typename Rng, typename Sink> requires (for_each_detail::eoverloadCHUNK == for_each_detail::select_overload<Rng, Sink>::value)
	constexpr auto for_each(Rng&& rng, Sink&& sink) return_MAYTHROW(
		tc_internal_continue_if_not_break(/*do we really need that copy?*/tc::as_const(tc::as_lvalue(tc::decay_copy(sink))).chunk(std::forward<Rng>(rng)))
	)

	template<typename Rng, typename Sink> requires (for_each_detail::eoverloadADL == for_each_detail::select_overload<Rng, Sink>::value)
	constexpr auto for_each(Rng&& rng, Sink&& sink) return_MAYTHROW(
		for_each_impl(std::forward<Rng>(rng), std::forward<Sink>(sink))
	)

	template<typename Rng, typename Sink> requires (for_each_detail::eoverloadADLTAG == for_each_detail::select_overload<Rng, Sink>::value)
	constexpr auto for_each(Rng&& rng, Sink&& sink) return_MAYTHROW(
		for_each_impl(for_each_adl::adl_tag, std::forward<Rng>(rng), std::forward<Sink>(sink))
	)

	template<typename Rng, typename Sink> requires (for_each_detail::eoverloadINVOKERNG == for_each_detail::select_overload<Rng, Sink>::value)
	constexpr auto for_each(Rng&& rng, Sink&& sink) return_MAYTHROW(
		tc_internal_continue_if_not_break(std::forward<Rng>(rng)(tc::verify_sink_result<for_each_detail::make_break_or_continue_t<decltype(std::declval<Rng>()(std::declval<Sink>()))>>(std::forward<Sink>(sink))))
	)

	template<typename Rng, typename Sink, /*not requires because of CWG issue 2369*/std::enable_if_t<for_each_detail::eoverloadINDEX == for_each_detail::select_overload<Rng, Sink>::value>* = nullptr>
	constexpr auto for_each(Rng&& rng, Sink&& sink_) noexcept(
		noexcept(rng.at_end_index(tc::as_lvalue(tc::decay_copy(rng.begin_index())))) &&
		noexcept(rng.increment_index(tc::as_lvalue(rng.begin_index()))) &&
		noexcept(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), rng.dereference_index(tc::as_lvalue(rng.begin_index()))))
	) -> tc::common_type_t<decltype(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), rng.dereference_index(tc::as_lvalue(rng.begin_index())))), tc::constant<tc::continue_>> {
		tc::decay_t<Sink> const sink = sink_; // do we really need that copy?
		for (auto i = rng.begin_index(); !rng.at_end_index(i); rng.increment_index(i)) {
			tc_yield(sink, rng.dereference_index(i));
		}
		return tc::constant<tc::continue_>();
	}

	template<typename Rng, typename Sink, /*not requires because of CWG issue 2369*/std::enable_if_t<for_each_detail::eoverloadITERATOR == for_each_detail::select_overload<Rng, Sink>::value>* = nullptr>
	constexpr auto for_each(Rng&& rng, Sink&& sink_) noexcept(
		noexcept(tc::end(rng)) && 
		noexcept(++tc::as_lvalue(tc::begin(rng))) && 
		noexcept(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), *tc::as_lvalue(tc::begin(rng))))
	) -> tc::common_type_t<decltype(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), *tc::as_lvalue(tc::begin(rng)))), tc::constant<tc::continue_>> {
		tc::decay_t<Sink> const sink = sink_; // do we really need that copy?
		auto const itEnd = tc::end(rng);
		for(auto it = tc::begin(rng); it!= itEnd; ++it) {
			tc_yield(sink, *it);
		}
		return tc::constant<tc::continue_>();
	}

	namespace for_each_detail {
		template<typename... T, typename Sink, typename R = typename tc::common_type_break_or_continue_t<decltype(tc::continue_if_not_break(std::declval<Sink>(), T()))...>>
		constexpr R for_each_parameter_pack(tc::type::list<T...>, Sink const sink) MAYTHROW /*ICE on MSVC 2017: noexcept((noexcept(tc::continue_if_not_break(sink, T())) && ...))*/ {
			if constexpr (std::is_same<tc::constant<tc::continue_>, R>::value) {
				(sink(T()), ...); // plain call, tc::invoke on std::integral_constant or tc::type has no value.
				return tc::constant<tc::continue_>();
			} else {
				auto const boc = tc::continue_if(((tc::continue_ == tc_internal_continue_if_not_break(sink(T()))) && ...));

				if constexpr (std::is_same<tc::constant<tc::break_>, R>::value) {
					return tc::constant<tc::break_>();
				} else {
					return boc;
				}
			}
		}

		namespace no_adl {
			template<typename IntSeq>
			struct integer_sequence_to_type_list;

			template<typename TIndex, TIndex... Is>
			struct integer_sequence_to_type_list<std::integer_sequence<TIndex, Is...>> {
				using type = tc::type::list<tc::constant<Is>...>;
			};

			template<typename Tuple, typename Sink>
			struct tuple_index_sink final {
				Tuple&& m_tuple;
				Sink m_sink;
				template<std::size_t I>
				constexpr auto operator()(tc::constant<I>) const& return_decltype_MAYTHROW(
					tc::invoke(m_sink, tc::get<I>(std::forward<Tuple>(m_tuple)))
				)
			};
		}
		using no_adl::integer_sequence_to_type_list;
		using no_adl::tuple_index_sink;
	}

	namespace for_each_adl {
		template<typename TIndex, TIndex... Is, typename Sink>
		constexpr auto for_each_impl(adl_tag_t, std::integer_sequence<TIndex, Is...>, Sink&& sink) return_decltype_MAYTHROW(
			for_each_detail::for_each_parameter_pack(tc::type::list<tc::constant<Is>...>(), std::forward<Sink>(sink))
		)

		template<typename... Ts, typename Sink>
		constexpr auto for_each_impl(adl_tag_t, tc::type::list<Ts...>, Sink&& sink) return_decltype_MAYTHROW(
			for_each_detail::for_each_parameter_pack(tc::type::list<tc::type::identity<Ts>...>(), std::forward<Sink>(sink))
		)

		template<typename Tuple, typename Sink,
			std::enable_if_t<tc::instance_or_derived<std::remove_reference_t<Tuple>, std::tuple>>* = nullptr,
			typename IndexList = typename for_each_detail::integer_sequence_to_type_list<std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple>>::value>>::type
		>
		constexpr auto for_each_impl(adl_tag_t, Tuple&& tuple, Sink&& sink) return_decltype_MAYTHROW(
			for_each_detail::for_each_parameter_pack(IndexList(), for_each_detail::tuple_index_sink<Tuple, tc::decay_t<Sink>>{std::forward<Tuple>(tuple), std::forward<Sink>(sink)})
		)

		template<typename Rng, typename Sink, /*not requires because of CWG issue 2369*/std::enable_if_t<!std::is_reference<Rng>::value>* = nullptr>
		constexpr auto for_each_impl(adl_tag_t, Rng&& rng, Sink&& sink_) noexcept(
			noexcept(tc::end(rng)) &&
			noexcept(++tc::as_lvalue(tc::begin(rng))) &&
			noexcept(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), tc_move_always(rng.extract(tc::begin(rng)).value())))
		) -> tc::common_type_t<decltype(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), tc_move_always(rng.extract(tc::begin(rng)).value()))), tc::constant<tc::continue_>> {
			tc::decay_t<Sink> const sink = sink_; // do we really need that copy?

			auto it = tc::begin(rng);
			auto const itEnd = tc::end(rng);
			while (it != itEnd) {
				auto itNext = tc_modified(it, ++_);
				tc_yield(sink, tc_move_always(rng.extract(it).value()));
				it = itNext;
			}
			return tc::constant<tc::continue_>();
		}
	}

	namespace range_output_tuple_impl {
		template<template<typename...> typename TupleT, typename Tuple>
		using type = tc::type::unique_t<tc::type::transform_t<
			tc::type::transform_t<
				typename tc::is_instance_or_derived<std::remove_reference_t<Tuple>, TupleT>::arguments,
				tc::type::rcurry<tc::apply_cvref_t, Tuple>::template type
			>,
			tc::remove_rvalue_reference_t
		>>;
	}

	namespace range_output_t_adl {
		template<typename... Ts>
		auto range_output_t_impl(adl_tag_t, tc::type::list<Ts...>) -> tc::type::unique_t<tc::type::list<tc::type::identity<Ts>...>>; // declaration only

		template<typename Tuple>
		auto range_output_t_impl(adl_tag_t, Tuple&&) -> range_output_tuple_impl::type<std::tuple, Tuple&&>; // declaration only
	}

	namespace tuple_adl {
		template<typename Tuple, typename Sink, typename IndexSeq = std::make_index_sequence<std::remove_reference_t<Tuple>::tc_tuple_impl::size>>
		constexpr auto for_each_impl(Tuple&& tuple, Sink&& sink) return_decltype_MAYTHROW(
			tc::for_each(
				IndexSeq(),
				for_each_detail::tuple_index_sink<Tuple, tc::decay_t<Sink>>{std::forward<Tuple>(tuple), std::forward<Sink>(sink)}
			)
		)

		template<typename Tuple, typename Sink, typename ReverseIndexSeq = tc::make_reverse_integer_sequence<std::size_t, 0, std::remove_reference_t<Tuple>::tc_tuple_impl::size>>
		constexpr auto for_each_reverse_impl(Tuple&& tuple, Sink&& sink) return_decltype_MAYTHROW(
			tc::for_each(
				ReverseIndexSeq(),
				for_each_detail::tuple_index_sink<Tuple, tc::decay_t<Sink>>{std::forward<Tuple>(tuple), std::forward<Sink>(sink)}
			)
		)

		template<typename Tuple>
		auto range_output_t_impl(Tuple&&) -> range_output_tuple_impl::type<tc::tuple, Tuple&&> {} // unevaluated
	}

	TC_HAS_EXPR(for_each, (Rng)(Sink), tc::for_each(std::declval<Rng>(), std::declval<Sink>()))

	template<typename... Fn>
	[[nodiscard]] constexpr auto break_or_continue_sequence(Fn&&... fn) MAYTHROW {
		return tc::for_each(tc::forward_as_tuple(std::forward<Fn>(fn)...), [](auto fn) MAYTHROW {
			return fn(); // MAYTHROW
		});
	}
}

#define TC_BREAK_OR_CONTINUE_SEQUENCE_IMPL(...) ([&]() MAYTHROW { return TC_EXPAND(__VA_ARGS__); })

#define tc_break_or_continue_sequence(exprseq) \
	tc::break_or_continue_sequence(TC_PP_ENUM_TRANSFORMED_SEQ(TC_PP_APPLY_MACRO, TC_BREAK_OR_CONTINUE_SEQUENCE_IMPL, exprseq))
