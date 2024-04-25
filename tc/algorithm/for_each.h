
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
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
				tc::base_cast<Sink>(*this)(tc_move_if_owned(args)...); // MAYTHROW
				return {};
			}

			template<typename Rng> requires tc::has_mem_fn_chunk<Sink const&, Rng>
			constexpr tc::constant<boc> chunk(Rng&& rng) const& noexcept(noexcept(
				tc::base_cast<Sink>(*this).chunk(tc_move_if_owned(rng))
			)) {
				check_sink_result<decltype(tc::base_cast<Sink>(*this).chunk(tc_move_if_owned(rng))), boc>();
				tc::base_cast<Sink>(*this).chunk(tc_move_if_owned(rng)); // MAYTHROW
				return {};
			}
		};

		template<typename Sink>
		struct guaranteed_break_or_continue final {
			using type = tc::break_or_continue;
		};

		template<typename Sink> requires requires { typename std::remove_reference_t<Sink>::guaranteed_break_or_continue; }
		struct guaranteed_break_or_continue<Sink> final {
			using type = typename std::remove_reference_t<Sink>::guaranteed_break_or_continue;
		};
	}
	template<typename Sink>
	using guaranteed_break_or_continue_t = typename void_generator_type_check_no_adl::guaranteed_break_or_continue<Sink>::type;

	template<typename BreakOrContinue, typename Sink>
		requires std::is_same<BreakOrContinue, tc::break_or_continue>::value
			|| std::is_same<BreakOrContinue, tc::guaranteed_break_or_continue_t<Sink>>::value
	constexpr decltype(auto) verify_sink_result(Sink&& sink) noexcept {
		return tc_move_if_owned(sink);
	}

	template<typename BreakOrContinue, typename Sink>
	constexpr void_generator_type_check_no_adl::verify_sink_result_impl<BreakOrContinue::value, tc::decay_t<Sink>> verify_sink_result(Sink&& sink) noexcept {
		static_assert( std::is_same<guaranteed_break_or_continue_t<Sink>, tc::break_or_continue>::value, "Mismatch between range and sink result types (broken range implementation)." );
		return {tc_move_if_owned(sink)};
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
		tc_internal_continue_if_not_break(/*do we really need that copy?*/tc::as_const(tc::as_lvalue(tc::decay_copy(sink))).chunk(tc_move_if_owned(rng)))
	)

	template<typename Rng, typename Sink> requires (for_each_detail::eoverloadADL == for_each_detail::select_overload<Rng, Sink>::value)
	constexpr auto for_each(Rng&& rng, Sink&& sink) return_MAYTHROW(
		for_each_impl(tc_move_if_owned(rng), tc_move_if_owned(sink))
	)

	template<typename Rng, typename Sink> requires (for_each_detail::eoverloadADLTAG == for_each_detail::select_overload<Rng, Sink>::value)
	constexpr auto for_each(Rng&& rng, Sink&& sink) return_MAYTHROW(
		for_each_impl(for_each_adl::adl_tag, tc_move_if_owned(rng), tc_move_if_owned(sink))
	)

	template<typename Rng, typename Sink> requires (for_each_detail::eoverloadINVOKERNG == for_each_detail::select_overload<Rng, Sink>::value)
	constexpr auto for_each(Rng&& rng, Sink&& sink) return_MAYTHROW(
		tc_internal_continue_if_not_break(tc_move_if_owned(rng)(tc::verify_sink_result<for_each_detail::make_break_or_continue_t<decltype(std::declval<Rng>()(std::declval<Sink>()))>>(tc_move_if_owned(sink))))
	)

	template<typename Rng, typename Sink, /*not requires because of CWG issue 2369*/std::enable_if_t<for_each_detail::eoverloadINDEX == for_each_detail::select_overload<Rng, Sink>::value>* = nullptr>
	constexpr auto for_each(Rng&& rng, Sink&& sink_) noexcept(
		noexcept(rng.at_end_index(tc::as_lvalue(tc::decay_copy(rng.begin_index())))) &&
		noexcept(rng.increment_index(tc::as_lvalue(rng.begin_index()))) &&
		noexcept(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), rng.dereference_index(tc::as_lvalue(rng.begin_index()))))
	) -> tc::common_type_t<decltype(tc::continue_if_not_break(std::declval<tc::decay_t<Sink> const&>(), rng.dereference_index(tc::as_lvalue(rng.begin_index())))), tc::constant<tc::continue_>> {
		tc::decay_t<Sink> const sink = sink_; // do we really need that copy?
		for (auto i = rng.begin_index(); !rng.at_end_index(i); rng.increment_index(i)) {
			tc_return_if_break(tc::continue_if_not_break(sink, rng.dereference_index(i)))
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
			tc_return_if_break(tc::continue_if_not_break(sink, *it))
		}
		return tc::constant<tc::continue_>();
	}

	namespace for_each_detail {
		template<typename T>
		constexpr auto type_value = std::type_identity<T>{};

		template<auto Value>
		constexpr auto type_value<tc::constant<Value>> = tc::constant<Value>{};

		// If the type is a type list, we turn it into a tuple of type values.
		// That way `tc::for_each(tc::zip(list1, list2), []<typename U, typename V>(std::type_identity<U>, std::type_identity<V>) { ... })` just works.
		template<typename ... T>
		constexpr auto type_value<boost::mp11::mp_list<T...>> = tc::make_tuple(type_value<T>...);

		template<typename... T, typename Sink, typename R = typename tc::common_type_break_or_continue_t<decltype(tc::continue_if_not_break(std::declval<Sink>(), type_value<T>))...>>
		constexpr R for_each_parameter_pack(boost::mp11::mp_list<T...>, Sink const sink) noexcept((noexcept(tc::continue_if_not_break(sink, type_value<T>)) && ...)) {
			if constexpr (std::is_same<tc::constant<tc::continue_>, R>::value) {
				(tc_invoke(sink, type_value<T>), ...);
				return tc::constant<tc::continue_>();
			} else {
				auto const boc = tc::continue_if(((tc::continue_ == tc_internal_continue_if_not_break(tc_invoke(sink, type_value<T>))) && ...));

				if constexpr (std::is_same<tc::constant<tc::break_>, R>::value) {
					return tc::constant<tc::break_>();
				} else {
					return boc;
				}
			}
		}

		namespace no_adl {
			template<typename Tuple, typename Sink>
			struct tuple_index_sink final {
				Tuple&& m_tuple;
				Sink m_sink;
				template<std::size_t I>
				constexpr auto operator()(tc::constant<I>) const& return_decltype_MAYTHROW(
					tc_invoke(m_sink, tc::get<I>(tc_move_if_owned(m_tuple)))
				)
			};
		}
		using no_adl::tuple_index_sink;
	}

	namespace for_each_adl {
		template<typename TIndex, TIndex... Is, typename Sink>
		constexpr auto for_each_impl(adl_tag_t, std::integer_sequence<TIndex, Is...>, Sink&& sink) return_decltype_MAYTHROW(
			for_each_detail::for_each_parameter_pack(boost::mp11::mp_list<tc::constant<Is>...>(), tc_move_if_owned(sink))
		)

		template<typename... Ts, typename Sink>
		constexpr auto for_each_impl(adl_tag_t, boost::mp11::mp_list<Ts...>, Sink&& sink) return_decltype_MAYTHROW(
			for_each_detail::for_each_parameter_pack(boost::mp11::mp_list<Ts...>(), tc_move_if_owned(sink))
		)

		template<tc::tuple_like Tuple, typename Sink,
			typename IndexList = tc::mp_integer_list<std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple>>::value>>
			TC_REQUIRES_CWG2369_WORKAROUND(!tc::range_with_iterators<Tuple>)
		constexpr auto for_each_impl(adl_tag_t, Tuple&& tuple, Sink&& sink) return_decltype_MAYTHROW(
			for_each_detail::for_each_parameter_pack(IndexList(), for_each_detail::tuple_index_sink<Tuple, tc::decay_t<Sink>>{tc_move_if_owned(tuple), tc_move_if_owned(sink)})
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
				tc_return_if_break(tc::continue_if_not_break(sink, tc_move_always(rng.extract(it).value())))
				it = itNext;
			}
			return tc::constant<tc::continue_>();
		}
	}

	namespace range_output_tuple_impl::no_adl {
		struct fn_range_output final {
			template<typename... Ts> boost::mp11::mp_unique<boost::mp11::mp_list<Ts...>> operator()(Ts&&...) const& {
				return {}; // unevaluated, not declaration only because of return_invoke workaround
			}
		};
	}

	namespace range_output_t_adl {
		template<typename... Ts>
		auto range_output_t_impl(adl_tag_t, boost::mp11::mp_list<Ts...>)
			-> boost::mp11::mp_unique<boost::mp11::mp_list<std::remove_const_t<decltype(for_each_detail::type_value<Ts>)>...>>; // declaration only

		template<tc::tuple_like Tuple TC_REQUIRES_CWG2369_WORKAROUND(!tc::range_with_iterators<Tuple>)
		auto range_output_t_impl(adl_tag_t, Tuple&& tpl)
			-> decltype(tc_apply(range_output_tuple_impl::no_adl::fn_range_output(), std::declval<Tuple>())); // declaration only
	}
	
	namespace make_lazy_adl {
		template<typename Lazy, typename Sink,
			std::enable_if_t<tc::instance<std::remove_reference_t<Lazy>, tc::make_lazy>>* = nullptr
		>
		constexpr auto for_each_impl(Lazy&& lazy, Sink&& sink) return_decltype_MAYTHROW(
			tc::for_each(tc_move_if_owned(lazy)(),tc_move_if_owned(sink))
		)
	}

	TC_HAS_EXPR(for_each, (Rng)(Sink), tc::for_each(std::declval<Rng>(), std::declval<Sink>()))
}

#define TC_RETURN_BREAK_OR_CONTINUE_SEQUENCE_TYPE(state, _, elem) \
	decltype(elem),
#define TC_RETURN_BREAK_OR_CONTINUE_SEQUENCE_EVAL(state, _, i, elem) \
	tc_return_if_break_impl( \
		tc::implicit_cast<BreakOrContinue>(tc::constant<tc::break_>()), \
		elem \
	)

#define tc_return_break_or_continue_impl(exprseq) \
	using BreakOrContinue = tc::common_type_break_or_continue_t< \
		BOOST_PP_SEQ_FOR_EACH(TC_RETURN_BREAK_OR_CONTINUE_SEQUENCE_TYPE, _, exprseq) \
		tc::constant<tc::continue_> \
	>; \
	BOOST_PP_SEQ_FOR_EACH_I(TC_RETURN_BREAK_OR_CONTINUE_SEQUENCE_EVAL, _, exprseq) \
	if constexpr(!std::same_as<BreakOrContinue, tc::constant<break_>>) { \
		return tc::implicit_cast<BreakOrContinue>(tc::constant<tc::continue_>()); \
	}

#define tc_return_break_or_continue(exprseq) \
	tc_return_break_or_continue_impl(exprseq)
