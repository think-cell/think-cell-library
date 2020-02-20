
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "for_each.h"
#include "array.h"
#include "size_bounded.h"
#include "format.h"
#include "iterator_cache.h"
#include "restrict_size_decrement.h"
#include "try_finally.h"

namespace tc {
	template< typename Rng, typename Func, int... i >
	constexpr auto for_each_adjacent_tuple_impl(Rng&& rng, Func func, std::integer_sequence<int, i...>) MAYTHROW -> tc::common_type_t<INTEGRAL_CONSTANT(tc::continue_), decltype(tc::continue_if_not_break(func, *tc::begin(rng), (i, *tc::begin(rng))...))> {
		constexpr int N= sizeof...(i)+1;
		if (tc::size_bounded(rng, N)<N) {
			return INTEGRAL_CONSTANT(tc::continue_)();
		} else {
			auto const itEnd = tc::end(rng);
			auto it = tc::begin(rng);
			auto ait=tc::explicit_cast<std::array<
				tc::iterator_cache< 
					typename boost::range_iterator<Rng>::type
				>,
				N
			>>(tc::func_tag, [&](std::size_t) noexcept { return it++; });

			for (;;) {
				for (int n = 0; n<N; ++n) {
					if (it == itEnd) {
						return continue_if_not_break(func, *tc_move_always(ait[n]), *tc_move_always(ait[(n + i + 1) % N])...);
					}
					RETURN_IF_BREAK(continue_if_not_break(func, *tc_move_always(ait[n]), *ait[(n + i + 1) % N]...));
					ait[n] = it;
					++it;
				}
			}
		}
	}

	template< int N, typename Rng, typename Func, std::enable_if_t< is_range_with_iterators<Rng>::value >* =nullptr >
	constexpr auto for_each_adjacent_tuple(Rng&& rng, Func func) MAYTHROW {
		return for_each_adjacent_tuple_impl(std::forward<Rng>(rng), std::forward<Func>(func), std::make_integer_sequence<int,N-1>());
	}

	/////////////////////////////////////////////////////
	// for_each_may_remove_current


	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng, typename Func >
	constexpr auto for_each_may_remove_current(Rng&& rng, Func func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
		static_assert( !tc::range_filter_by_move_element< std::remove_reference_t<Rng> >::value );
		auto it=tc::begin(rng);
		auto const itEnd=tc::end(rng);
		while( it!=itEnd ) {
			auto const rsize = constexpr_restrict_size_decrement(rng, 0, 1);

			auto bc = try_finally([&]() return_decltype_MAYTHROW(tc::continue_if_not_break(func, *it++)), [&]() noexcept {rsize.dtor();});

			if constexpr (std::is_same<decltype(bc), INTEGRAL_CONSTANT(tc::break_)>::value) {
				return INTEGRAL_CONSTANT(tc::break_)();
			} else if constexpr (!std::is_same<decltype(bc), INTEGRAL_CONSTANT(tc::continue_)>::value) {
				if (tc::break_ == bc) {
					return tc::break_;
				}
			}
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	DEFINE_FN(for_each_may_remove_current);

	/////////////////////////////////////////////////////
	// for_each_ordered_pair

	template< typename Rng, typename Func >
	auto for_each_ordered_pair(Rng const& rng, Func func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(rng), *tc::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
		auto const itEndRng = tc::end(rng);
		for(auto itEnd = tc::begin(rng); itEnd != itEndRng; ++itEnd) {
			tc::reference_or_value<tc::range_reference_t<Rng const>> ref(aggregate_tag, *itEnd);

			RETURN_IF_BREAK(
				tc::for_each(
					tc::take(rng, itEnd),
					[&](auto const& _) MAYTHROW { return func(_, *ref); }
				)
			);
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	template<typename Rng, typename Func, std::enable_if_t<!is_range_with_iterators<Rng>::value>* = nullptr>
	auto for_each_adjacent_pair(Rng&& rng, Func func) MAYTHROW {
		std::optional<tc::range_value_t<Rng>> oparam;
		return tc::for_each( std::forward<Rng>(rng), [&](auto&& u) MAYTHROW
			-> tc::common_type_t<decltype(tc::continue_if_not_break(
#ifdef __clang__
				// Apple clang 10: Using func and *oparam in lambda return type leads to ICE.
				std::declval<Func&>(), std::declval<tc::range_value_t<Rng>>(), u
#else
				// MSVC 15.8: Using std::declval<Func>() and std::declval<tc::range_value_t<Rng>&>() in lambda return type leads to compilation failure.
				func, *tc_move_always(oparam), u
#endif
			)), INTEGRAL_CONSTANT(tc::continue_)>
		{
			if (oparam) {
				RETURN_IF_BREAK(tc::continue_if_not_break(func, *tc_move_always(oparam), u));
			}
			oparam.emplace(tc_move_if_owned(u));

			return INTEGRAL_CONSTANT(tc::continue_)();
		} );
	}

	template<typename Rng, typename FuncBegin, typename FuncElem, typename FuncSeparator, typename FuncEnd>
	constexpr auto framed_for_each(Rng&& rng, FuncBegin funcBegin, FuncElem funcElement, FuncSeparator funcSeparator, FuncEnd funcEnd) MAYTHROW {
		bool bEmpty = true;

		using funcbegin_breakorcontinue_t = decltype(tc::continue_if_not_break(funcBegin));

		// As of c++17, constexpr functions cannot have static variables (even if the variables are constexpr) - Clang correctly complains, but Visual Studio accepts
		// it. However, as of Visual Studio 19.15.26726, if the variable is not static, and also used inside a lambda, Visual Studio wrongly attempts to capture it,
		// and forbids the usage of the variable in constant expressions, effectively triggering error C2131: expression did not evaluate to a constant. As a
		// workaround, a type is used instead.
		using funcbegin_always_breaks = std::is_same<INTEGRAL_CONSTANT(tc::break_), funcbegin_breakorcontinue_t>;

		// As of Visual Studio compiler 19.15.26726, it's not possible to explicitly specify a common return type (break or continue) for the following lambda. If
		// provided, the compiler triggers error C2672: 'tc::for_each': no matching overloaded function found. Using auto return type deduction, instead.
		auto breakorcontinue = tc::for_each(std::forward<Rng>(rng), [&](auto&& t) MAYTHROW {
			if constexpr(funcbegin_always_breaks::value) {
				return funcBegin();
			} else {
				using breakorcontinue_t = tc::common_type_t<
					funcbegin_breakorcontinue_t,
					decltype(tc::continue_if_not_break(funcSeparator)),
					decltype(tc::continue_if_not_break(funcElement, std::forward<decltype(t)>(t)))
				>;

				if(tc::change(bEmpty, false)) {
					RETURN_IF_BREAK(tc::implicit_cast<breakorcontinue_t>(tc::continue_if_not_break(funcBegin)));
				} else {
					RETURN_IF_BREAK(tc::implicit_cast<breakorcontinue_t>(tc::continue_if_not_break(funcSeparator)));
				}
				return tc::implicit_cast<breakorcontinue_t>(tc::continue_if_not_break(funcElement, std::forward<decltype(t)>(t)));
			}
		});

		if constexpr(funcbegin_always_breaks::value) {
			return breakorcontinue;
		} else {
			using breakorcontinue_t = tc::common_type_t<decltype(breakorcontinue), decltype(tc::continue_if_not_break(funcEnd))>;
			if(tc::break_==breakorcontinue || bEmpty) {
				return tc::implicit_cast<breakorcontinue_t>(breakorcontinue);
			} else {
				return tc::implicit_cast<breakorcontinue_t>(tc::continue_if_not_break(funcEnd));
			}
		}
	}

	namespace no_adl {
		template<typename RngBegin, typename RngRng, typename RngSep, typename RngEnd>
		struct [[nodiscard]] join_framed_adaptor {
			template<typename RngBegin2, typename RngRng2, typename RngSep2, typename RngEnd2>
			explicit join_framed_adaptor(aggregate_tag_t, RngBegin2&& rngBegin, RngRng2&& baserng, RngSep2&& rngSep, RngEnd2&& rngEnd) noexcept
				: m_rngBegin(aggregate_tag, std::forward<RngBegin2>(rngBegin))
				, m_baserng(aggregate_tag, std::forward<RngRng2>(baserng))
				, m_rngSep(aggregate_tag, std::forward<RngSep2>(rngSep))
				, m_rngEnd(aggregate_tag, std::forward<RngEnd2>(rngEnd))
			{}

			template<typename Sink>
			auto operator()(Sink sink) const& MAYTHROW {
				return tc::framed_for_each(*m_baserng,
					[&]() MAYTHROW {
						return tc::for_each(*m_rngBegin, sink);
					},
					[&](auto&& rng) MAYTHROW {
						return tc::for_each(std::forward<decltype(rng)>(rng), sink);
					},
					[&]() MAYTHROW {
						return tc::for_each(*m_rngSep, sink);
					},
					[&]() MAYTHROW {
						return tc::for_each(*m_rngEnd, tc_move_always(sink));
					}
				);
			}

			bool empty() const& noexcept {
				return
					tc::empty(*m_baserng)
					|| (
						tc::empty(*m_rngBegin) && tc::empty(*m_rngEnd) && [&]() noexcept {
							bool const bEmptySep = tc::empty(*m_rngSep);
							bool bFirst = false;
							return tc::continue_==tc::for_each(*m_baserng, [&](auto const& rng) noexcept {
								return tc::continue_if(tc::empty(rng) && (tc::change(bFirst, true) || bEmptySep));
							});
						}()
					);
			}

		private:
			tc::reference_or_value<RngBegin> m_rngBegin;
			tc::reference_or_value<RngRng> m_baserng;
			tc::reference_or_value<RngSep> m_rngSep;
			tc::reference_or_value<RngEnd> m_rngEnd;
		};

		template<typename JoinFramedAdaptor, typename RngBegin, typename RngRng, typename RngSep, typename RngEnd>
		struct range_value<JoinFramedAdaptor, join_framed_adaptor<RngBegin, RngRng, RngSep, RngEnd>, tc::void_t<tc::common_range_value_t<RngBegin, tc::range_value_t<RngRng>, RngSep, RngEnd>>> final {
			using type = tc::common_range_value_t<RngBegin, tc::range_value_t<RngRng>, RngSep, RngEnd>;
		};
	}
	using no_adl::join_framed_adaptor;

	template<typename RngBegin, typename RngRng, typename RngSep, typename RngEnd>
	auto join_framed(RngBegin&& rngBegin, RngRng&& rngrng, RngSep&& rngSep, RngEnd&& rngEnd) return_ctor_noexcept(
		join_framed_adaptor<RngBegin BOOST_PP_COMMA() RngRng BOOST_PP_COMMA() RngSep BOOST_PP_COMMA() RngEnd>,
		(aggregate_tag, std::forward<RngBegin>(rngBegin), std::forward<RngRng>(rngrng), std::forward<RngSep>(rngSep), std::forward<RngEnd>(rngEnd))
	)

	template<typename RngBegin, typename RngRng, typename RngEnd>
	auto join_framed(RngBegin&& rngBegin, RngRng&& rngrng, RngEnd&& rngEnd) return_ctor_noexcept(
		join_framed_adaptor<RngBegin BOOST_PP_COMMA() RngRng BOOST_PP_COMMA() tc::empty_range BOOST_PP_COMMA() RngEnd>,
		(aggregate_tag, std::forward<RngBegin>(rngBegin), std::forward<RngRng>(rngrng), tc::empty_range(), std::forward<RngEnd>(rngEnd))
	)

	template<typename RngRng, typename RngSep>
	auto join_separated(RngRng&& rngrng, RngSep&& rngSep) return_ctor_noexcept(
		join_framed_adaptor<tc::empty_range BOOST_PP_COMMA() RngRng BOOST_PP_COMMA() RngSep BOOST_PP_COMMA() tc::empty_range>,
		(aggregate_tag, tc::empty_range(), std::forward<RngRng>(rngrng), std::forward<RngSep>(rngSep), tc::empty_range())
	)

	namespace no_adl {
		template<typename List, typename Enable = void>
		struct make_range_impl;

		template<typename T0, typename T1, typename... T>
		struct [[nodiscard]] make_range_impl<tc::type::list<T0, T1, T...>, std::enable_if_t<!std::conjunction<std::is_same<T0, T1>, std::is_same<T0, T>...>::value>> final {
			static decltype(auto) fn(T0&& t0, T1&& t1, T&&... t) noexcept {
				return tc::transform(
					std::make_tuple(
						tc::make_reference_or_value(std::forward<T0>(t0)),
						tc::make_reference_or_value(std::forward<T1>(t1)),
						tc::make_reference_or_value(std::forward<T>(t))...
					),
					tc::fn_indirection()
				);
			}
		};

		template<typename T0, typename... T>
		struct [[nodiscard]] make_range_impl<tc::type::list<T0, T...>, std::enable_if_t<std::conjunction<std::is_same<T0, T>...>::value>> final {
			static constexpr decltype(auto) fn(T0&& t0, T&&... t) noexcept {
				return tc::make_array<std::conditional_t<std::is_reference<T0>::value, T0, std::remove_cv_t<T0>>>(tc::aggregate_tag,
					std::forward<T0>(t0), std::forward<T>(t)...
				);
			}
		};
	}

	template<typename... T>
	constexpr decltype(auto) make_range(T&&... t) noexcept {
		return no_adl::make_range_impl<tc::type::list<T...>>::fn(std::forward<T>(t)...);
	}

	template<typename RngSep, typename... Rngs>
	decltype(auto) concat_nonempty_separated(RngSep&& rngSep, Rngs&&... rngs) noexcept {
		return tc::join_separated(
			tc::filter(tc::make_range(std::forward<Rngs>(rngs)...), tc::not_fn(tc::fn_empty())),
			std::forward<RngSep>(rngSep)
		);
	}
}
