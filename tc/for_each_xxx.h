
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
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

	namespace no_adl {
		template<typename Rng, std::size_t N>
		struct [[nodiscard]] adjacent_tuples_adaptor : tc::range_adaptor_base_range<Rng> {
			static_assert( 1 < N );
			using tc::range_adaptor_base_range<Rng>::range_adaptor_base_range;

			template<typename Self, typename Sink, std::enable_if_t<tc::is_base_of_decayed<adjacent_tuples_adaptor, Self>::value>* = nullptr>
			friend constexpr auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				return internal_for_each(
					std::forward<Self>(self),
					std::forward<Sink>(sink),
					std::make_index_sequence<N - (tc::is_range_with_iterators<Rng>::value ? 1 : 2)>()
				);
			}

			template<ENABLE_SFINAE>
			[[nodiscard]] constexpr auto size() const& noexcept -> decltype(tc::size_raw(SFINAE_VALUE(this)->base_range())) {
				auto n = tc::size_raw(this->base_range());
				if( n < N ) {
					return 0;
				} else {
					return n - (N - 1);
				}
			}

		private:
			template<typename Self, typename Sink, std::size_t... i /*=0,1,...,N-2*/>
			static constexpr auto internal_for_each(Self&& self, Sink const sink, std::index_sequence<i...>) MAYTHROW {
				if constexpr (tc::is_range_with_iterators<Rng>::value) {
					auto GenerateAdjacentTuples = [&]() MAYTHROW {
						auto const itEnd = tc::end(self.base_range());
						auto it = tc::begin(self.base_range());
						auto ait=tc::explicit_cast<std::array<
							tc::iterator_cache<decltype(it)>,
							N
						>>(tc::func_tag, [&](std::size_t) noexcept { return it++; });

						for (;;) {
							for (std::size_t n = 0; n<N; ++n) {
								if (it == itEnd) {
									return tc::continue_if_not_break(sink, tc::forward_as_tuple(*tc_move_always(tc::at(ait, n)), *tc_move_always(tc::at(ait, (n + i + 1) % N))...)); // MAYTHROW
								}
								RETURN_IF_BREAK(tc::continue_if_not_break(sink, tc::forward_as_tuple(*tc_move_always(tc::at(ait, n)), *tc::at(ait, (n + i + 1) % N)...))); // MAYTHROW
								tc::at(ait, n) = it;
								++it;
							}
						}
					};
					return CONDITIONAL_PRVALUE_AS_VAL(
						tc::size_bounded(self.base_range(), N)<N,
						INTEGRAL_CONSTANT(tc::continue_)(),
						GenerateAdjacentTuples() // MAYTHROW
					);
				} else {
					std::array<tc::storage_for<tc::range_value_t<Rng>>, N - 1> aoval;
					std::size_t n = 0;
					scope_exit( tc::for_each(tc::begin_next<tc::return_take>(aoval, tc::min(n, N - 1)), TC_MEM_FN(.dtor)) );

					return tc::for_each(std::forward<Self>(self).base_range(), [&](auto&& u) MAYTHROW {
						auto CallSink = [&]() MAYTHROW {
							return tc::continue_if_not_break( // MAYTHROW
								sink,
								tc::forward_as_tuple(
									tc_move_always(*tc::at(aoval, n % (N - 1))),
									*tc::at(aoval, (n + i + 1) % (N - 1))...,
									u
								)
							);
						};
						if (n < N - 1) {
							tc::at(aoval, n).ctor(tc_move_if_owned(u));
						} else {
							RETURN_IF_BREAK( CallSink() ); // MAYTHROW
							tc::renew(*tc::at(aoval, n % (N - 1)), tc_move_if_owned(u));
						}
						++n;
						return tc::implicit_cast<tc::common_type_t<decltype(CallSink()), INTEGRAL_CONSTANT(tc::continue_)>>(INTEGRAL_CONSTANT(tc::continue_)());
					});
				}
			}
		};

		template<typename Rng, std::size_t N>
		struct constexpr_size_base<adjacent_tuples_adaptor<Rng, N>, std::void_t<typename tc::constexpr_size<Rng>::type>>
			: std::integral_constant<std::size_t, tc::constexpr_size<Rng>::value < N ? 0 : tc::constexpr_size<Rng>::value - (N - 1)>
		{};

		template<typename AdjacentTuplesAdaptor, typename Rng, std::size_t N>
		struct range_value<AdjacentTuplesAdaptor, adjacent_tuples_adaptor<Rng, N>, std::void_t<tc::range_value_t<tc::range_value_t<Rng>>>> final {
			using type = tc::type::apply_t<tc::tuple, tc::type::repeat_n_t<N, tc::range_value_t<Rng>>>;
		};
	}

	template<std::size_t N, typename Rng>
	constexpr no_adl::adjacent_tuples_adaptor<Rng, N> adjacent_tuples(Rng&& rng) noexcept {
		return {tc::aggregate_tag, std::forward<Rng>(rng)};
	}

	/////////////////////////////////////////////////////
	// may_remove_current

	namespace no_adl {
		template<typename Rng>
		struct [[nodiscard]] may_remove_current_impl final { // TODO VS2019: if constexpr does not work well in lambda in VS15.8.0
			tc::reference_or_value<Rng> m_rng;
			constexpr explicit may_remove_current_impl(Rng&& rng) noexcept: m_rng(tc::aggregate_tag, std::forward<Rng>(rng)) {}

			template<typename Func>
			constexpr auto operator()(Func func) /* no & */ MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(*m_rng))), INTEGRAL_CONSTANT(tc::continue_)> {
				auto it=tc::begin(*m_rng);
				auto const itEnd=tc::end(*m_rng);
				while( it!=itEnd ) {
					auto const rsize = constexpr_restrict_size_decrement(*m_rng, 0, 1);
					RETURN_IF_BREAK( try_finally([&]() return_decltype_MAYTHROW(tc::continue_if_not_break(func, *it++)), [&]() noexcept {rsize.dtor();}) );
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};
	}

	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng >
	[[nodiscard]] constexpr auto may_remove_current(Rng&& rng) noexcept code_return_decltype(
		static_assert( !tc::range_filter_by_move_element< std::remove_reference_t<Rng> >::value );,
		no_adl::may_remove_current_impl<Rng>(std::forward<Rng>(rng))
	)

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

	template<typename Rng, typename FuncBegin, typename FuncElem, typename FuncSeparator, typename FuncEnd>
	constexpr auto framed_for_each(Rng&& rng, FuncBegin funcBegin, FuncElem funcElement, FuncSeparator funcSeparator, FuncEnd funcEnd) MAYTHROW {
		bool bEmpty = true;
		return tc_break_or_continue_sequence(
			(tc::for_each(std::forward<Rng>(rng), [&](auto&& t) MAYTHROW {
				using T = decltype(t);
				return tc_break_or_continue_sequence(
					(CONDITIONAL_PRVALUE_AS_VAL(tc::change(bEmpty, false),
						tc_internal_continue_if_not_break(funcBegin()),
						tc_internal_continue_if_not_break(funcSeparator())
					))
					(tc::continue_if_not_break(funcElement, std::forward<T>(t)))
				);
			}))
			(CONDITIONAL_PRVALUE_AS_VAL(!bEmpty,
				tc_internal_continue_if_not_break(funcEnd()),
				INTEGRAL_CONSTANT(tc::continue_)()
			))
		);
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
					tc::make_tuple(
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
}
