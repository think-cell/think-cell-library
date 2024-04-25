
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "empty_range.h"
#include "../algorithm/for_each.h"
#include "../algorithm/empty.h"
#include "../base/reference_or_value.h"

namespace tc {
	namespace no_adl {
		template<typename FuncBegin, typename FuncElem, typename FuncSeparator>
		struct framed_for_each_sink { // MSVC workaround: not a lambda for shorter symbol names
			FuncBegin const& m_funcBegin;
			FuncElem const& m_funcElement;
			FuncSeparator const& m_funcSeparator;
			bool& m_bEmpty;

			constexpr auto operator()(auto&& t) const& MAYTHROW {
				tc_return_break_or_continue(
					(tc_conditional_prvalue_as_val(tc::change(m_bEmpty, false),
						tc_internal_continue_if_not_break(m_funcBegin()),
						tc_internal_continue_if_not_break(m_funcSeparator())
					))
					(tc::continue_if_not_break(m_funcElement, tc_move_if_owned(t)))
				);
			}
		};
	}

	template<typename Rng, typename FuncBegin, typename FuncElem, typename FuncSeparator, typename FuncEnd>
	constexpr auto framed_for_each(Rng&& rng, FuncBegin funcBegin, FuncElem funcElement, FuncSeparator funcSeparator, FuncEnd funcEnd) MAYTHROW {
		bool bEmpty = true;

		tc_return_break_or_continue(
			(tc::for_each(tc_move_if_owned(rng), no_adl::framed_for_each_sink<FuncBegin, FuncElem, FuncSeparator>{funcBegin, funcElement, funcSeparator, bEmpty}))
			(tc_conditional_prvalue_as_val(!bEmpty,
				tc_internal_continue_if_not_break(funcEnd()),
				tc::constant<tc::continue_>()
			))
		);
	}

	namespace no_adl {
		template<typename RngBegin, typename RngRng, typename RngSep, typename RngEnd>
		struct [[nodiscard]] join_framed_adaptor {
			template<typename RngBegin2, typename RngRng2, typename RngSep2, typename RngEnd2>
			explicit join_framed_adaptor(aggregate_tag_t, RngBegin2&& rngBegin, RngRng2&& baserng, RngSep2&& rngSep, RngEnd2&& rngEnd) noexcept
				: m_rngBegin(aggregate_tag, tc_move_if_owned(rngBegin))
				, m_baserng(aggregate_tag, tc_move_if_owned(baserng))
				, m_rngSep(aggregate_tag, tc_move_if_owned(rngSep))
				, m_rngEnd(aggregate_tag, tc_move_if_owned(rngEnd))
			{}

			template<tc::decayed_derived_from<join_framed_adaptor> Self, typename Sink>
			friend auto for_each_impl(Self&& self, Sink sink) MAYTHROW {
				return tc::framed_for_each(*tc_move_if_owned(self).m_baserng,
					[&]() MAYTHROW {
						return tc::for_each(*tc_move_if_owned(self).m_rngBegin, sink);
					},
					[&](auto&& rng) MAYTHROW {
						return tc::for_each(tc_move_if_owned(rng), sink);
					},
					[&]() MAYTHROW {
						return tc::for_each(*tc_move_if_owned(self).m_rngSep, sink);
					},
					[&]() MAYTHROW {
						return tc::for_each(*tc_move_if_owned(self).m_rngEnd, tc_move(sink));
					}
				);
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, join_framed_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&& self) -> boost::mp11::mp_unique<boost::mp11::mp_append<
				tc::range_output_t<decltype(*tc_move_if_owned(self).m_rngBegin)>,
				boost::mp11::mp_flatten<tc::mp_transform<tc::range_output_t, tc::range_output_t<decltype(*tc_move_if_owned(self).m_baserng)>>>,
				tc::range_output_t<decltype(*tc_move_if_owned(self).m_rngSep)>,
				tc::range_output_t<decltype(*tc_move_if_owned(self).m_rngEnd)>
			>> {} // unevaluated

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
	}
	using no_adl::join_framed_adaptor;

	template<typename RngBegin, typename RngRng, typename RngSep, typename RngEnd>
	auto join_framed(RngBegin&& rngBegin, RngRng&& rngrng, RngSep&& rngSep, RngEnd&& rngEnd) return_ctor_noexcept(
		TC_FWD(join_framed_adaptor<RngBegin, RngRng, RngSep, RngEnd>),
		(aggregate_tag, tc_move_if_owned(rngBegin), tc_move_if_owned(rngrng), tc_move_if_owned(rngSep), tc_move_if_owned(rngEnd))
	)

	template<typename RngBegin, typename RngRng, typename RngEnd>
	auto join_framed(RngBegin&& rngBegin, RngRng&& rngrng, RngEnd&& rngEnd) return_ctor_noexcept(
		TC_FWD(join_framed_adaptor<RngBegin, RngRng, tc::empty_range, RngEnd>),
		(aggregate_tag, tc_move_if_owned(rngBegin), tc_move_if_owned(rngrng), tc::empty_range(), tc_move_if_owned(rngEnd))
	)

	template<typename RngSep, typename RngRng>
	auto join_with_separator(RngSep&& rngSep, RngRng&& rngrng) return_ctor_noexcept(
		TC_FWD(join_framed_adaptor<tc::empty_range, RngRng, RngSep, tc::empty_range>),
		(aggregate_tag, tc::empty_range(), tc_move_if_owned(rngrng), tc_move_if_owned(rngSep), tc::empty_range())
	)
}
