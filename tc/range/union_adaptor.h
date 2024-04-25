
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/modified.h"
#include "../algorithm/compare.h"
#include "../algorithm/algorithm.h"
#include "range_adaptor.h"
#include "meta.h"

namespace tc {
	namespace union_adaptor_detail::no_adl {
		template<typename Sink, bool bDisjoint>
		struct union_adaptor_sink {
			Sink const m_sink;

			constexpr auto operator()(interleave_2_detail::lhs_tag_t, auto&& lhs) const& MAYTHROW {
				return tc_invoke(m_sink, tc_move_if_owned(lhs));
			}
			constexpr auto operator()(interleave_2_detail::rhs_tag_t, auto&& rhs) const& MAYTHROW {
				return tc_invoke(m_sink, tc_move_if_owned(rhs));
			}
			constexpr auto operator()(interleave_2_detail::lhsrhs_tag_t, auto&& lhs, tc::unused) const& MAYTHROW {
				if constexpr(bDisjoint) {
					_ASSERTFALSE;
				}
				return tc_invoke(m_sink, tc_move_if_owned(lhs));
			}
		};

		template<typename Comp>
		struct SNegateComp { // MSVC workaround: not a lambda for shorter symbol names
			Comp const m_comp;
			constexpr auto operator()(auto const& lhs, auto const& rhs) const& MAYTHROW {
				return tc::negate(tc_invoke(m_comp, lhs, rhs));
			}
		};
	}

	namespace union_adaptor_adl {

		template<
			typename Comp,
			typename Rng0,
			typename Rng1,
			bool bDisjoint = false,
			bool bHasIterator = tc::ranges_with_common_reference<Rng0, Rng1>
		>
		struct union_adaptor;

		template<typename Comp, typename Rng0, typename Rng1, bool bDisjoint>
		struct [[nodiscard]] union_adaptor<Comp, Rng0, Rng1, bDisjoint, false>
		{
			tc::tuple<
				tc::range_adaptor_base_range< Rng0 >,
				tc::range_adaptor_base_range< Rng1 >
			> m_tupleadaptbaserng;

		protected:
			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp2>
			explicit union_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp2&& comp) noexcept
				: m_tupleadaptbaserng{{
					{{aggregate_tag, tc_move_if_owned(rhs0)}},
					{{aggregate_tag, tc_move_if_owned(rhs1)}}
				}},
				m_comp(tc_move_if_owned(comp))
			{}

		public:
			template<tc::decayed_derived_from<union_adaptor> Self, typename Sink>
			friend auto for_each_impl(Self&& self, Sink&& sink) MAYTHROW {
				return interleave_2_detail::internal_interleave_2(
					tc::get<0>(tc_move_if_owned(self).m_tupleadaptbaserng).base_range(),
					tc::get<1>(tc_move_if_owned(self).m_tupleadaptbaserng).base_range(),
					tc_move_if_owned(self).m_comp,
					union_adaptor_detail::no_adl::union_adaptor_sink<Sink, bDisjoint>{tc_move_if_owned(sink)}
				);
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, union_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> boost::mp11::mp_unique<boost::mp11::mp_append<
				tc::range_output_t<decltype(tc::get<0>(std::declval<Self>().m_tupleadaptbaserng).base_range())>,
				tc::range_output_t<decltype(tc::get<1>(std::declval<Self>().m_tupleadaptbaserng).base_range())>
			>> {} // unevaluated

			template<tc::decayed_derived_from<union_adaptor> Self, typename Sink>
			friend auto for_each_reverse_impl(Self&& self, Sink&& sink) MAYTHROW {
				return interleave_2_detail::internal_interleave_2(
					tc::reverse(tc::get<0>(tc_move_if_owned(self).m_tupleadaptbaserng).base_range()),
					tc::reverse(tc::get<1>(tc_move_if_owned(self).m_tupleadaptbaserng).base_range()),
					union_adaptor_detail::no_adl::SNegateComp<Comp>{tc_move_if_owned(self).m_comp},
					union_adaptor_detail::no_adl::union_adaptor_sink<Sink, bDisjoint>{tc_move_if_owned(sink)}
				);
			}
		};

		template<typename Index0, typename Index1>
		struct union_adaptor_index : tc::tuple<Index0, Index1> {
			// Inherit equality from tc::tuple
			std::weak_ordering m_order=std::weak_ordering::less; // dummy default value to make index default constructible
		};

		template<
			typename Comp,
			typename Rng0,
			typename Rng1,
			bool bDisjoint
		>
		struct [[nodiscard]] union_adaptor<Comp, Rng0, Rng1, bDisjoint, true> :
			union_adaptor<Comp, Rng0, Rng1, bDisjoint, false>,
			range_iterator_from_index<
				union_adaptor<Comp, Rng0, Rng1, bDisjoint, true>,
				union_adaptor_index<
					tc::index_t<std::remove_reference_t<
						Rng0
					>>,
					tc::index_t<std::remove_reference_t<
						Rng1
					>>
				>
			>
		{
		private:
			using this_type = union_adaptor;

		public:
			using typename this_type::range_iterator_from_index::tc_index;
			using union_adaptor<Comp, Rng0, Rng1, bDisjoint, false>::union_adaptor;

			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng0>>::value || tc::has_stashing_index<std::remove_reference_t<Rng1>>::value;
			static constexpr bool c_bPrefersForEach = true;

		private:
			void find_order(tc_index& idx) const& noexcept {
				if (base_at_end_index<0>(idx)) {
					idx.m_order = base_at_end_index<1>(idx) ? std::weak_ordering::less : std::weak_ordering::greater;
				} else if (base_at_end_index<1>(idx)) {
					idx.m_order = std::weak_ordering::less;
				} else {
					idx.m_order = this->m_comp(base_dereference_index<0>(idx), base_dereference_index<1>(idx));
					if constexpr(bDisjoint) {
						_ASSERT(tc::is_neq(idx.m_order));
					}
				}
			}

			template<int N>
			void base_increment_index(tc_index& idx) const& noexcept {
				tc::increment_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(idx));
			}

			template<int N>
			void base_decrement_index(tc_index& idx) const& noexcept {
				tc::decrement_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(idx));
			}

			template<int N>
			bool base_at_end_index(tc_index const& idx) const& noexcept {
				return tc::at_end_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(idx));
			}

			template<int N>
			bool base_at_begin_index(tc_index const& idx) const& noexcept {
				return tc::get<N>(idx) == tc::get<N>(this->m_tupleadaptbaserng).base_begin_index();
			}

			template<int N, typename Index>
			auto base_dereference_index(Index&& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(tc_move_if_owned(idx)))
			)

			STATIC_FINAL(at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return std::is_lt(idx.m_order) && base_at_end_index<0>(idx);
			}

			STATIC_FINAL(begin_index)() const& noexcept -> tc_index {
				tc_index idx{{tc::get<0>(this->m_tupleadaptbaserng).base_begin_index(), tc::get<1>(this->m_tupleadaptbaserng).base_begin_index()}};
				find_order(idx);
				return idx;
			}

			STATIC_FINAL(dereference_index)(auto&& idx) const& MAYTHROW -> decltype(auto) {
				return tc_conditional_prvalue_as_val(
					std::is_lteq(idx.m_order),
					base_dereference_index<0>(tc_move_if_owned(idx)),
					base_dereference_index<1>(tc_move_if_owned(idx))
				);
			}

			STATIC_FINAL(end_index)() const& noexcept -> tc_index
				requires tc::has_end_index<Rng0> && tc::has_end_index<Rng1>
			{
				tc_index idx{{tc::get<0>(this->m_tupleadaptbaserng).base_end_index(), tc::get<1>(this->m_tupleadaptbaserng).base_end_index()}, std::weak_ordering::less};
				return idx;
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& noexcept -> void {
				if(std::is_lt(idx.m_order)) {
					base_increment_index<0>(idx);
				} else {
					if(tc::is_eq(idx.m_order)) {
						base_increment_index<0>(idx);
					} else {
						_ASSERTDEBUG(std::is_gt(idx.m_order));
					}
					base_increment_index<1>(idx);
				}

				find_order(idx);
			}

			STATIC_FINAL(decrement_index)(tc_index& idx) const& noexcept -> void
				requires tc::has_decrement_index<Rng0> && tc::has_decrement_index<Rng1>
			{
				if (base_at_begin_index<0>(idx)) {
					_ASSERT(!base_at_begin_index<1>(idx));
					base_decrement_index<1>(idx);
					idx.m_order = std::weak_ordering::greater;
				} else if (base_at_begin_index<1>(idx)) {
					_ASSERT(!base_at_begin_index<0>(idx));
					base_decrement_index<0>(idx);
					idx.m_order = std::weak_ordering::less;
				} else {
					auto idxOriginal = idx;
					base_decrement_index<0>(idx);
					base_decrement_index<1>(idx);
					idx.m_order = tc::negate(this->m_comp(base_dereference_index<0>(idx), base_dereference_index<1>(idx)));
					if(std::is_lt(idx.m_order)) {
						tc::get<1>(idx) = tc::get<1>(tc_move(idxOriginal));
					} else if(std::is_gt(idx.m_order)) {
						tc::get<0>(idx) = tc::get<0>(tc_move(idxOriginal));
					} else {
						_ASSERTDEBUG(tc::is_eq(idx.m_order));
						_ASSERT(!bDisjoint);
					}
				}
			}

			// partition_point would be a more efficient customization point
			STATIC_FINAL(middle_point)(tc_index& idx, tc_index const& idxEnd) const& noexcept -> void
				requires tc::has_middle_point<Rng0> && tc::has_middle_point<Rng1>
			{
				if (tc::get<0>(idx) == tc::get<0>(idxEnd)) {
					tc::middle_point(tc::get<1>(this->m_tupleadaptbaserng).base_range(), tc::get<1>(idx), tc::get<1>(idxEnd));
					idx.m_order = std::weak_ordering::greater;
				} else {
					auto idx0Begin = tc::get<0>(idx);
					tc::middle_point(tc::get<0>(this->m_tupleadaptbaserng).base_range(), tc::get<0>(idx), tc::get<0>(idxEnd));
					auto&& ref0 = base_dereference_index<0>(idx);
					tc::get<1>(idx) = tc::iterator2index<Rng1>(
						tc::lower_bound(
							tc::make_iterator(tc::get<1>(tc::as_mutable(this->m_tupleadaptbaserng)).base_range(), tc::get<1>(idx)),
							tc::make_iterator(tc::get<1>(tc::as_mutable(this->m_tupleadaptbaserng)).base_range(), tc::get<1>(idxEnd)),
							ref0,
							tc::greaterfrom3way([&](auto const& _1, auto const& _2) noexcept { return this->m_comp(_2, _1); })
						)
					);

					if (base_at_end_index<1>(idx)) {
						idx.m_order = std::weak_ordering::less;
					} else {
						auto&& ref1 = base_dereference_index<1>(idx);

						idx.m_order = this->m_comp(ref0, ref1);
						_ASSERT(std::is_lteq(idx.m_order));

						if (tc::is_eq(idx.m_order)) {
							auto idx0 = tc::get<0>(idx);
							typename boost::range_size<Rng0>::type n = 0;
							while (idx0Begin != idx0) {
								tc::decrement_index(tc::get<0>(this->m_tupleadaptbaserng).base_range(), idx0);

								if (tc::is_neq(this->m_comp(tc::dereference_index(tc::get<0>(this->m_tupleadaptbaserng).base_range(), idx0), ref1))) {
									_ASSERT(!bDisjoint);
									break;
								}
								++n;
							}
							while (0<n) {
								base_increment_index<1>(idx);
								--n;

								if (base_at_end_index<1>(idx) || std::is_lt(this->m_comp(ref0, base_dereference_index<1>(idx)))) {
									idx.m_order = std::weak_ordering::less;
									break;
								}
							}
						}
					}
				}
			}
		};
	}
	using union_adaptor_adl::union_adaptor;

	template<typename Comp, typename Rng0, typename Rng1, bool bDisjoint>
	constexpr auto enable_stable_index_on_move<tc::union_adaptor<Comp, Rng0, Rng1, bDisjoint, true>>
		= tc::stable_index_on_move<Rng0> && tc::stable_index_on_move<Rng1>;

	template<typename Comp, typename Rng0, typename Rng1, bool bDisjoint>
	[[nodiscard]] auto split_union(no_adl::subrange<union_adaptor<Comp, Rng0, Rng1, bDisjoint> &> const& rngsubunion) return_decltype_NOEXCEPT( // make_tuple is noexcept(false); the rest of this should only throw bad_alloc
		tc::make_tuple(
			tc::slice(tc::get<0>(rngsubunion.base_range().m_tupleadaptbaserng).base_range(), tc::get<0>(rngsubunion.begin_index()), tc::get<0>(rngsubunion.end_index())),
			tc::slice(tc::get<1>(rngsubunion.base_range().m_tupleadaptbaserng).base_range(), tc::get<1>(rngsubunion.begin_index()), tc::get<1>(rngsubunion.end_index()))
		)
	)

	template<typename Rng0, typename Rng1, typename Comp = tc::fn_compare>
	[[nodiscard]] auto union_range(Rng0&& rng0, Rng1&& rng1, Comp&& comp = Comp()) return_ctor_noexcept(
		TC_FWD(union_adaptor< tc::decay_t<Comp>, Rng0, Rng1, /*bDisjoint*/false>),
		(tc_move_if_owned(rng0), tc_move_if_owned(rng1), tc_move_if_owned(comp))
	)

	template<typename Rng0, typename Rng1, typename Comp = tc::fn_compare>
	[[nodiscard]] auto disjoint_union_range(Rng0&& rng0, Rng1&& rng1, Comp&& comp = Comp()) return_ctor_noexcept(
		TC_FWD(union_adaptor< tc::decay_t<Comp>, Rng0, Rng1, /*bDisjoint*/true>),
		(tc_move_if_owned(rng0), tc_move_if_owned(rng1), tc_move_if_owned(comp))
	)
}
