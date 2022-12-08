
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/modified.h"
#include "../algorithm/compare.h"
#include "../algorithm/algorithm.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"

namespace tc {

	namespace union_adaptor_adl {

		template<
			typename Comp,
			typename Rng0,
			typename Rng1,
			bool HasIterator = is_range_with_iterators< Rng0 >::value && is_range_with_iterators< Rng1 >::value
		>
		struct union_adaptor;

		template<typename Comp, typename Rng0, typename Rng1>
		struct [[nodiscard]] union_adaptor<Comp, Rng0, Rng1, false>
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
					{{aggregate_tag, std::forward<Rhs0>(rhs0)}},
					{{aggregate_tag, std::forward<Rhs1>(rhs1)}}
				}},
				m_comp(std::forward<Comp2>(comp))
			{}

		private:
			template<typename Sink>
			struct FForwardFirstArgOnly final {
				Sink const& m_sink;

				template<typename T0, typename T1>
				auto operator()(T0&& arg0, T1&&) const {
					return tc::continue_if_not_break(m_sink, std::forward<T0>(arg0));
				}
			};

		public:
			template<typename Self, typename Sink> requires tc::is_base_of_decayed<union_adaptor, Self>::value
			friend auto for_each_impl(Self&& self, Sink const sink) MAYTHROW {
				return tc::interleave_2(
					tc::get<0>(std::forward<Self>(self).m_tupleadaptbaserng).base_range(),
					tc::get<1>(std::forward<Self>(self).m_tupleadaptbaserng).base_range(),
					std::forward<Self>(self).m_comp,
					std::ref(sink),
					std::ref(sink),
					FForwardFirstArgOnly<Sink>{sink}
				);
			}

			template<typename Self, std::enable_if_t<tc::is_base_of_decayed<union_adaptor, Self>::value>* = nullptr>
			friend auto range_output_t_impl(Self&&) -> tc::type::unique_t<tc::type::concat_t<
				tc::range_output_t<decltype(tc::get<0>(std::declval<Self>().m_tupleadaptbaserng).base_range())>,
				tc::range_output_t<decltype(tc::get<1>(std::declval<Self>().m_tupleadaptbaserng).base_range())>
			>> {} // unevaluated

			template<typename Self, typename Sink> requires tc::is_base_of_decayed<union_adaptor, Self>::value
			friend auto for_each_reverse_impl(Self&& self, Sink const sink) MAYTHROW {
				return tc::interleave_2(
					tc::reverse(tc::get<0>(std::forward<Self>(self).m_tupleadaptbaserng).base_range()),
					tc::reverse(tc::get<1>(std::forward<Self>(self).m_tupleadaptbaserng).base_range()),
					/*comp*/[&](auto const& lhs, auto const& rhs) noexcept { return tc::negate(self.m_comp(lhs, rhs)); },
					std::ref(sink),
					std::ref(sink),
					FForwardFirstArgOnly<Sink>{sink}
				);
			}
		};

		template<
			typename Index0,
			typename Index1
		>
		struct union_adaptor_index {
			template<typename... Args>
			union_adaptor_index(Args... args) noexcept
				: m_tplindex{std::forward<Args>(args)...}
			{}

			template<typename... Args>
			union_adaptor_index(std::weak_ordering order, Args... args) noexcept
				: m_tplindex{std::forward<Args>(args)...}
				, m_oorder(order)
			{}

			friend bool operator==(union_adaptor_index const& lhs, union_adaptor_index const& rhs) noexcept {
				return EQUAL_MEMBERS(m_tplindex);
			}

			tc::tuple<Index0, Index1> m_tplindex;
			std::optional<std::weak_ordering> m_oorder;
		};

		template<
			typename Comp,
			typename Rng0,
			typename Rng1
		>
		struct [[nodiscard]] union_adaptor<Comp, Rng0, Rng1, true> :
			union_adaptor<Comp, Rng0, Rng1, false>,
			range_iterator_from_index<
				union_adaptor<Comp, Rng0, Rng1, true>,
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
			using union_adaptor<Comp, Rng0, Rng1, false>::union_adaptor;

			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng0>>::value || tc::has_stashing_index<std::remove_reference_t<Rng1>>::value;
		private:
			void find_order(tc_index& idx) const& noexcept {
				if (at_end_index_fwd<0>(idx)) {
					idx.m_oorder = at_end_index_fwd<1>(idx) ? std::weak_ordering::less : std::weak_ordering::greater;
				} else if (at_end_index_fwd<1>(idx)) {
					idx.m_oorder = std::weak_ordering::less;
				} else {
					idx.m_oorder = this->m_comp(dereference_index_fwd<0>(idx), dereference_index_fwd<1>(idx));
				}
			}

			template<int N>
			void increment_index_fwd(tc_index& idx) const& noexcept {
				tc::increment_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(idx.m_tplindex));
			}

			template<int N>
			void decrement_index_fwd(tc_index& idx) const& noexcept {
				tc::decrement_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(idx.m_tplindex));
			}

			template<int N>
			bool at_end_index_fwd(tc_index const& idx) const& noexcept {
				return tc::at_end_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(idx.m_tplindex));
			}

			template<int N>
			bool at_begin_index(tc_index const& idx) const& noexcept {
				return tc::get<N>(idx.m_tplindex) == tc::get<N>(this->m_tupleadaptbaserng).base_begin_index();
			}

			template<int N>
			auto dereference_index_fwd(tc_index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(tc::get<N>(this->m_tupleadaptbaserng).base_range(), tc::get<N>(idx.m_tplindex))
			)

			STATIC_FINAL(at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return std::is_lt(*idx.m_oorder) && at_end_index_fwd<0>(idx);
			}

			STATIC_FINAL(begin_index)() const& noexcept -> tc_index {
				tc_index idx(tc::get<0>(this->m_tupleadaptbaserng).base_begin_index(), tc::get<1>(this->m_tupleadaptbaserng).base_begin_index());
				find_order(idx);
				return idx;
			}

			STATIC_FINAL(dereference_index)(tc_index const& idx) const& noexcept -> decltype(auto) {
				return CONDITIONAL_PRVALUE_AS_VAL( std::is_lteq(*idx.m_oorder), dereference_index_fwd<0>(idx), dereference_index_fwd<1>(idx) );
			}

			STATIC_FINAL(end_index)() const& noexcept -> tc_index {
				tc_index idx(std::weak_ordering::less, tc::get<0>(this->m_tupleadaptbaserng).base_end_index(), tc::get<1>(this->m_tupleadaptbaserng).base_end_index());
				return idx;
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& noexcept -> void {
				if(std::is_lt(*idx.m_oorder)) {
					increment_index_fwd<0>(idx);
				} else {
					if(tc::is_eq(*idx.m_oorder)) {
						increment_index_fwd<0>(idx);
					} else {
						_ASSERT(std::is_gt(*idx.m_oorder));
					}
					increment_index_fwd<1>(idx);
				}

				find_order(idx);
			}

			STATIC_FINAL(decrement_index)(tc_index& idx) const& noexcept -> void {
				if (at_begin_index<0>(idx)) {
					_ASSERT(!at_begin_index<1>(idx));
					decrement_index_fwd<1>(idx);
					idx.m_oorder = std::weak_ordering::greater;
				} else if (at_begin_index<1>(idx)) {
					_ASSERT(!at_begin_index<0>(idx));
					decrement_index_fwd<0>(idx);
					idx.m_oorder = std::weak_ordering::less;
				} else {
					auto idxOriginal = idx;
					decrement_index_fwd<0>(idx);
					decrement_index_fwd<1>(idx);
					idx.m_oorder = tc::negate(this->m_comp(dereference_index_fwd<0>(idx), dereference_index_fwd<1>(idx)));
					if(std::is_lt(*idx.m_oorder)) {
						tc::get<1>(idx.m_tplindex) = tc::get<1>(tc_move(idxOriginal).m_tplindex);
					} else if(std::is_gt(*idx.m_oorder)) {
						tc::get<0>(idx.m_tplindex) = tc::get<0>(tc_move(idxOriginal).m_tplindex);
					} else {
						_ASSERT(tc::is_eq(*idx.m_oorder));
					}
				}
			}

			// partition_point would be a more efficient customization point
			STATIC_FINAL(middle_point)(tc_index& idx, tc_index const& idxEnd) const& noexcept -> void {
				if (tc::get<0>(idx.m_tplindex) == tc::get<0>(idxEnd.m_tplindex)) {
					tc::middle_point(tc::get<1>(this->m_tupleadaptbaserng).base_range(), tc::get<1>(idx.m_tplindex), tc::get<1>(idxEnd.m_tplindex));
					idx.m_oorder = std::weak_ordering::greater;
				} else {
					auto idx0Begin = tc::get<0>(idx.m_tplindex);
					tc::middle_point(tc::get<0>(this->m_tupleadaptbaserng).base_range(), tc::get<0>(idx.m_tplindex), tc::get<0>(idxEnd.m_tplindex));
					auto&& ref0 = dereference_index_fwd<0>(idx);
					tc::get<1>(idx.m_tplindex) = tc::iterator2index(
						tc::lower_bound(
							tc::make_iterator(tc::get<1>(tc::as_mutable(this->m_tupleadaptbaserng)).base_range(), tc::get<1>(idx.m_tplindex)),
							tc::make_iterator(tc::get<1>(tc::as_mutable(this->m_tupleadaptbaserng)).base_range(), tc::get<1>(idxEnd.m_tplindex)),
							ref0,
							tc::greaterfrom3way([&](auto const& _1, auto const& _2) noexcept { return this->m_comp(_2, _1); })
						)
					);

					if (at_end_index_fwd<1>(idx)) {
						idx.m_oorder = std::weak_ordering::less;
					} else {
						auto&& ref1 = dereference_index_fwd<1>(idx);

						idx.m_oorder = this->m_comp(ref0, ref1);
						_ASSERT(std::is_lteq(*idx.m_oorder));

						if (tc::is_eq(*idx.m_oorder)) {
							auto idx0 = tc::get<0>(idx.m_tplindex);
							typename boost::range_size<Rng0>::type n = 0;
							while (idx0Begin != idx0) {
								tc::decrement_index(tc::get<0>(this->m_tupleadaptbaserng).base_range(), idx0);

								if (tc::is_neq(this->m_comp(tc::dereference_index(tc::get<0>(this->m_tupleadaptbaserng).base_range(), idx0), ref1))) {
									break;
								}
								++n;
							}
							while (0<n) {
								increment_index_fwd<1>(idx);
								--n;

								if (at_end_index_fwd<1>(idx) || std::is_lt(this->m_comp(ref0, dereference_index_fwd<1>(idx)))) {
									idx.m_oorder = std::weak_ordering::less;
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

	template<typename Comp, typename Rng0, typename Rng1>
	[[nodiscard]] auto split_union(subrange<union_adaptor<Comp, Rng0, Rng1> &> const& rngsubunion) return_decltype_NOEXCEPT( // make_tuple is noexcept(false); the rest of this should only throw bad_alloc
		tc::make_tuple(
			tc::slice(tc::get<0>(rngsubunion.base_range().m_tupleadaptbaserng).base_range(), tc::get<0>(rngsubunion.begin_index().m_tplindex), tc::get<0>(rngsubunion.end_index().m_tplindex)),
			tc::slice(tc::get<1>(rngsubunion.base_range().m_tupleadaptbaserng).base_range(), tc::get<1>(rngsubunion.begin_index().m_tplindex), tc::get<1>(rngsubunion.end_index().m_tplindex))
		)
	)

	template<typename Rng0, typename Rng1, typename Comp = tc::fn_compare>
	auto union_range(Rng0&& rng0, Rng1&& rng1, Comp&& comp = Comp()) return_ctor_noexcept(
		TC_FWD(union_adaptor< tc::decay_t<Comp>, Rng0, Rng1>),
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	namespace no_adl {
		template<typename Comp, typename Rng0, typename Rng1>
		struct is_index_valid_for_move_constructed_range<tc::union_adaptor<Comp, Rng0, Rng1, true>>: std::conjunction<
			is_index_valid_for_move_constructed_range<Rng0>,
			is_index_valid_for_move_constructed_range<Rng1>
		> {};
	}
}
