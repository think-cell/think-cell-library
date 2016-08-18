//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "modified.h"
#include "compare.h"

namespace tc {

	namespace union_adaptor_adl_barrier {

		template<
			typename Comp,
			typename Rng0,
			typename Rng1,
			bool HasIterator = is_range_with_iterators< Rng0 >::value && is_range_with_iterators< Rng1 >::value
		>
		struct union_adaptor;

		template<typename Comp, typename Rng0, typename Rng1>
		struct union_adaptor<Comp, Rng0, Rng1, false>
		{
			std::tuple<
				reference_or_value< index_range_t<Rng0> >,
				reference_or_value< index_range_t<Rng1> >
			> m_baserng;

		protected:
			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp>
			explicit union_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp&& comp) noexcept
				: m_baserng(
					reference_or_value< index_range_t<Rng0> >(aggregate_tag(), std::forward<Rhs0>(rhs0)),
					reference_or_value< index_range_t<Rng1> >(aggregate_tag(), std::forward<Rhs1>(rhs1))
				),
				m_comp(std::forward<Comp>(comp))
			{}

		private:
			template<typename Func>
			struct FForwardFirstArgOnly final {
				Func& m_func;

				explicit FForwardFirstArgOnly(Func& func) noexcept : m_func(func)
				{}

				template<typename T0, typename T1>
				tc::break_or_continue operator()(T0&& arg0, T1&&) const {
					return tc::continue_if_not_break(m_func, std::forward<T0>(arg0));
				}

			};

		public:
			template< typename Func >
			auto operator()(Func func) const/* no & */ MAYTHROW -> break_or_continue
			{
				return tc::interleave(
					boost::implicit_cast<Rng0 const&>(*std::get<0>(m_baserng)),
					boost::implicit_cast<Rng1 const&>(*std::get<1>(m_baserng)),
					m_comp,
					std::ref(func),
					std::ref(func),
					FForwardFirstArgOnly<Func>(func)
				);
			}

			template< typename Func >
			auto operator()(Func func) /* no & */ MAYTHROW -> break_or_continue
			{
				return tc::interleave(
					boost::implicit_cast<Rng0&>(*std::get<0>(m_baserng)),
					boost::implicit_cast<Rng1&>(*std::get<1>(m_baserng)),
					m_comp,
					std::ref(func),
					std::ref(func),
					FForwardFirstArgOnly<Func>(func)
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
				: m_tplindex(std::forward<Args>(args)...)
			{}

			template<typename... Args>
			union_adaptor_index(tc::order order, Args... args) noexcept
				: m_tplindex(std::forward<Args>(args)...)
				, m_order(order)
			{}

			std::tuple<Index0, Index1> m_tplindex;
			tc::order m_order;
		};

		template<
			typename Comp,
			typename Rng0,
			typename Rng1
		>
		struct union_adaptor<Comp, Rng0, Rng1, true> :
			union_adaptor<Comp, Rng0, Rng1, false>,
			range_iterator_from_index<
				union_adaptor<Comp, Rng0, Rng1, true>,
				union_adaptor_index<
					typename std::remove_reference_t<
						index_range_t<Rng0>
					>::index,
					typename std::remove_reference_t<
						index_range_t<Rng1>
					>::index
				>,
				tc::demote_iterator_traversal_tag_t<
					boost::iterators::bidirectional_traversal_tag,
					traversal_t<Rng0>,
					traversal_t<Rng1>
				>
			>
		{
		private:
			using this_type = union_adaptor;

		public:
			using index = typename union_adaptor::index;

			template<typename Rhs0, typename Rhs1, typename Comp>
			explicit union_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp&& comp) noexcept
				: union_adaptor<Comp, Rng0, Rng1, false>(std::forward<Rhs0>(rhs0), std::forward<Rhs1>(rhs1), std::forward<Comp>(comp))
			{}

		private:
			void find_order(index& idx) const& noexcept {
				if (at_end_index_fwd<0>(idx)) {
					idx.m_order = at_end_index_fwd<1>(idx) ? tc::order::less : tc::order::greater;
				} else if (at_end_index_fwd<1>(idx)) {
					idx.m_order = tc::order::less;
				} else {
					idx.m_order = this->m_comp(dereference_index_fwd<0>(idx), dereference_index_fwd<1>(idx));
				}
			}

			template<int N, typename Index>
			auto get_idx(Index&& index) & noexcept return_decltype(
				std::get<N>(index.m_tplindex)
			)

			template<int N, typename Index>
			auto get_idx(Index&& index) const& noexcept return_decltype(
				std::get<N>(index.m_tplindex)
			)

			template<int N>
			auto baserng() & noexcept return_decltype(
				std::get<N>(this->m_baserng)
			)

			template<int N>
			auto baserng() const& noexcept return_decltype(
				std::get<N>(this->m_baserng)
			)

			template<int N>
			void increment_index_fwd(index& idx) const& noexcept {
				baserng<N>()->increment_index(get_idx<N>(idx));
			}

			template<int N>
			void decrement_index_fwd(index& idx) const& noexcept {
				baserng<N>()->decrement_index(get_idx<N>(idx));
			}

			template<int N>
			bool at_end_index_fwd(index const& idx) const& noexcept {
				return baserng<N>()->at_end_index(get_idx<N>(idx));
			}

			template<int N>
			bool at_begin_index(index const& idx) const& noexcept {
				return baserng<N>()->equal_index(get_idx<N>(idx), std::get<N>(this->m_baserng)->begin_index());
			}

			template<int N>
			bool equal_index_fwd(index const& lhs, index const& rhs) const& noexcept {
				return baserng<N>()->equal_index(get_idx<N>(lhs), get_idx<N>(rhs));
			}

			template<int N>
			auto dereference_index_fwd(index const& idx) const& noexcept return_decltype(
				std::get<N>(this->m_baserng)->dereference_index(get_idx<N>(idx))
			)

		public:

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::order::less == idx.m_order && at_end_index_fwd<0>(idx);
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				index idx(baserng<0>()->begin_index(), baserng<1>()->begin_index());
				find_order(idx);
				return idx;
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept ->
			tc::lvalue_or_decay_t<common_reference_t<
				typename range_traits<Rng0>::reference,
				typename range_traits<Rng1>::reference
			>> {
				return VERIFYINITIALIZED(idx.m_order) < tc::order::greater
					? dereference_index_fwd<0>(idx)
					: dereference_index_fwd<1>(idx);
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				index idx(tc::order::less, baserng<0>()->end_index(), baserng<1>()->end_index());
				return idx;
			}

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return equal_index_fwd<0>(idxLhs, idxRhs) &&
					equal_index_fwd<1>(idxLhs, idxRhs);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				switch_no_default( VERIFYINITIALIZED(idx.m_order) ) {
					case tc::order::less:
						increment_index_fwd<0>(idx);
						break;
					case tc::order::equal:
						increment_index_fwd<0>(idx);
						BOOST_FALLTHROUGH;
					case tc::order::greater:
						increment_index_fwd<1>(idx);
						break;
				}

				find_order(idx);
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				if (at_begin_index<0>(idx)) {
					_ASSERT(!at_begin_index<1>(idx));
					decrement_index_fwd<1>(idx);
					idx.m_order = tc::order::greater;
				} else if (at_begin_index<1>(idx)) {
					_ASSERT(!at_begin_index<0>(idx));
					decrement_index_fwd<0>(idx);
					idx.m_order = tc::order::less;
				} else {
					auto idxOriginal = idx;
					decrement_index_fwd<0>(idx);
					decrement_index_fwd<1>(idx);
					idx.m_order = -this->m_comp(dereference_index_fwd<0>(idx), dereference_index_fwd<1>(idx));
					switch_no_default(idx.m_order) {
						case tc::order::less:
							get_idx<1>(idx) = tc_move_always(get_idx<1>(idxOriginal));
							break;
						case tc::order::greater:
							get_idx<0>(idx) = tc_move_always(get_idx<0>(idxOriginal));
							break;
						case tc::order::equal:
							break;
					}
				}
			}

			// partition_point would be a more efficient customization point
			STATIC_FINAL(middle_point)(index& idx, index const& idxEnd) const& noexcept -> void {
				if (equal_index_fwd<0>(idx, idxEnd)) {
					baserng<1>()->middle_point(get_idx<1>(idx), get_idx<1>(idxEnd));
					idx.m_order = tc::order::greater;
				} else {
					auto idx0Begin = get_idx<0>(idx);
					baserng<0>()->middle_point(get_idx<0>(idx), get_idx<0>(idxEnd));
					auto&& ref0 = dereference_index_fwd<0>(idx);
					get_idx<1>(idx) = iterator2index(
						tc::lower_bound(
							std::get<1>(tc::as_mutable(this->m_baserng))->make_iterator(get_idx<1>(idx)),
							std::get<1>(tc::as_mutable(this->m_baserng))->make_iterator(get_idx<1>(idxEnd)),
							ref0,
							tc::greaterfrom3way(std::bind(std::ref(this->m_comp), std::placeholders::_2, std::placeholders::_1))
						)
					);

					if (at_end_index_fwd<1>(idx)) {
						idx.m_order = tc::order::less;
					} else {
						auto&& ref1 = dereference_index_fwd<1>(idx);

						idx.m_order = this->m_comp(ref0, ref1);
						_ASSERT(idx.m_order < tc::order::greater);

						if (tc::order::equal == idx.m_order) {
							auto idx0 = get_idx<0>(idx);
							typename boost::range_size<Rng0>::type n = 0;
							while (!baserng<0>()->equal_index(idx0Begin, idx0)) {
								baserng<0>()->decrement_index(idx0);

								if (tc::order::equal != this->m_comp(baserng<0>()->dereference_index(idx0), ref1)) {
									break;
								}
								++n;
							}
							while (0<n) {
								increment_index_fwd<1>(idx);
								--n;

								if (at_end_index_fwd<1>(idx) || tc::order::less==this->m_comp(ref0, dereference_index_fwd<1>(idx))) {
									idx.m_order = tc::order::less;
									break;
								}
							}
						}
					}
				}
			}
		};
	}

	using union_adaptor_adl_barrier::union_adaptor;

	template<typename Comp, typename Rng0, typename Rng1>
	auto split_union(sub_range<union_adaptor<Comp, Rng0, Rng1> &> const& rngsubunion) noexcept return_decltype(
		std::make_tuple(
			tc::slice(*std::get<0>(rngsubunion.base_range().m_baserng), std::get<0>(rngsubunion.begin_index().m_tplindex), std::get<0>(rngsubunion.end_index().m_tplindex)),
			tc::slice(*std::get<1>(rngsubunion.base_range().m_baserng), std::get<1>(rngsubunion.begin_index().m_tplindex), std::get<1>(rngsubunion.end_index().m_tplindex))
		)
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto union_range(Rng0&& rng0, Rng1&& rng1, Comp&& comp) noexcept return_ctor(
		union_adaptor< tc::decay_t<Comp> BOOST_PP_COMMA() view_by_value_t<Rng0> BOOST_PP_COMMA() view_by_value_t<Rng1>>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto union_range(Rng0&& rng0, Rng1&& rng1) noexcept return_decltype(
		union_range(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), tc::fn_compare())
	)
}
