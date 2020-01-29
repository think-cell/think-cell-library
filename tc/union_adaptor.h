
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "modified.h"
#include "compare.h"
#include "algorithm.h"

namespace tc {

	namespace no_adl {

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
			std::tuple<
				reference_or_value< Rng0 >,
				reference_or_value< Rng1 >
			> m_baserng;

		protected:
			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp2>
			explicit union_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp2&& comp) noexcept
				: m_baserng(
					reference_or_value< Rng0 >(aggregate_tag, std::forward<Rhs0>(rhs0)),
					reference_or_value< Rng1 >(aggregate_tag, std::forward<Rhs1>(rhs1))
				),
				m_comp(std::forward<Comp2>(comp))
			{}

		private:
			template<typename Func>
			struct FForwardFirstArgOnly final {
				Func& m_func;

				explicit FForwardFirstArgOnly(Func& func) noexcept : m_func(func)
				{}

				template<typename T0, typename T1>
				auto operator()(T0&& arg0, T1&&) const {
					return tc::continue_if_not_break(m_func, std::forward<T0>(arg0));
				}

			};

		public:
			template< typename Func >
			auto operator()(Func func) const/* no & */ MAYTHROW
			{
				return tc::interleave_2(
					*std::get<0>(m_baserng),
					*std::get<1>(m_baserng),
					m_comp,
					std::ref(func),
					std::ref(func),
					FForwardFirstArgOnly<Func>(func)
				);
			}

			template< typename Func >
			auto operator()(Func func) /* no & */ MAYTHROW
			{
				return tc::interleave_2(
					*std::get<0>(m_baserng),
					*std::get<1>(m_baserng),
					m_comp,
					std::ref(func),
					std::ref(func),
					FForwardFirstArgOnly<Func>(func)
				);
			}

			template<typename This, typename Func>
			static auto enumerate_reversed(This&& rngThis, Func&& func) MAYTHROW {
				return tc::interleave_2(
					tc::reverse(*std::get<0>(std::forward<This>(rngThis).m_baserng)),
					tc::reverse(*std::get<1>(std::forward<This>(rngThis).m_baserng)),
					/*comp*/[&](auto const& lhs, auto const& rhs) noexcept { return -rngThis.m_comp(lhs, rhs); },
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
			using index = typename union_adaptor::index;

			template<typename Rhs0, typename Rhs1, typename Comp2>
			explicit union_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp2&& comp) noexcept
				: union_adaptor<Comp2, Rng0, Rng1, false>(std::forward<Rhs0>(rhs0), std::forward<Rhs1>(rhs1), std::forward<Comp2>(comp))
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
			auto get_idx(Index&& index) & return_decltype_noexcept(
				std::get<N>(index.m_tplindex)
			)

			template<int N, typename Index>
			auto get_idx(Index&& index) const& return_decltype_noexcept(
				std::get<N>(index.m_tplindex)
			)

			template<int N>
			void increment_index_fwd(index& idx) const& noexcept {
				tc::increment_index(*std::get<N>(this->m_baserng),get_idx<N>(idx));
			}

			template<int N>
			void decrement_index_fwd(index& idx) const& noexcept {
				tc::decrement_index(*std::get<N>(this->m_baserng),get_idx<N>(idx));
			}

			template<int N>
			bool at_end_index_fwd(index const& idx) const& noexcept {
				return tc::at_end_index(*std::get<N>(this->m_baserng),get_idx<N>(idx));
			}

			template<int N>
			bool at_begin_index(index const& idx) const& noexcept {
				return tc::equal_index(*std::get<N>(this->m_baserng), get_idx<N>(idx), tc::begin_index(std::get<N>(this->m_baserng)));
			}

			template<int N>
			bool equal_index_fwd(index const& lhs, index const& rhs) const& noexcept {
				return tc::equal_index(*std::get<N>(this->m_baserng), get_idx<N>(lhs), get_idx<N>(rhs));
			}

			template<int N>
			auto dereference_index_fwd(index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(*std::get<N>(this->m_baserng), this->get_idx<N>(idx))
			)

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return tc::order::less == idx.m_order && at_end_index_fwd<0>(idx);
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				index idx(tc::begin_index(std::get<0>(this->m_baserng)), tc::begin_index(std::get<1>(this->m_baserng)));
				find_order(idx);
				return idx;
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept -> decltype(auto) {
				return CONDITIONAL_PRVALUE_AS_VAL( VERIFYINITIALIZED(idx.m_order) < tc::order::greater, dereference_index_fwd<0>(idx), dereference_index_fwd<1>(idx) );
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				index idx(tc::order::less, tc::end_index(std::get<0>(this->m_baserng)), tc::end_index(std::get<1>(this->m_baserng)));
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
						[[fallthrough]];
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
					tc::middle_point(*std::get<1>(this->m_baserng), get_idx<1>(idx), get_idx<1>(idxEnd));
					idx.m_order = tc::order::greater;
				} else {
					auto idx0Begin = get_idx<0>(idx);
					tc::middle_point(*std::get<0>(this->m_baserng), get_idx<0>(idx), get_idx<0>(idxEnd));
					auto&& ref0 = dereference_index_fwd<0>(idx);
					get_idx<1>(idx) = tc::iterator2index(
						tc::lower_bound(
							tc::make_iterator(*std::get<1>(tc::as_mutable(this->m_baserng)), get_idx<1>(idx)),
							tc::make_iterator(*std::get<1>(tc::as_mutable(this->m_baserng)), get_idx<1>(idxEnd)),
							ref0,
							tc::greaterfrom3way([&](auto const& _1, auto const& _2) noexcept { return this->m_comp(_2, _1); })
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
							while (!tc::equal_index(*std::get<0>(this->m_baserng), idx0Begin, idx0)) {
								tc::decrement_index(*std::get<0>(this->m_baserng), idx0);

								if (tc::order::equal != this->m_comp(tc::dereference_index(*std::get<0>(this->m_baserng), idx0), ref1)) {
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

	using no_adl::union_adaptor;

	template<typename Comp, typename Rng0, typename Rng1>
	[[nodiscard]] auto split_union(subrange<union_adaptor<Comp, Rng0, Rng1> &> const& rngsubunion) return_decltype_NOEXCEPT( // make_tuple is noexcept(false); the rest of this should only throw bad_alloc
		std::make_tuple(
			tc::slice(*std::get<0>(rngsubunion.base_range().m_baserng), std::get<0>(rngsubunion.begin_index().m_tplindex), std::get<0>(rngsubunion.end_index().m_tplindex)),
			tc::slice(*std::get<1>(rngsubunion.base_range().m_baserng), std::get<1>(rngsubunion.begin_index().m_tplindex), std::get<1>(rngsubunion.end_index().m_tplindex))
		)
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto union_range(Rng0&& rng0, Rng1&& rng1, Comp&& comp) return_ctor_noexcept(
		union_adaptor< tc::decay_t<Comp> BOOST_PP_COMMA() Rng0 BOOST_PP_COMMA() Rng1>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto union_range(Rng0&& rng0, Rng1&& rng1) return_decltype_noexcept(
		union_range(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), tc::fn_compare())
	)
}
