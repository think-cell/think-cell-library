#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "../modified.h"
#include "../compare.h"

namespace RANGE_PROPOSAL_NAMESPACE {

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
				reference_or_value< typename index_range<Rng0>::type >,
				reference_or_value< typename index_range<Rng1>::type >
			> m_baserng;

		protected:
			Comp m_comp;

		public:
			template<typename Rhs0, typename Rhs1, typename Comp>
			union_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp&& comp)
				: m_baserng(
					reference_or_value< typename index_range<Rng0>::type >(std::forward<Rhs0>(rhs0), aggregate_tag()),
					reference_or_value< typename index_range<Rng1>::type >(std::forward<Rhs1>(rhs1), aggregate_tag())
				),
				m_comp(std::forward<Comp>(comp))
			{}

		private:
			template<typename Func>
			struct FForwardFirstArgOnly {
				Func& m_func;

				explicit FForwardFirstArgOnly(Func& func) : m_func(func)
				{}

				template<typename T0, typename T1>
				tc::break_or_continue operator()(T0&& arg0, T1&&) const {
					return tc::continue_if_not_break(m_func, std::forward<T0>(arg0));
				}

			};

		public:
			template< typename Func >
			auto operator()(Func func) const -> break_or_continue
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
			auto operator()(Func func) -> break_or_continue
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
			union_adaptor_index(Args... args)
				: m_tplindex(std::forward<Args>(args)...)
			{}

			template<typename... Args>
			union_adaptor_index(tc::order order, Args... args)
				: m_tplindex(std::forward<Args>(args)...)
				, m_order(order)
			{}

			std::tuple<Index0, Index1> m_tplindex;
			tc::order m_order;
		};

		template<typename Comp, typename Rng0, typename Rng1>
		using union_adaptor_base = range_iterator_from_index<
			union_adaptor<Comp, Rng0, Rng1, true>,
			union_adaptor_index<
				typename std::remove_reference<
					typename index_range<Rng0>::type
				>::type::index,
				typename std::remove_reference<
					typename index_range<Rng1>::type
				>::type::index
			>,
			typename boost::range_detail::demote_iterator_traversal_tag<
				boost::iterators::bidirectional_traversal_tag,
				typename boost::range_detail::demote_iterator_traversal_tag<
					traversal_t<Rng0>,
					traversal_t<Rng1>
				>::type
			>::type
		>;

		template<
			typename Comp,
			typename Rng0,
			typename Rng1
		>
		struct union_adaptor<Comp, Rng0, Rng1, true> :
			union_adaptor<Comp, Rng0, Rng1, false>,
			union_adaptor_base<Comp, Rng0, Rng1>
		{

			using index = typename union_adaptor::index;

			template<typename Rhs0, typename Rhs1, typename Comp>
			union_adaptor(Rhs0&& rhs0, Rhs1&& rhs1, Comp&& comp)
				: union_adaptor<Comp, Rng0, Rng1, false>(std::forward<Rhs0>(rhs0), std::forward<Rhs1>(rhs1), std::forward<Comp>(comp))
			{}

		private:

			using this_type = union_adaptor;

			void find_order(index& idx) const {
				if (at_end_index_fwd<0>(idx)) {
					idx.m_order = at_end_index_fwd<1>(idx) ? tc::order::less : tc::order::greater;
				} else if (at_end_index_fwd<1>(idx)) {
					idx.m_order = tc::order::less;
				} else {
					idx.m_order = this->m_comp(dereference_index_fwd<0>(idx), dereference_index_fwd<1>(idx));
				}
			}

			template<int N, typename Index>
			auto get_idx(Index&& index) return_decltype(
				std::get<N>(index.m_tplindex)
			)

			template<int N, typename Index>
			auto get_idx(Index&& index) const return_decltype(
				std::get<N>(index.m_tplindex)
			)

			template<int N>
			auto baserng() return_decltype(
				std::get<N>(THIS_IN_DECLTYPE m_baserng)
			)

			template<int N>
			auto baserng() const return_decltype(
				std::get<N>(THIS_IN_DECLTYPE m_baserng)
			)

			template<int N>
			void increment_index_fwd(index& idx) const {
				baserng<N>()->increment_index(get_idx<N>(idx));
			}

			template<int N>
			void decrement_index(index& idx) const {
				baserng<N>()->decrement_index(get_idx<N>(idx));
			}

			template<int N>
			bool at_end_index_fwd(index const& idx) const {
				return baserng<N>()->at_end_index(get_idx<N>(idx));
			}

			template<int N>
			bool at_begin_index(index const& idx) const {
				return baserng<N>()->equal_index(get_idx<N>(idx), std::get<N>(this->m_baserng)->begin_index());
			}

			template<int N>
			bool equal_index(index const& lhs, index const& rhs) const {
				return baserng<N>()->equal_index(get_idx<N>(lhs), get_idx<N>(rhs));
			}

			template<int N>
			auto dereference_index_fwd(index const& idx) const return_decltype(
				std::get<N>(THIS_IN_DECLTYPE m_baserng)->dereference_index(get_idx<N>(idx))
			)

		public:

			STATIC_FINAL(at_end_index)(index const& idx) const -> bool {
				return tc::order::less == idx.m_order && at_end_index_fwd<0>(idx);
			}

			STATIC_FINAL(begin_index)() const -> index {
				index idx(baserng<0>()->begin_index(), baserng<1>()->begin_index());
				find_order(idx);
				return idx;
			}

			STATIC_FINAL(dereference_index)(index const& idx) const ->
			typename reference_type<
				typename range_traits<std::remove_reference_t<Rng0> const>::reference,
				typename range_traits<std::remove_reference_t<Rng1> const>::reference
			>::type {
				return VERIFYINITIALIZED(idx.m_order) < tc::order::greater
					? dereference_index_fwd<0>(idx)
					: dereference_index_fwd<1>(idx);
			}

			STATIC_FINAL(end_index)() const -> index {
				index idx(tc::order::less, baserng<0>()->end_index(), baserng<1>()->end_index());
				return idx;
			}

			bool equal_index(index const& idxLhs, index const& idxRhs) const {
				return equal_index<0>(idxLhs, idxRhs) &&
					equal_index<1>(idxLhs, idxRhs);
			}

			STATIC_FINAL(increment_index)(index& idx) const -> void {
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

			void decrement_index(index& idx) const {
				if (at_begin_index<0>(idx)) {
					_ASSERT(!at_begin_index<1>(idx));
					decrement_index<1>(idx);
					idx.m_order = tc::order::greater;
				} else if (at_begin_index<1>(idx)) {
					_ASSERT(!at_begin_index<0>(idx));
					decrement_index<0>(idx);
					idx.m_order = tc::order::less;
				} else {
					auto idxOriginal = idx;
					decrement_index<0>(idx);
					decrement_index<1>(idx);
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
			void middle_point(index& idx, index const& idxEnd) const {
				if (equal_index<0>(idx, idxEnd)) {
					baserng<1>()->middle_point(get_idx<1>(idx), get_idx<1>(idxEnd));
					idx.m_order = tc::order::greater;
				} else {
					auto idx0Begin = get_idx<0>(idx);
					baserng<0>()->middle_point(get_idx<0>(idx), get_idx<0>(idxEnd));
					auto&& ref0 = dereference_index_fwd<0>(idx);
					get_idx<1>(idx) = iterator2index(
						tc::lower_bound(
							std::get<1>(tc::make_mutable(this->m_baserng))->make_iterator(get_idx<1>(idx)),
							std::get<1>(tc::make_mutable(this->m_baserng))->make_iterator(get_idx<1>(idxEnd)),
							ref0,
							tc::greaterfrom3way(boost::bind<tc::order>(std::ref(this->m_comp), _2, _1))
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
	auto split_union(sub_range<union_adaptor<Comp, Rng0, Rng1> &> const& rngsubunion) return_decltype(
		std::make_tuple(
			tc::slice(*std::get<0>(rngsubunion.base_range().m_baserng), std::get<0>(rngsubunion.begin_index().m_tplindex), std::get<0>(rngsubunion.end_index().m_tplindex)),
			tc::slice(*std::get<1>(rngsubunion.base_range().m_baserng), std::get<1>(rngsubunion.begin_index().m_tplindex), std::get<1>(rngsubunion.end_index().m_tplindex))
		)
	)

	template<typename Rng0, typename Rng1, typename Comp>
	auto union_range(Rng0&& rng0, Rng1&& rng1, Comp&& comp) return_ctor(
		union_adaptor< typename std::decay<Comp>::type BOOST_PP_COMMA() typename range_by_value<Rng0>::type BOOST_PP_COMMA() typename range_by_value<Rng1>::type>,
		(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), std::forward<Comp>(comp))
	)

	template<typename Rng0, typename Rng1>
	auto union_range(Rng0&& rng0, Rng1&& rng1) return_decltype(
		union_range(std::forward<Rng0>(rng0), std::forward<Rng1>(rng1), tc::fn_compare())
	)
}
