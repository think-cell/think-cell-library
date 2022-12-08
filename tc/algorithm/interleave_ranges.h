#pragma once

#include "../range/subrange.h"
#include "../range/filter_adaptor.h"

#include <boost/range/algorithm/heap_algorithm.hpp>

namespace tc {
	namespace no_adl {
		template<typename RngRng>
		struct interleave_ranges_index {
			template<bool bHasStashingElement = tc::is_stashing_element<tc::iterator_t<RngRng const&>>::value>
			struct SIteratorView;

			using RangeView = decltype(*std::declval<tc::iterator_t<RngRng const&>>());

			template<>
			struct SIteratorView<false> : std::conditional_t<std::is_lvalue_reference<RangeView>::value, tc::copyable, tc::noncopyable>
			{
				static_assert( tc::is_index_valid_for_move_constructed_range<RangeView>::value );

				explicit SIteratorView(tc::iterator_t<RngRng const&> const& itrng) noexcept
					: m_rng(tc::aggregate_tag, *itrng)
					, m_idx(tc::begin_index(*m_rng))
				{}

				void increment_index() & noexcept {
					tc::increment_index(*m_rng, m_idx);
				}

				bool empty() const& noexcept {
					return tc::at_end_index(*m_rng, m_idx);
				}

				auto dereference() const& ->decltype(auto) {
					return tc::dereference_index(*m_rng, m_idx);
				}
			private:
				tc::reference_or_value<RangeView> m_rng;
				tc::index_t<std::remove_reference_t<decltype(*m_rng)>> m_idx;
			};

			template<>
			struct SIteratorView<true> // Stashing iterator
				: private tc::iterator_t<RngRng const&>
				, public SIteratorView<false>
			{
				explicit SIteratorView(tc::iterator_t<RngRng const&> itrng) noexcept
					: tc::iterator_t<RngRng const&>(tc_move(itrng))
					, SIteratorView<false>(tc::base_cast<tc::iterator_t<RngRng const&>>(*this))
				{}
			};

			tc::vector<SIteratorView<>> m_vecview;
			std::size_t m_nLast;

			static_assert(!tc::is_stashing_element<tc::iterator_t<RngRng const&>>::value || std::is_copy_constructible<tc::iterator_t<RngRng const&>>::value);
			interleave_ranges_index(RngRng const& rng) noexcept
				: MEMBER_INIT_CAST(m_vecview, tc::filter(
					tc::make_range_of_iterators(rng),
					[](auto const& it) noexcept {
						return !tc::empty(*it);
					}
				))
			{}
		};

		template<typename RngRng, typename Less>
		struct interleave_ranges_adaptor
			: tc::range_adaptor_base_range<RngRng>
			, tc::range_iterator_from_index<
				interleave_ranges_adaptor<RngRng, Less>,
				interleave_ranges_index<RngRng>
			>
		{
		private:
			using this_type = interleave_ranges_adaptor;
		public:
			using typename this_type::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex = true;

			Less m_less;

			template<typename RngRng_, typename Less_>
			interleave_ranges_adaptor(RngRng_&& rngrng, Less_&& less) noexcept
				: tc::range_adaptor_base_range<RngRng>(tc::aggregate_tag, tc_move_if_owned(rngrng))
				, m_less(tc_move_if_owned(less))
			{}

			constexpr auto greater() const& noexcept {
				return tc::reverse_binary_rel(tc::projected(m_less, TC_MEM_FN(.dereference)));
			}

			constexpr void prepare_index(tc_index& idx) const& noexcept {
				auto_cref(greater, this->greater());
				idx.m_nLast = tc::size(idx.m_vecview);
				do {
					boost::range::pop_heap(tc::begin_next<tc::return_take>(idx.m_vecview, idx.m_nLast), greater);
					--idx.m_nLast;
				} while (0 < idx.m_nLast && !greater(tc::front(idx.m_vecview), tc::at(idx.m_vecview,idx.m_nLast) ));
			}

			STATIC_FINAL_MOD(constexpr, begin_index)() const& noexcept -> tc_index {
				tc_index idx(this->base_range());
				if (!this->at_end_index(idx)) {
					boost::range::make_heap(idx.m_vecview, this->greater());
					prepare_index(idx);
				}
				return idx;
			}

			STATIC_FINAL_MOD(constexpr, end_index)() const& noexcept -> tc_index {
				return {tc::empty_range(), 0};
			}

			STATIC_FINAL_MOD(constexpr, at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return tc::empty(idx.m_vecview);
			}

			STATIC_FINAL_MOD(constexpr, increment_index)(tc_index& idx) const& noexcept -> void {
				_ASSERTE(!this->at_end_index(idx));

				auto_cref(greater, this->greater());

				tc::filter_inplace(idx.m_vecview, tc::begin_next<tc::return_border>(idx.m_vecview, idx.m_nLast), [](auto& view) noexcept{
					view.increment_index();
					return !tc::empty(view);
				});
				tc::for_each(
					tc::iota(idx.m_nLast, tc::size(idx.m_vecview)),
					[&](auto const n) noexcept {
						boost::range::push_heap(tc::begin_next<tc::return_take>(idx.m_vecview, n+1), greater);
					}
				);

				if (!this->at_end_index(idx)) {
					prepare_index(idx);
				}
			}

			STATIC_FINAL_MOD(constexpr, dereference_index)(tc_index const& idx) const& {
				return tc::transform(
					tc::begin_next<tc::return_drop>(idx.m_vecview, idx.m_nLast),
					TC_MEM_FN(.dereference)
				);
			}
		};
	}

	template<typename RngRng,  typename Less = tc::fn_less>
	auto interleave_ranges(RngRng&& rngrng, Less&& less = Less()) {
		return no_adl::interleave_ranges_adaptor<RngRng, tc::decay_t<Less>>(tc_move_if_owned(rngrng), tc_move_if_owned(less));
	}

}
