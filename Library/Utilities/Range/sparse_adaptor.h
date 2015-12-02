#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include <tuple>


namespace RANGE_PROPOSAL_NAMESPACE {

	namespace sparse_adaptor_adl_barrier {

		template<
			typename RngPairIndexValue,
			typename TValue,
			bool HasIterator = is_range_with_iterators< RngPairIndexValue >::value
		>
		struct sparse_adaptor;

		template<
			typename RngPairIndexValue,
			typename TValue
		>
		struct sparse_adaptor<RngPairIndexValue, TValue, false> {
		protected:
			reference_or_value< typename index_range<RngPairIndexValue>::type > m_baserng;
			std::size_t m_nEnd;
			reference_or_value<TValue> m_default;

		public:
			template<typename Rhs, typename RHSValue>
			sparse_adaptor(Rhs&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue)
				: m_baserng(reference_or_value< typename index_range<RngPairIndexValue>::type >(std::forward<Rhs>(rngpairIndexValue), aggregate_tag()))
				, m_nEnd(nEnd)
				, m_default(std::forward<RHSValue>(defaultValue), aggregate_tag())
			{}

		private:
			template<typename Func>
			struct FSparseRangeInput {
				Func& m_func;
				reference_or_value<TValue> const& m_default;
				std::size_t& m_n;

				FSparseRangeInput(Func& func, reference_or_value<TValue> const& defaultValue, std::size_t& n) :
					m_func(func),
					m_default(defaultValue),
					m_n(n)
				{}

				template<typename PairIndexValue>
				auto operator()(PairIndexValue&& pairindexvalue) const -> tc::break_or_continue {
					for (; m_n < std::get<0>(pairindexvalue); ++m_n) {
						RETURN_IF_BREAK(tc::continue_if_not_break(m_func, *m_default));
					}
					RETURN_IF_BREAK(tc::continue_if_not_break(m_func, std::get<1>(std::forward<PairIndexValue>(pairindexvalue))));
					return tc::continue_;
				}
			};

		public:
			template<typename Func>
			auto operator()(Func func) const -> break_or_continue
			{
				std::size_t n=0;
				RETURN_IF_BREAK(tc::for_each(
					*m_baserng,
					FSparseRangeInput<Func>(func, m_default, n)
				));
				for (; n < m_nEnd; ++n) {
					RETURN_IF_BREAK(tc::continue_if_not_break(func, *m_default));
				}
				return tc::continue_;
			}
		};

		template<typename Rng>
		struct sparse_adaptor_index {
			std::size_t m_n;
			typename std::remove_reference<
				typename index_range<Rng>::type
			>::type::index m_idxBase;
		};

		template<typename RngPairIndexValue, typename DefaultValue>
		using sparse_adaptor_base = range_iterator_from_index<
			sparse_adaptor<RngPairIndexValue, DefaultValue>,
			sparse_adaptor_index<RngPairIndexValue>,
			typename boost::range_detail::demote_iterator_traversal_tag<
				boost::iterators::forward_traversal_tag, // could be bidirectional, no use-case yet
				traversal_t<RngPairIndexValue>
			>::type
		>;

		template<
			typename RngPairIndexValue,
			typename TValue
		>
		struct sparse_adaptor<RngPairIndexValue, TValue, true> :
			sparse_adaptor<RngPairIndexValue, TValue, false>,
			sparse_adaptor_base<RngPairIndexValue, TValue>
		{
			using index=typename sparse_adaptor::index;

			template<typename RhsRngPairIndexValue, typename RHSValue>
			sparse_adaptor(RhsRngPairIndexValue&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue) :
				sparse_adaptor<RngPairIndexValue, TValue, false>(std::forward<RhsRngPairIndexValue>(rngpairIndexValue), nEnd, std::forward<RHSValue>(defaultValue))
			{}

		private:
			using this_type = sparse_adaptor;
			bool IndexIsDefault(index const& idx) const {
				_ASSERT(idx.m_n < this->m_nEnd);
				return this->m_baserng->at_end_index(idx.m_idxBase) ||
					idx.m_n != tc::unsigned_cast(std::get<0>(this->m_baserng->dereference_index(idx.m_idxBase)));
			}

		public:
			STATIC_FINAL(begin_index)() const -> index {
				return {0, this->m_baserng->begin_index()};
			}

			STATIC_FINAL(end_index)() const -> index {
				return {this->m_nEnd, this->m_baserng->end_index()};
			}

			bool equal_index(index const& lhs, index const& rhs) const {
				return lhs.m_n == rhs.m_n;
			}

			STATIC_FINAL(increment_index)(index& idx) const -> void {
				if (!IndexIsDefault(idx)) {
					this->m_baserng->increment_index(idx.m_idxBase);
				}
				++idx.m_n;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const -> bool {
				return idx.m_n == this->m_nEnd;
			}

			bool at_begin_index(index const& idx) const {
				return 0 == idx.m_n;
			}

			STATIC_FINAL(dereference_index)(index const& idx) const return_decltype(
				IndexIsDefault(idx)
					? *THIS_IN_DECLTYPE m_default
					: std::get<1>(THIS_IN_DECLTYPE m_baserng->dereference_index(idx.m_idxBase))
			)

		};

	}

	using sparse_adaptor_adl_barrier::sparse_adaptor;

	template<
		typename RngPairIndexValue,
		typename TValue = typename std::tuple_element<1, typename tc::range_value<RngPairIndexValue>::type>::type
	>
	auto sparse_range(RngPairIndexValue&& rngpairindexvalue, std::size_t nsizerng, TValue&& valueDefault = TValue()) return_ctor(
		sparse_adaptor< typename range_by_value<RngPairIndexValue>::type BOOST_PP_COMMA() TValue >,
		(std::forward<RngPairIndexValue>(rngpairindexvalue), nsizerng, std::forward<TValue>(valueDefault))
	)
}