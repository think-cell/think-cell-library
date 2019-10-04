
// think-cell public library
//
// Copyright (C) 2016-2019 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "equality_comparable.h"
#include <tuple>


namespace tc {

	namespace no_adl {

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
		struct [[nodiscard]] sparse_adaptor<RngPairIndexValue, TValue, false> {
		protected:
			reference_or_value< RngPairIndexValue > m_baserng;
			std::size_t m_nEnd;
			reference_or_value<TValue> m_default;

		public:
			template<typename Rhs, typename RHSValue>
			explicit sparse_adaptor(Rhs&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue) noexcept
				: m_baserng(reference_or_value< RngPairIndexValue >(aggregate_tag, std::forward<Rhs>(rngpairIndexValue)))
				, m_nEnd(nEnd)
				, m_default(aggregate_tag, std::forward<RHSValue>(defaultValue))
			{}

		private:
			template<typename Func>
			struct FSparseRangeInput final {
				Func& m_func;
				reference_or_value<TValue> const& m_default;
				std::size_t& m_n;

				FSparseRangeInput(Func& func, reference_or_value<TValue> const& defaultValue, std::size_t& n) noexcept :
					m_func(func),
					m_default(defaultValue),
					m_n(n)
				{}

				template<typename PairIndexValue>
				auto operator()(PairIndexValue&& pairindexvalue) const& MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(m_func, *m_default)), INTEGRAL_CONSTANT(tc::continue_)> {
					for (; m_n < std::get<0>(pairindexvalue); ++m_n) {
						RETURN_IF_BREAK(tc::continue_if_not_break(m_func, *m_default));
					}
					RETURN_IF_BREAK(tc::continue_if_not_break(m_func, std::get<1>(std::forward<PairIndexValue>(pairindexvalue))));
					return INTEGRAL_CONSTANT(tc::continue_)();
				}
			};

		public:
			template<typename Func>
			auto operator()(Func func) const& MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *m_default)), INTEGRAL_CONSTANT(tc::continue_)>
			{
				std::size_t n=0;
				RETURN_IF_BREAK(tc::for_each(
					*m_baserng,
					FSparseRangeInput<Func>(func, m_default, n)
				));
				for (; n < m_nEnd; ++n) {
					RETURN_IF_BREAK(tc::continue_if_not_break(func, *m_default));
				}
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};

		template<typename Rng>
		struct sparse_adaptor_index {
			std::size_t m_n;
			tc::index_t<std::remove_reference_t<Rng>> m_idxBase;
		};

		template<
			typename RngPairIndexValue,
			typename TValue
		>
		struct [[nodiscard]] sparse_adaptor<RngPairIndexValue, TValue, true> :
			sparse_adaptor<RngPairIndexValue, TValue, false>,
			range_iterator_from_index<
				sparse_adaptor<RngPairIndexValue, TValue>,
				sparse_adaptor_index<RngPairIndexValue>,
				typename boost::range_detail::demote_iterator_traversal_tag<
					boost::iterators::forward_traversal_tag, // could be bidirectional, no use-case yet
					traversal_t<RngPairIndexValue>
				>::type
			>
		{
			using index=typename sparse_adaptor::index;

			template<typename RhsRngPairIndexValue, typename RHSValue>
			explicit sparse_adaptor(RhsRngPairIndexValue&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue) noexcept :
				sparse_adaptor<RngPairIndexValue, TValue, false>(std::forward<RhsRngPairIndexValue>(rngpairIndexValue), nEnd, std::forward<RHSValue>(defaultValue))
			{}

		private:
			using this_type = sparse_adaptor;
			bool IndexIsDefault(index const& idx) const& noexcept {
				_ASSERT(idx.m_n < this->m_nEnd);
				return tc::at_end_index(*this->m_baserng,idx.m_idxBase) ||
					idx.m_n != tc::unsigned_cast(std::get<0>(tc::dereference_index(*this->m_baserng,idx.m_idxBase)));
			}

		public:
			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return {0, tc::begin_index(this->m_baserng)};
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return {this->m_nEnd, tc::end_index(this->m_baserng)};
			}

			STATIC_FINAL(equal_index)(index const& lhs, index const& rhs) const& noexcept -> bool {
				return EQUAL_MEMBERS(m_n);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				if (!IndexIsDefault(idx)) {
					tc::increment_index(*this->m_baserng,idx.m_idxBase);
				}
				++idx.m_n;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return idx.m_n == this->m_nEnd;
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept return_decltype(
				IndexIsDefault(idx)
					? *(this->m_default)
					: std::get<1>(tc::dereference_index(*this->m_baserng,idx.m_idxBase))
			)

		};
	}

	template<
		typename RngPairIndexValue,
		typename TValue = typename std::tuple_element<1, tc::range_value_t<RngPairIndexValue>>::type
	>
	auto sparse_range(RngPairIndexValue&& rngpairindexvalue, std::size_t nsizerng, TValue&& valueDefault = TValue()) noexcept return_ctor(
		no_adl::sparse_adaptor< RngPairIndexValue BOOST_PP_COMMA() TValue >,
		(std::forward<RngPairIndexValue>(rngpairindexvalue), nsizerng, std::forward<TValue>(valueDefault))
	)
}
