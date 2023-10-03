
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../algorithm/compare.h"
#include "range_adaptor.h"
#include "meta.h"
#include "repeat_n.h"
#include <tuple>


namespace tc {

	namespace no_adl {

		template<
			typename RngPairIndexValue,
			typename TValue,
			bool HasIterator = tc::range_with_iterators< RngPairIndexValue >
		>
		struct [[nodiscard]] sparse_adaptor;

		template<
			typename RngPairIndexValue,
			typename TValue
		>
		struct [[nodiscard]] sparse_adaptor<RngPairIndexValue, TValue, false> : tc::range_adaptor_base_range<RngPairIndexValue> {
		protected:
			std::size_t m_nEnd;
			reference_or_value<TValue> m_default;

		public:
			template<typename Rhs, typename RHSValue>
			explicit sparse_adaptor(Rhs&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue) noexcept
				: tc::range_adaptor_base_range<RngPairIndexValue>(aggregate_tag, std::forward<Rhs>(rngpairIndexValue))
				, m_nEnd(nEnd)
				, m_default(aggregate_tag, std::forward<RHSValue>(defaultValue))
			{}

			template<typename Func>
			auto operator()(Func func) const& MAYTHROW {
				std::size_t n=0;
				auto const GenerateDefaultUpTo = [&](std::size_t const nEnd) MAYTHROW {
					_ASSERT( n <= nEnd );
					auto bc=tc::for_each(tc::repeat_n(nEnd - n, *m_default), func); // MAYTHROW
					n = nEnd;
					return bc;
				};
				tc_return_if_break(tc::for_each(this->base_range(), [&](auto&& pairindexvalue) MAYTHROW {
					tc_return_if_break(GenerateDefaultUpTo(get<0>(pairindexvalue))); // MAYTHROW
					++n;
					return tc::continue_if_not_break(func, tc::get<1>(tc_move_if_owned(pairindexvalue))); // MAYTHROW
				}));
				return GenerateDefaultUpTo(m_nEnd); // MAYTHROW
			}
		};

		template<typename Index>
		struct sparse_adaptor_index {
			std::size_t m_n;
			Index m_idxBase;

			friend bool operator==(sparse_adaptor_index const& lhs, sparse_adaptor_index const& rhs) noexcept {
				return EQUAL_MEMBERS(m_n);
			}
		};

		template<
			typename RngPairIndexValue,
			typename TValue
		>
		struct [[nodiscard]] sparse_adaptor<RngPairIndexValue, TValue, true> :
			sparse_adaptor<RngPairIndexValue, TValue, false>,
			range_iterator_from_index<
				sparse_adaptor<RngPairIndexValue, TValue>,
				sparse_adaptor_index<tc::index_t<std::remove_reference_t<RngPairIndexValue>>>
			>
		{
			using typename sparse_adaptor::range_iterator_from_index::tc_index;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<RngPairIndexValue>>::value;

			template<typename RhsRngPairIndexValue, typename RHSValue>
			explicit sparse_adaptor(RhsRngPairIndexValue&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue) noexcept :
				sparse_adaptor<RngPairIndexValue, TValue, false>(std::forward<RhsRngPairIndexValue>(rngpairIndexValue), nEnd, std::forward<RHSValue>(defaultValue))
			{}

		private:
			using this_type = sparse_adaptor;
			bool IndexIsDefault(tc_index const& idx) const& noexcept {
				_ASSERT(idx.m_n < this->m_nEnd);
				return tc::at_end_index(this->base_range(),idx.m_idxBase) ||
					idx.m_n != tc::as_unsigned(tc::get<0>(tc::dereference_index(this->base_range(),idx.m_idxBase)));
			}

			STATIC_FINAL(begin_index)() const& noexcept -> tc_index {
				return {0, this->base_begin_index()};
			}

			STATIC_FINAL(end_index)() const& noexcept -> tc_index {
				return {this->m_nEnd, this->base_end_index()};
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& noexcept -> void {
				if (!IndexIsDefault(idx)) {
					tc::increment_index(this->base_range(),idx.m_idxBase);
				}
				++idx.m_n;
			}

			STATIC_FINAL(at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return idx.m_n == this->m_nEnd;
			}

			STATIC_FINAL(dereference_index)(tc_index const& idx) const& return_decltype_MAYTHROW(
				IndexIsDefault(idx)
					? *(this->m_default)
					: tc::get<1>(tc::dereference_index(this->base_range(),idx.m_idxBase))
			)

		};
	}

	template<
		typename RngPairIndexValue,
		typename TValue = typename std::tuple_element<1, tc::range_value_t<RngPairIndexValue>>::type
	>
	auto sparse_range(RngPairIndexValue&& rngpairindexvalue, std::size_t nsizerng, TValue&& valueDefault = TValue()) return_ctor_noexcept(
		TC_FWD(no_adl::sparse_adaptor< RngPairIndexValue, TValue >),
		(std::forward<RngPairIndexValue>(rngpairindexvalue), nsizerng, std::forward<TValue>(valueDefault))
	)
}
