
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../algorithm/compare.h"
#include "range_adaptor.h"
#include "meta.h"
#include "repeat_n.h"


namespace tc {

	namespace no_adl {
		template<typename Sink, typename TValue>
		struct sparse_adaptor_sink { // MSVC workaround: not a lambda for shorter symbol names
			Sink const m_sink;
			std::size_t const m_nEnd;
			TValue const m_default;
			std::size_t m_n=0;

			auto GenerateDefaultUpTo(std::size_t const nEnd) & MAYTHROW {
				_ASSERT( m_n <= nEnd );
				auto const bc=tc::for_each(tc::repeat_n(nEnd - m_n, m_default), m_sink); // MAYTHROW
				m_n = nEnd;
				return bc;
			}

			auto operator()(std::size_t const nIndex, auto&& value) & MAYTHROW {
				tc_return_if_break(GenerateDefaultUpTo(nIndex)); // MAYTHROW
				++m_n;
				return tc::continue_if_not_break(m_sink, tc_move_if_owned(value)); // MAYTHROW
			}
		};

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
			tc::reference_or_value<TValue> m_default;

		public:
			template<typename Rhs, typename RHSValue>
			explicit sparse_adaptor(Rhs&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue) noexcept
				: tc::range_adaptor_base_range<RngPairIndexValue>(aggregate_tag, tc_move_if_owned(rngpairIndexValue))
				, m_nEnd(nEnd)
				, m_default(aggregate_tag, tc_move_if_owned(defaultValue))
			{}

			auto operator()(auto&& sink) const& MAYTHROW {
				sparse_adaptor_sink<tc::decay_t<decltype(sink)>, decltype(*m_default)> adaptedsink{tc_move_if_owned(sink), m_nEnd, *m_default};
				tc_return_if_break(tc::for_each(this->base_range(), std::ref(adaptedsink))); // MAYTHROW
				return adaptedsink.GenerateDefaultUpTo(m_nEnd); // MAYTHROW
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
				sparse_adaptor<RngPairIndexValue, TValue, false>(tc_move_if_owned(rngpairIndexValue), nEnd, tc_move_if_owned(defaultValue))
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

			STATIC_FINAL(dereference_index)(auto&& idx) const& MAYTHROW -> decltype(auto) { // MSVC 19.39 gets confused when using return_decltype_MAYTHROW
				return IndexIsDefault(idx)
					? *(this->m_default)
					: tc::get<1>(tc::dereference_index(this->base_range(), tc_move_if_owned(idx).m_idxBase));
			}
		};
	}

	template<
		typename RngPairIndexValue,
		typename TValue = typename std::tuple_element<1, tc::range_value_t<RngPairIndexValue>>::type
	>
	auto sparse_range(RngPairIndexValue&& rngpairindexvalue, std::size_t nsizerng, TValue&& valueDefault = TValue()) return_ctor_noexcept(
		TC_FWD(no_adl::sparse_adaptor< RngPairIndexValue, TValue >),
		(tc_move_if_owned(rngpairindexvalue), nsizerng, tc_move_if_owned(valueDefault))
	)
}
