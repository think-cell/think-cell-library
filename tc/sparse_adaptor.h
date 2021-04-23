
// think-cell public library
//
// Copyright (C) 2016-2021 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "assert_defs.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "equality_comparable.h"
#include "repeat_n.h"
#include <tuple>


namespace tc {

	namespace no_adl {

		template<
			typename RngPairIndexValue,
			typename TValue,
			bool HasIterator = is_range_with_iterators< RngPairIndexValue >::value
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
				auto GenerateDefaultUpTo = [&](std::size_t nEnd) MAYTHROW {
					_ASSERT( n <= nEnd );
					auto bc=tc::for_each(tc::repeat_n(nEnd - n, *m_default), func); // MAYTHROW
					n = nEnd;
					return bc;
				};
				RETURN_IF_BREAK(tc::for_each(this->base_range(), [&](auto&& pairindexvalue) MAYTHROW {
					RETURN_IF_BREAK(GenerateDefaultUpTo(get<0>(pairindexvalue))); // MAYTHROW
					++n;
					return tc::continue_if_not_break(func, /*adl*/get<1>(tc_move_if_owned(pairindexvalue))); // MAYTHROW
				}));
				return GenerateDefaultUpTo(m_nEnd); // MAYTHROW
			}
		};

		template<typename Index>
		struct sparse_adaptor_index : tc::equality_comparable<sparse_adaptor_index<Index>> {
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
			using typename sparse_adaptor::range_iterator_from_index::index;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<RngPairIndexValue>>::value;

			template<typename RhsRngPairIndexValue, typename RHSValue>
			explicit sparse_adaptor(RhsRngPairIndexValue&& rngpairIndexValue, std::size_t nEnd, RHSValue&& defaultValue) noexcept :
				sparse_adaptor<RngPairIndexValue, TValue, false>(std::forward<RhsRngPairIndexValue>(rngpairIndexValue), nEnd, std::forward<RHSValue>(defaultValue))
			{}

		private:
			using this_type = sparse_adaptor;
			bool IndexIsDefault(index const& idx) const& noexcept {
				_ASSERT(idx.m_n < this->m_nEnd);
				return tc::at_end_index(this->base_range(),idx.m_idxBase) ||
					idx.m_n != tc::unsigned_cast(/*adl*/get<0>(tc::dereference_index(this->base_range(),idx.m_idxBase)));
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				return {{}, 0, this->base_begin_index()};
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return {{}, this->m_nEnd, this->base_end_index()};
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				if (!IndexIsDefault(idx)) {
					tc::increment_index(this->base_range(),idx.m_idxBase);
				}
				++idx.m_n;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return idx.m_n == this->m_nEnd;
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& return_decltype_MAYTHROW(
				IndexIsDefault(idx)
					? *(this->m_default)
					: /*adl*/get<1>(tc::dereference_index(this->base_range(),idx.m_idxBase))
			)

		};
	}

	template<
		typename RngPairIndexValue,
		typename TValue = typename std::tuple_element<1, tc::range_value_t<RngPairIndexValue>>::type
	>
	auto sparse_range(RngPairIndexValue&& rngpairindexvalue, std::size_t nsizerng, TValue&& valueDefault = TValue()) return_ctor_noexcept(
		no_adl::sparse_adaptor< RngPairIndexValue BOOST_PP_COMMA() TValue >,
		(std::forward<RngPairIndexValue>(rngpairindexvalue), nsizerng, std::forward<TValue>(valueDefault))
	)
}
