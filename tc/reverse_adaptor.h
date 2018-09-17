
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "types.h"
#include "size.h"
#include "modified.h"
#include "sub_range.h"

namespace tc {
	namespace adl_barrier {
		template<typename Rng>
		struct reverse_adaptor : tc::range_iterator_from_index<
			reverse_adaptor<Rng>,
			boost::optional<
				tc::index_t<std::remove_reference_t<
					Rng
				>>
			>,
			traversal_t<Rng>
		>
		{
			reference_or_value< Rng > m_baserng;
		private:
			using this_type = reverse_adaptor;
		public:
			using index = typename reverse_adaptor::index;

			template<typename RngRef>
			explicit reverse_adaptor(aggregate_tag, RngRef&& rng) :
				m_baserng(reference_or_value< Rng >(aggregate_tag(), std::forward<RngRef>(rng)))
			{}

			template< typename Func >
			auto operator()(Func func) /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(*m_baserng))), INTEGRAL_CONSTANT(tc::continue_)> {
					auto const itBegin=tc::begin(*m_baserng);
					auto itEnd=tc::end(*m_baserng);
					while( itEnd!=itBegin ) {
						--itEnd;
						RETURN_IF_BREAK(tc::continue_if_not_break(func, *itEnd));
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
			}

			template< typename Func >
			auto operator()(Func func) const /* no & */ MAYTHROW {
				return [&]() MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *tc::begin(*m_baserng))), INTEGRAL_CONSTANT(tc::continue_)> {
					auto const itBegin=tc::begin(*m_baserng);
					auto itEnd=tc::end(*m_baserng);
					while( itEnd!=itBegin ) {
						--itEnd;
						RETURN_IF_BREAK(tc::continue_if_not_break(func, *itEnd));
					}
					return INTEGRAL_CONSTANT(tc::continue_)();
				}();
			}

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				auto idx = tc::end_index(m_baserng);
				if( tc::equal_index(*m_baserng,tc::begin_index(m_baserng),idx) ) {
					return boost::none;
				} else {
					tc::decrement_index(*m_baserng,idx);
					return idx;
				}
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return boost::none;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return !tc::bool_cast(idx);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				if (tc::equal_index(*m_baserng,tc::begin_index(m_baserng), *idx)) {
					idx = boost::none;
				} else {
					tc::decrement_index(*m_baserng,*idx);
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				if (idx) {
					tc::increment_index(*m_baserng,*idx);
				} else {
					idx = tc::begin_index(m_baserng);
				}
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept return_decltype(
				tc::dereference_index(*m_baserng,*idx)
			)

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept return_decltype(
				tc::dereference_index(*m_baserng,*idx)
			)

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::bool_cast(idxLhs) == tc::bool_cast(idxRhs) && (!idxLhs || tc::equal_index(*m_baserng,*idxLhs, *idxRhs));
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept {
				return tc::distance_to_index(*m_baserng, idxRhs ? *idxRhs : tc::begin_index(m_baserng), idxLhs ? *idxLhs : tc::begin_index(m_baserng)) + (idxRhs ? 0 : 1) + (idxLhs ? 0 : -1);
			}

			STATIC_FINAL(advance_index)(index& idx, typename boost::range_difference<Rng>::type d) const& noexcept -> void {
				if (idx) {
					tc::advance_index(*m_baserng,*idx, -(d-1));
					if (tc::equal_index(*m_baserng,tc::begin_index(m_baserng), *idx)) {
						idx = boost::none;
					} else {
						tc::decrement_index(*m_baserng,*idx);
					}
				} else {
					if (0 != d) {
						_ASSERT(d < 0);
						idx = tc::begin_index(m_baserng);
						tc::advance_index(*m_baserng,*idx, -(d+1));
					}
				}
			}

			auto border_base_index(index const& idx) const& noexcept {
				return idx ? modified(*idx, tc::increment_index(*m_baserng,_)) : tc::begin_index(m_baserng);
			}

			auto element_base_index(index const& idx) const& noexcept {
				_ASSERT(!this->at_end_index(idx));
				return *idx;
			}

			auto base_range() & noexcept {
				return *m_baserng;
			}

			auto base_range() const & noexcept {
				return *m_baserng;
			}

			STATIC_FINAL(middle_point)(index & idx, index const& idxEnd ) const& noexcept -> void {
				auto idxBeginBase = border_base_index(idxEnd);
				tc::middle_point(*m_baserng, idxBeginBase, border_base_index(idx));
				idx = idxBeginBase;
			}

			template <typename It>
			void take_inplace(It&& it) & noexcept {
				tc::drop_inplace(*m_baserng, it.border_base());
			}

			template <typename It>
			void drop_inplace(It&& it) & noexcept {
				tc::take_inplace(*m_baserng, it.border_base());
			}
		};
	}
	using adl_barrier::reverse_adaptor;

	template<typename Rng>
	auto reverse(Rng&& rng) noexcept return_ctor(
		reverse_adaptor< Rng >,
		(aggregate_tag(), std::forward<Rng>(rng))
	)


}






