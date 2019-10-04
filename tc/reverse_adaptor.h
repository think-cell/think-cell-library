
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
#include "size.h"
#include "modified.h"
#include "sub_range.h"

namespace tc {
	namespace no_adl {
		template<typename Rng>
		struct [[nodiscard]] reverse_adaptor<Rng, false> {
		protected:
			reference_or_value<Rng> m_baserng;

		private:
			template<typename Rng_, typename Func>
			static auto internal_enumerate(Rng_&& baserng, Func&& func, int) MAYTHROW return_decltype(
				tc::remove_cvref_t<Rng_>::enumerate_reversed(std::forward<Rng_>(baserng), std::forward<Func>(func))
			)
			template<typename Rng_, typename Func>
			static auto internal_enumerate(Rng_&& baserng, Func&& func, ...) MAYTHROW return_decltype(
				internal_enumerate(tc::make_iterator_range(tc::begin(baserng), tc::end(baserng)), std::forward<Func>(func), 0)
			)

		public:
			template<typename RngRef>
			explicit reverse_adaptor(aggregate_tag_t, RngRef&& rng) :
				m_baserng(reference_or_value< Rng >(aggregate_tag, std::forward<RngRef>(rng)))
			{}

			template<typename Func>
			auto operator()(Func&& func) const/* no & */ MAYTHROW return_decltype(
				internal_enumerate(*m_baserng, std::forward<Func>(func), 0)
			)
			template<typename Func>
			auto operator()(Func&& func) /* no & */ MAYTHROW return_decltype(
				internal_enumerate(*m_baserng, std::forward<Func>(func), 0)
			)

			template<typename This, typename Func>
			static auto enumerate_reversed(This&& rngThis, Func&& func) MAYTHROW return_decltype(
				tc::for_each(*std::forward<This>(rngThis).m_baserng, std::forward<Func>(func))
			)

			constexpr decltype(auto) base_range() & noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() const& noexcept {
				return *m_baserng;
			}
			constexpr decltype(auto) base_range() && noexcept {
				return *std::move(m_baserng);
			}
			constexpr decltype(auto) base_range() const&& noexcept {
				return *std::move(m_baserng);
			}
		};

		template<typename Rng>
		struct [[nodiscard]] reverse_adaptor<Rng, true>
			: reverse_adaptor<Rng, false>
			, tc::range_iterator_from_index<
				reverse_adaptor<Rng>,
				std::optional<
					tc::index_t<std::remove_reference_t<
						Rng
					>>
				>,
				traversal_t<Rng>
			>
		{
		private:
			using this_type = reverse_adaptor;
		public:
			using index = typename reverse_adaptor::index;
			using reverse_adaptor<Rng, false>::reverse_adaptor;

			STATIC_FINAL(begin_index)() const& noexcept -> index {
				auto idx = tc::end_index(this->m_baserng);
				if( tc::equal_index(*this->m_baserng,tc::begin_index(this->m_baserng),idx) ) {
					return std::nullopt;
				} else {
					tc::decrement_index(*this->m_baserng,idx);
					return idx;
				}
			}

			STATIC_FINAL(end_index)() const& noexcept -> index {
				return std::nullopt;
			}

			STATIC_FINAL(at_end_index)(index const& idx) const& noexcept -> bool {
				return !tc::bool_cast(idx);
			}

			STATIC_FINAL(increment_index)(index& idx) const& noexcept -> void {
				if (tc::equal_index(*this->m_baserng,tc::begin_index(this->m_baserng), *idx)) {
					idx = std::nullopt;
				} else {
					tc::decrement_index(*this->m_baserng,*idx);
				}
			}

			STATIC_FINAL(decrement_index)(index& idx) const& noexcept -> void {
				if (idx) {
					tc::increment_index(*this->m_baserng,*idx);
				} else {
					idx = tc::begin_index(this->m_baserng);
				}
			}

			STATIC_FINAL(dereference_index)(index const& idx) const& noexcept return_decltype(
				tc::dereference_index(*this->m_baserng,*idx)
			)

			STATIC_FINAL(dereference_index)(index const& idx) & noexcept return_decltype(
				tc::dereference_index(*this->m_baserng,*idx)
			)

			STATIC_FINAL(equal_index)(index const& idxLhs, index const& idxRhs) const& noexcept -> bool {
				return tc::bool_cast(idxLhs) == tc::bool_cast(idxRhs) && (!idxLhs || tc::equal_index(*this->m_baserng,*idxLhs, *idxRhs));
			}

			STATIC_FINAL(distance_to_index)(index const& idxLhs, index const& idxRhs) const& noexcept {
				return tc::distance_to_index(*this->m_baserng, idxRhs ? *idxRhs : tc::begin_index(this->m_baserng), idxLhs ? *idxLhs : tc::begin_index(this->m_baserng)) + (idxRhs ? 0 : 1) + (idxLhs ? 0 : -1);
			}

			STATIC_FINAL(advance_index)(index& idx, typename boost::range_difference<Rng>::type d) const& noexcept -> void {
				if (idx) {
					tc::advance_index(*this->m_baserng,*idx, -(d-1));
					if (tc::equal_index(*this->m_baserng,tc::begin_index(this->m_baserng), *idx)) {
						idx = std::nullopt;
					} else {
						tc::decrement_index(*this->m_baserng,*idx);
					}
				} else {
					if (0 != d) {
						_ASSERT(d < 0);
						idx = tc::begin_index(this->m_baserng);
						tc::advance_index(*this->m_baserng,*idx, -(d+1));
					}
				}
			}

			auto border_base_index(index const& idx) const& noexcept {
				return idx ? modified(*idx, tc::increment_index(*this->m_baserng,_)) : tc::begin_index(this->m_baserng);
			}

			auto element_base_index(index const& idx) const& noexcept {
				_ASSERT(!this->at_end_index(idx));
				return *idx;
			}

			STATIC_FINAL(middle_point)(index & idx, index const& idxEnd ) const& noexcept -> void {
				auto idxBeginBase = border_base_index(idxEnd);
				tc::middle_point(*this->m_baserng, idxBeginBase, border_base_index(idx));
				idx = idxBeginBase;
			}

			template <typename It>
			void take_inplace(It&& it) & noexcept {
				tc::drop_inplace(*this->m_baserng, it.border_base());
			}

			template <typename It>
			void drop_inplace(It&& it) & noexcept {
				tc::take_inplace(*this->m_baserng, it.border_base());
			}
		};
	}

	template<typename Rng>
	auto reverse(Rng&& rng) noexcept return_ctor(
		reverse_adaptor< Rng >,
		(aggregate_tag, std::forward<Rng>(rng))
	)

	DEFINE_FN( reverse )
}
