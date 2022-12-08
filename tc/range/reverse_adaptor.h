
// think-cell public library
//
// Copyright (C) 2016-2022 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/modified.h"
#include "../algorithm/size.h"
#include "range_fwd.h"
#include "range_adaptor.h"
#include "meta.h"
#include "subrange.h"

namespace tc {
	namespace reverse_adaptor_detail {
		namespace for_each_reverse_default {
			template<typename Rng, typename Sink>
			auto for_each_reverse_impl(Rng&& rng, Sink const sink) MAYTHROW
				-> tc::common_type_t<decltype(tc::continue_if_not_break(sink, tc::dereference_index(std::declval<Rng>(), tc::as_lvalue(tc::begin_index(std::declval<Rng&>()))))), tc::constant<tc::continue_>>
			{
				auto const idxBegin = tc::begin_index(rng);
				auto idxEnd = tc::end_index(rng);
				while( idxBegin != idxEnd ) {
					tc::decrement_index(rng, idxEnd);
					RETURN_IF_BREAK(tc::continue_if_not_break(sink, tc::dereference_index(rng, idxEnd)));
				}
				return tc::constant<tc::continue_>();
			}
		}

		DEFINE_TMPL_FUNC_WITH_CUSTOMIZATIONS(for_each_reverse)
	}

	namespace reverse_adaptor_adl {
		template<typename Rng>
		struct [[nodiscard]] reverse_adaptor<Rng, false> : tc::range_adaptor_base_range<Rng>, tc::range_output_from_base_range {
			using tc::range_adaptor_base_range<Rng>::range_adaptor_base_range;

			template<typename Self, typename Sink> requires tc::is_base_of_decayed<reverse_adaptor, Self>::value
			friend auto for_each_impl(Self&& self, Sink&& sink) return_MAYTHROW(
				reverse_adaptor_detail::for_each_reverse(std::forward<Self>(self).base_range(), std::forward<Sink>(sink))
			)

			template<typename Self, typename Sink> requires tc::is_base_of_decayed<reverse_adaptor, Self>::value
			friend auto for_each_reverse_impl(Self&& self, Sink&& sink) return_MAYTHROW(
				tc::for_each(std::forward<Self>(self).base_range(), std::forward<Sink>(sink))
			)
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
				>
			>
		{
		private:
			using this_type = reverse_adaptor;

			static_assert(tc::has_decrement_index<std::remove_reference_t<Rng>>::value, "Base range must have bidirectional traversal or it cannot be reversed");

		public:
			using typename reverse_adaptor::range_iterator_from_index::tc_index;
			using reverse_adaptor<Rng, false>::reverse_adaptor;
			static constexpr bool c_bHasStashingIndex=tc::has_stashing_index<std::remove_reference_t<Rng>>::value;

		private:
			STATIC_FINAL(begin_index)() const& noexcept -> tc_index {
				auto idx = this->base_end_index();
				if( this->base_begin_index() == idx ) {
					return std::nullopt;
				} else {
					tc::decrement_index(this->base_range(),idx);
					return idx;
				}
			}

			STATIC_FINAL(end_index)() const& noexcept -> tc_index {
				return std::nullopt;
			}

			STATIC_FINAL(at_end_index)(tc_index const& idx) const& noexcept -> bool {
				return !tc::explicit_cast<bool>(idx);
			}

			STATIC_FINAL(increment_index)(tc_index& idx) const& noexcept -> void {
				if (this->base_begin_index() == *idx) {
					idx = std::nullopt;
				} else {
					tc::decrement_index(this->base_range(),*idx);
				}
			}

			STATIC_FINAL(decrement_index)(tc_index& idx) const& noexcept -> void {
				if (idx) {
					tc::increment_index(this->base_range(),*idx);
				} else {
					idx = this->base_begin_index();
				}
			}

			STATIC_FINAL(dereference_index)(tc_index const& idx) const& return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(),*idx)
			)

			STATIC_FINAL(dereference_index)(tc_index const& idx) & return_decltype_MAYTHROW(
				tc::dereference_index(this->base_range(),*idx)
			)

			STATIC_FINAL_MOD(
				TC_FWD(template<
					ENABLE_SFINAE,
					std::enable_if_t<tc::has_distance_to_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value>* = nullptr
				>),
			distance_to_index)(tc_index const& idxLhs, tc_index const& idxRhs) const& noexcept {
				return tc::distance_to_index(this->base_range(), idxRhs ? *idxRhs : this->base_begin_index(), idxLhs ? *idxLhs : this->base_begin_index()) + (idxRhs ? 0 : 1) + (idxLhs ? 0 : -1);
			}

			STATIC_FINAL_MOD(
				TC_FWD(template<
					ENABLE_SFINAE,
					std::enable_if_t<
						tc::has_advance_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value &&
						tc::has_decrement_index<std::remove_reference_t<SFINAE_TYPE(Rng)>>::value &&
						tc::is_equality_comparable<SFINAE_TYPE(tc_index)>::value
					>* = nullptr
				>),
				advance_index
			)(tc_index& idx, typename boost::range_difference<Rng>::type d) const& noexcept -> void {
				if (idx) {
					tc::advance_index(this->base_range(),*idx, -(d-1));
					if (this->base_begin_index() == *idx) {
						idx = std::nullopt;
					} else {
						tc::decrement_index(this->base_range(),*idx);
					}
				} else {
					if (0 != d) {
						_ASSERT(d < 0);
						idx = this->base_begin_index();
						tc::advance_index(this->base_range(),*idx, -(d+1));
					}
				}
			}
		public:
			auto border_base_index(tc_index const& idx) const& noexcept {
				return idx ? modified(*idx, tc::increment_index(this->base_range(),_)) : this->base_begin_index();
			}

			auto element_base_index(tc_index const& idx) const& noexcept {
				_ASSERT(!this->at_end_index(idx));
				return *idx;
			}
		private:
			STATIC_FINAL(middle_point)(tc_index & idx, tc_index const& idxEnd ) const& noexcept -> void {
				auto idxBeginBase = border_base_index(idxEnd);
				tc::middle_point(this->base_range(), idxBeginBase, border_base_index(idx));
				idx = idxBeginBase;
			}
		public:
			template <typename It>
			void take_inplace(It&& it) & noexcept {
				tc::drop_inplace(this->base_range(), it.border_base());
			}

			template <typename It>
			void drop_inplace(It&& it) & noexcept {
				tc::take_inplace(this->base_range(), it.border_base());
			}
		};
	}

	template<typename Rng>
	struct no_adl::constexpr_size_base<tc::reverse_adaptor<Rng>, void> : tc::constexpr_size<Rng> {};

	template<typename Rng>
	auto reverse(Rng&& rng) return_ctor_noexcept(
		reverse_adaptor< Rng >,
		(aggregate_tag, std::forward<Rng>(rng))
	)
	
	namespace no_adl {
		template<typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::reverse_adaptor<Rng, true>> : tc::is_index_valid_for_move_constructed_range<Rng> {};
	}
}
