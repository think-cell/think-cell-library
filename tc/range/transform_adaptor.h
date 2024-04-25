
// think-cell public library
//
// Copyright (C) think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "../base/assert_defs.h"
#include "../base/move.h"

#include "range_adaptor.h"
#include "subrange.h"
#include "meta.h"

#include "transform.h"

namespace tc {
	namespace no_adl {
		template<typename Func, typename Sink>
		struct transform_sink /*final*/ {
			static_assert(tc::decayed<Sink>);
			using guaranteed_break_or_continue = guaranteed_break_or_continue_t<Sink>;
			Func const& m_func;
			Sink m_sink;

			template<typename T>
			constexpr auto operator()(T&& t) const& return_decltype_MAYTHROW(
				tc_invoke(m_sink, tc_invoke(m_func, tc_move_if_owned(t)))
			)
		};
	}

	namespace transform_adaptor_adl {
		template< typename Func, typename Rng, bool HasIterator=tc::range_with_iterators< Rng > >
		struct transform_adaptor;

		template< typename Func, typename Rng >
		struct [[nodiscard]] transform_adaptor<Func,Rng,false> : tc::generator_range_adaptor<Rng> {
		protected:
			static_assert(tc::decayed<Func>);
			Func m_func;

		public:
			constexpr transform_adaptor() = default;
			template< typename RngOther, typename FuncOther >
			constexpr transform_adaptor( RngOther&& rng, FuncOther&& func ) noexcept
				: transform_adaptor::generator_range_adaptor(aggregate_tag, tc_move_if_owned(rng))
				, m_func(tc_move_if_owned(func))
			{}

			constexpr auto size() const& MAYTHROW requires tc::has_size<Rng> {
				return tc::compute_range_adaptor_size<tc::identity{}>(this->base_range());
			}

			template<typename Sink>
			constexpr auto adapted_sink(Sink&& sink, bool /*bReverse*/) const& noexcept {
				return tc::no_adl::transform_sink<Func, tc::decay_t<Sink>>{m_func, tc_move_if_owned(sink)};
			}

			template<typename Self, std::enable_if_t<tc::decayed_derived_from<Self, transform_adaptor>>* = nullptr> // use terse syntax when Xcode supports https://cplusplus.github.io/CWG/issues/2369.html
			friend auto range_output_t_impl(Self&&) -> boost::mp11::mp_unique<tc::mp_transform<boost::mp11::mp_bind_front<tc::transform_output_t, Func>::template fn, tc::range_output_t<decltype(std::declval<Self>().base_range())>>> {} // unevaluated
		};


		template< typename Func, typename Rng >
		struct [[nodiscard]] transform_adaptor<Func, Rng, true>
			: tc::index_range_adaptor<
				transform_adaptor<Func, Rng, true>,
				Rng, tc::index_range_adaptor_flags::inherit_begin_end | tc::index_range_adaptor_flags::inherit_traversal,
				transform_adaptor<Func, Rng, false>
			>
		{
		private:
			using base_ = typename transform_adaptor::index_range_adaptor;
		public:
			using typename base_::tc_index;
			// TODO: static constexpr bool c_bHasStashingIndex=false if dereference returns a prvalue?
			static constexpr bool c_bPrefersForEach = tc::prefers_for_each<std::remove_reference_t<Rng>>;

			constexpr transform_adaptor() = default;
			using base_::base_;
			
			template <ENABLE_SFINAE, typename Index>
			constexpr auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(Index&& idx) & return_decltype_allow_xvalue_slow_MAYTHROW(
				// always call operator() const, which is assumed to be thread-safe
				tc_invoke(tc::as_const(SFINAE_VALUE(this->m_func)), tc::dereference_index(this->base_range(), tc_move_if_owned(idx)))
			)
			template <ENABLE_SFINAE, typename Index>
			constexpr auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(Index&& idx) const& return_decltype_allow_xvalue_slow_MAYTHROW(
				tc_invoke(SFINAE_VALUE(this->m_func), tc::dereference_index(this->base_range(), tc_move_if_owned(idx)))
			)

			static constexpr decltype(auto) border_base_index(auto&& idx) noexcept {
				return tc_move_if_owned(idx);
			}

			static constexpr decltype(auto) element_base_index(auto&& idx) noexcept {
				return tc_move_if_owned(idx);
			}

			constexpr decltype(auto) dereference_untransform(auto&& idx) const& MAYTHROW {
				return tc::dereference_index(this->base_range(), tc_move_if_owned(idx));
			}
		};
	}
	using transform_adaptor_adl::transform_adaptor;

	template<typename Func, typename Rng>
	constexpr auto enable_stable_index_on_move<tc::transform_adaptor<Func, Rng, true>> = tc::stable_index_on_move<Rng>;

	template<typename Rng, typename Func>
	[[nodiscard]] constexpr auto transform(Rng&& rng, Func&& func)
		return_ctor_noexcept(TC_FWD(transform_adaptor<tc::decay_t<Func>, Rng >), (tc_move_if_owned(rng), tc_move_if_owned(func)))

	// A range like `tc::concat` produces different types for generators, but only the common reference when used as an iterator.
	// Use `tc::transform_generator_only` if the transformation function does not work with the common reference.
	template<typename Rng, typename Func>
	[[nodiscard]] constexpr auto transform_generator_only(Rng&& rng, Func&& func)
		return_ctor_noexcept(TC_FWD(transform_adaptor<tc::decay_t<Func>, Rng, false >), (tc_move_if_owned(rng), tc_move_if_owned(func)))

	template<typename Rng>
	requires tc::instance_ttn<std::remove_reference_t<Rng>, transform_adaptor>
	[[nodiscard]] decltype(auto) untransform(Rng&& rng) noexcept {
		return tc_move_if_owned(rng).base_range();
	}

	template<typename Rng >
	requires tc::instance_ttn<std::remove_reference_t<tc::subrange_arg_t<Rng>>, transform_adaptor>
	[[nodiscard]] auto untransform(Rng&& rng) noexcept {
		return tc::slice(untransform(tc_move_if_owned(rng).base_range()), rng.begin_index(), rng.end_index());
	}
}

