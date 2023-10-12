
// think-cell public library
//
// Copyright (C) 2016-2023 think-cell Software GmbH
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
				tc::invoke(m_sink, tc::invoke(m_func, tc_move_if_owned(t)))
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
			friend auto range_output_t_impl(Self&&) -> tc::type::unique_t<tc::type::transform_t<tc::range_output_t<decltype(std::declval<Self>().base_range())>, tc::type::curry<tc::transform_output_t, Func>::template type>> {} // unevaluated
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
			// TODO: static constexpr bool c_bHasStashingIndex=false if transform_return_t is a value?

			constexpr transform_adaptor() = default;
			using base_::base_;

			template<ENABLE_SFINAE>
			constexpr auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(tc_index const& idx) & MAYTHROW -> tc::transform_return_t<
				SFINAE_TYPE(Func),
				decltype(tc::invoke(std::declval<SFINAE_TYPE(Func) const&>(), tc::dereference_index(this->base_range(), std::declval<tc_index const&>()))),
				decltype(tc::dereference_index(this->base_range(), std::declval<tc_index const&>()))
			> {
				// always call operator() const, which is assumed to be thread-safe
				return tc::invoke(tc::as_const(this->m_func), tc::dereference_index(this->base_range(), idx));
			}

			template<ENABLE_SFINAE>
			constexpr auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(tc_index const& idx) const& MAYTHROW -> tc::transform_return_t<
				SFINAE_TYPE(Func),
				decltype(tc::invoke(std::declval<SFINAE_TYPE(Func) const&>(), tc::dereference_index(this->base_range(), std::declval<tc_index const&>()))),
				decltype(tc::dereference_index(this->base_range(), std::declval<tc_index const&>()))
			> {
				// always call operator() const, which is assumed to be thread-safe
				return tc::invoke(tc::as_const(this->m_func), tc::dereference_index(this->base_range(), idx));
			}

			static constexpr decltype(auto) border_base_index(tc_index const& idx) noexcept {
				return idx;
			}

			static constexpr decltype(auto) border_base_index(tc_index&& idx) noexcept {
				return tc_move(idx);
			}

			static constexpr decltype(auto) element_base_index(tc_index const& idx) noexcept {
				return idx;
			}
			static constexpr decltype(auto) element_base_index(tc_index&& idx) noexcept {
				return tc_move(idx);
			}

			constexpr decltype(auto) dereference_untransform(tc_index const& idx) const& noexcept {
				return tc::dereference_index(this->base_range(), idx);
			}
		};
	}
	using transform_adaptor_adl::transform_adaptor;

	template<typename Func, typename Rng>
	constexpr auto enable_stable_index_on_move<tc::transform_adaptor<Func, Rng, true>> = tc::stable_index_on_move<Rng>;

	template<typename Rng, typename Func>
	[[nodiscard]] constexpr auto transform(Rng&& rng, Func&& func)
		return_ctor_noexcept(TC_FWD(transform_adaptor<tc::decay_t<Func>, Rng >), (tc_move_if_owned(rng), tc_move_if_owned(func)))

	template<typename Rng>
	requires tc::instance2<std::remove_reference_t<Rng>, transform_adaptor>
	[[nodiscard]] decltype(auto) untransform(Rng&& rng) noexcept {
		return tc_move_if_owned(rng).base_range();
	}

	template<typename Rng >
	requires tc::instance2<std::remove_reference_t<
			tc::type::only_t<
				typename tc::is_instance<std::remove_reference_t<Rng>, subrange>::arguments
			>
		>, transform_adaptor>
	[[nodiscard]] auto untransform(Rng&& rng) noexcept {
		return tc::slice(untransform(tc_move_if_owned(rng).base_range()), rng.begin_index(), rng.end_index());
	}
}

