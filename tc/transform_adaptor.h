
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
#include "subrange.h"
#include "meta.h"

#include "tc_move.h"
#include "transform.h"

namespace tc {
	namespace no_adl {
		template<typename Func, typename Sink>
		struct transform_sink /*final*/ {
			static_assert(tc::is_decayed<Sink>::value);
			using guaranteed_break_or_continue = guaranteed_break_or_continue_t<Sink>;
			Func const& m_func;
			Sink m_sink;

			template<typename T>
			constexpr auto operator()(T&& t) const& return_decltype_MAYTHROW(
				tc::invoke(m_sink, tc::invoke(m_func, std::forward<T>(t)))
			)
		};

		template< typename Func, typename Rng >
		struct [[nodiscard]] transform_adaptor<Func,Rng,false> : tc::generator_range_adaptor<Rng> {
		protected:
			static_assert( tc::is_decayed< Func >::value );
			Func m_func;

		public:
			constexpr transform_adaptor() = default;
			template< typename RngOther, typename FuncOther >
			constexpr transform_adaptor( RngOther&& rng, FuncOther&& func ) noexcept
				: transform_adaptor::generator_range_adaptor(aggregate_tag, std::forward<RngOther>(rng))
				, m_func(std::forward<FuncOther>(func))
			{}

			template< ENABLE_SFINAE, std::enable_if_t<tc::has_size<SFINAE_TYPE(Rng)>::value>* = nullptr >
			constexpr auto size() const& noexcept {
				return tc::size_raw(this->base_range());
			}

			template<typename Sink>
			constexpr auto adapted_sink(Sink&& sink, bool /*bReverse*/) const& noexcept {
				return transform_sink<Func, tc::decay_t<Sink>>{m_func, std::forward<Sink>(sink)};
			}
		};


		template< typename Func, typename Rng >
		struct [[nodiscard]] transform_adaptor<Func, Rng, true>
			: tc::index_range_adaptor<
				transform_adaptor<Func, Rng, true>,
				Rng,
				transform_adaptor<Func, Rng, false>
			>
		{
		private:
			using base_ = typename transform_adaptor::index_range_adaptor;
		public:
			using typename base_::index;
			// TODO: static constexpr bool c_bHasStashingIndex=false if transform_return_t is a value?

			constexpr transform_adaptor() = default;
			using base_::base_;

			template<ENABLE_SFINAE>
			constexpr auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(index const& idx) & MAYTHROW -> tc::transform_return_t<
				SFINAE_TYPE(Func),
				decltype(tc::invoke(std::declval<SFINAE_TYPE(Func) const&>(), std::declval<base_ &>().template dereference_index<base_>(std::declval<index const&>()))),
				decltype(std::declval<base_ &>().template dereference_index<base_>(std::declval<index const&>()))
			> {
				// always call operator() const, which is assumed to be thread-safe
				return tc::invoke(tc::as_const(this->m_func), this->template dereference_index<base_>(idx));
			}

			template<ENABLE_SFINAE>
			constexpr auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(index const& idx) const& MAYTHROW -> tc::transform_return_t<
				SFINAE_TYPE(Func),
				decltype(tc::invoke(std::declval<SFINAE_TYPE(Func) const&>(), std::declval<base_ const&>().template dereference_index<base_>(std::declval<index const&>()))),
				decltype(std::declval<base_ const&>().template dereference_index<base_>(std::declval<index const&>()))
			> {
				// always call operator() const, which is assumed to be thread-safe
				return tc::invoke(tc::as_const(this->m_func), this->template dereference_index<base_>(idx));
			}

			auto border_base_index(index const& idx) const& noexcept {
				return idx;
			}

			auto element_base_index(index const& idx) const& noexcept {
				return idx;
			}
		};

		template<typename Func, typename Rng, bool bConst>
		struct constexpr_size_base<tc::transform_adaptor<Func,Rng,bConst>, void> : tc::constexpr_size<Rng> {};

		template<typename Func, typename Rng>
		struct common_transform_range_value final {
			template<typename TransformValueT, typename /*AlternativeRngValueT*/, typename /*Enable*/=void>
			struct accumulate_fn final {
				using type = TransformValueT;
			};

			template<typename TransformValueT, typename AlternativeRngValueT>
			struct accumulate_fn<TransformValueT, AlternativeRngValueT, tc::void_t<tc::transform_value_t<Func, AlternativeRngValueT>>> final
				: tc::common_type_decayed<TransformValueT, tc::transform_value_t<Func, AlternativeRngValueT>>
			{};

			template<typename TransformValueT, typename AlternativeRngValueT>
			using accumulate_fn_t = typename accumulate_fn<TransformValueT, AlternativeRngValueT>::type;
		};

		// On a transform_adaptor, Func is invoked with perfectly forwarded references from the base range. Hence, in an ideal world, the range_value of a
		// transform_adaptor would be the common_type_decayed of the results of Func invocations with these references. For now, we have no way of gathering
		// these references and there is no obvious way to get to them. We workaround this fact by using the common_type_decayed on the results of Func being
		// invoked with different reference types derived from the range_value of the base range.

		// Note: this causes exponential number of template instantiations for code patterns like:
		//	 tc::make_vector(tc::transform(rng0, [](auto&& rng1) {
		//			return tc::make_vector(tc::transform(tc_move_if_owned(rng1), [](auto&& rng2) {
		//				return tc::make_vector(tc::transform(tc_move_if_owned(rng2), [](auto&& rng3) {
		//					// ..
		//				});
		//			});
		//		});
		template<typename TransformAdaptor, typename Func, typename Rng >
		struct range_value<TransformAdaptor, transform_adaptor<Func, Rng, false>, tc::void_t<tc::range_value_t<Rng>>> final
			: tc::type::accumulate<
				tc::transform_value_t<Func, tc::range_value_t<Rng>>, // Func is required to work on prvalues
				tc::type::list< // non-exhaustive alternative types (avoid template instantiations, see Note above)
					tc::range_value_t<Rng const>,
					tc::range_value_t<Rng> const&
				>,
				common_transform_range_value<Func, Rng>::template accumulate_fn_t
			>
		{};
	}

	template<typename Rng, std::enable_if_t<tc::is_instance2<transform_adaptor,std::remove_reference_t<Rng>>::value>* =nullptr >
	[[nodiscard]] decltype(auto) untransform(Rng&& rng) noexcept {
		return std::forward<Rng>(rng).base_range();
	}

	template<typename Rng, std::enable_if_t<
		tc::is_instance2<
			transform_adaptor,
			std::remove_reference_t<
				tc::type::only_t<
					typename tc::is_instance<subrange,std::remove_reference_t<Rng>>::arguments
				>
			>
		>::value
	>* =nullptr >
	[[nodiscard]] auto untransform(Rng&& rng) noexcept {
		return tc::slice(untransform(std::forward<Rng>(rng).base_range()), rng.begin_index(), rng.end_index());
	}

	namespace no_adl {
		template<typename Func, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::transform_adaptor<Func, Rng, true>, std::enable_if_t<std::is_lvalue_reference<Rng>::value>>: std::true_type {};
		
		template<typename Func, typename Rng>
		struct is_index_valid_for_move_constructed_range<tc::transform_adaptor<Func, Rng, true>, std::enable_if_t<!std::is_reference<Rng>::value>>: tc::is_index_valid_for_move_constructed_range<Rng> {};
	}
}

