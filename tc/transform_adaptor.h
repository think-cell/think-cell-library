
// think-cell public library
//
// Copyright (C) 2016-2020 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "range_defines.h"
#include "range_fwd.h"

#include "range_adaptor.h"
#include "subrange.h"
#include "meta.h"

#include "tc_move.h"
#include "transform.h"

namespace tc {
	namespace no_adl {
		struct transform_adaptor_access final {
			template< typename Func, typename Rng, bool bHasIterator >
			static Func&& get_func(transform_adaptor<Func,Rng,bHasIterator>&& rng) noexcept {
				return tc_move(rng).m_func;
			}
		};


		template< typename Func, typename Rng >
		struct [[nodiscard]] transform_adaptor<Func,Rng,false> : public range_adaptor<transform_adaptor<Func,Rng>, Rng > {
		private:
			using base_ = range_adaptor<transform_adaptor<Func,Rng>, Rng >;

		protected:
			using range_adaptor = base_;

			static_assert( tc::is_decayed< Func >::value );
			Func m_func;

		private:
			template<typename, typename>
			friend struct no_adl::range_adaptor_access;
			friend struct transform_adaptor_access;
			template< typename Apply, typename Arg>
			auto apply(Apply&& apply, Arg&& arg) const& return_decltype_MAYTHROW(
				// apply is called from a tc::invoke context, so do not tc::invoke again.
				tc::continue_if_not_break(std::forward<Apply>(apply), tc::invoke(m_func, std::forward<Arg>(arg)))
			)

		public:
			// other ctors
			template< typename RngOther, typename FuncOther >
			explicit transform_adaptor( RngOther&& rng, FuncOther&& func ) noexcept
				: base_(aggregate_tag, std::forward<RngOther>(rng))
				, m_func(std::forward<FuncOther>(func))
			{}

			template< ENABLE_SFINAE, std::enable_if_t<tc::has_size<SFINAE_TYPE(Rng)>::value>* = nullptr >
			constexpr auto size() const& noexcept {
				return tc::size_raw(*this->m_baserng);
			}
		};

		template< typename Func, typename Rng >
		struct [[nodiscard]] transform_adaptor<Func,Rng,true> : public transform_adaptor<Func,Rng,false> {
        private:
			using base_ = transform_adaptor<Func,Rng,false>;
			using range_adaptor = typename base_::range_adaptor; // using not accepted by MSVC
			template<typename, typename>
			friend struct no_adl::range_adaptor_access;
		public:
			using typename base_::index;

			// ctor from range and functor
			template< typename RngOther, typename FuncOther >
			explicit transform_adaptor( RngOther&& rng, FuncOther&& func ) noexcept
				: base_(std::forward<RngOther>(rng),std::forward<FuncOther>(func))
			{}

			template<ENABLE_SFINAE>
			auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(index const& idx) & MAYTHROW -> tc::transform_return_t<
				SFINAE_TYPE(Func),
				decltype(tc::invoke(std::declval<SFINAE_TYPE(Func) const&>(), std::declval<range_adaptor &>().template dereference_index<range_adaptor>(std::declval<index const&>()))),
				decltype(std::declval<range_adaptor &>().template dereference_index<range_adaptor>(std::declval<index const&>()))
			> {
				// always call operator() const, which is assumed to be thread-safe
				return tc::invoke(tc::as_const(this->m_func), this->template dereference_index<base_>(idx));
			}

			template<ENABLE_SFINAE>
			auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(index const& idx) const& MAYTHROW -> tc::transform_return_t<
				SFINAE_TYPE(Func),
				decltype(tc::invoke(std::declval<SFINAE_TYPE(Func) const&>(), std::declval<range_adaptor const&>().template dereference_index<range_adaptor>(std::declval<index const&>()))),
				decltype(std::declval<range_adaptor const&>().template dereference_index<range_adaptor>(std::declval<index const&>()))
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

	namespace replace_if_impl {
		template< typename Func, typename T >
		struct replace_if final {
		private:
			tc::decay_t<Func> m_func;
			tc::decay_t<T> m_t;
			
		public:
			replace_if(Func&& func, T&& t) noexcept
				: m_func(std::forward<Func>(func))
				, m_t(std::forward<T>(t))
			{}
			template< typename S >
			decltype(auto) operator()(S&& s) const& MAYTHROW {
				return CONDITIONAL_RVALUE_AS_REF(m_func(s),m_t,std::forward<S>(s));
			}
			friend std::true_type returns_reference_to_argument(replace_if) noexcept; // ADL
		};
	}

	template<typename Rng, typename Func, typename T>
	auto replace_if(Rng&& rng, Func func, T&& t) noexcept {
		return tc::transform( std::forward<Rng>(rng), replace_if_impl::replace_if<Func,T>(std::forward<Func>(func),std::forward<T>(t) ) );
	}

	template<typename Rng, typename S, typename T>
	auto replace(Rng&& rng, S&& s, T&& t) noexcept {
		return tc::replace_if( std::forward<Rng>(rng), [s_=tc::decay_copy(std::forward<S>(s))](auto const& _) noexcept { return tc::equal_to(_, s_); }, std::forward<T>(t) );
	}

	template <typename Rng, typename Func, typename T>
	Rng& replace_if_inplace(Rng& rng, Func func, T const& t) noexcept {
		for_each(rng, [&](auto&& v) noexcept {
			if (func(tc::as_const(v))) {
				v = t;
			}
		});
		return rng;
	}

	template<typename Rng, typename S, typename T>
	Rng& replace_inplace(Rng& rng, S&& s, T const& t) noexcept {
		return tc::replace_if_inplace( rng, [&](auto const& _) noexcept { return tc::equal_to(_, s); }, t );
	}
	
	template<typename Rng, std::enable_if_t<tc::is_instance2<transform_adaptor,std::remove_reference_t<Rng>>::value>* =nullptr >
	[[nodiscard]] decltype(auto) untransform(Rng&& rng) noexcept {
		return std::forward<Rng>(rng).base_range();
	}

	template<typename Rng, std::enable_if_t<tc::is_instance<subrange,std::remove_reference_t<Rng>>::value>* =nullptr >
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

