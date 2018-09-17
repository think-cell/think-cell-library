
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
#include "sub_range.h"
#include "meta.h"

#include "tc_move.h"
#include "transform.h"

namespace tc {
	namespace transform_adaptor_impl {
		struct transform_adaptor_access final {
			template< typename Func, typename Rng, bool bHasIterator >
			static Func&& get_func(transform_adaptor<Func,Rng,bHasIterator>&& rng) noexcept {
				return tc_move(rng).m_func;
			}
		};


		template< typename Func, typename Rng >
		struct transform_adaptor<Func,Rng,false> : public range_adaptor<transform_adaptor<Func,Rng>, Rng > {
		private:
			using base_ = range_adaptor<transform_adaptor<Func,Rng>, Rng >;

		protected:
			using range_adaptor = base_;

			static_assert( tc::is_decayed< Func >::value );
			Func m_func;

		private:
			friend struct no_adl::range_adaptor_access;
			friend struct transform_adaptor_access;
			template< typename Apply, typename... Args>
			auto apply(Apply&& apply, Args&& ... args) const& MAYTHROW return_decltype (
				tc::continue_if_not_break(std::forward<Apply>(apply), m_func(std::forward<Args>(args)...))
			)

		public:
			// other ctors
			template< typename RngOther, typename FuncOther >
			explicit transform_adaptor( RngOther&& rng, FuncOther&& func ) noexcept
				: base_(aggregate_tag(), std::forward<RngOther>(rng))
				, m_func(std::forward<FuncOther>(func))
			{}

			template< typename Rng2 = Rng, std::enable_if_t<tc::has_size<Rng2>::value>* = nullptr >
			constexpr auto size() const& noexcept {
				return tc::size_raw(*this->m_baserng);
			}
		};

		template< typename Func, typename Rng >
		struct transform_adaptor<Func,Rng,true> : public transform_adaptor<Func,Rng,false> {
        private:
			using base_ = transform_adaptor<Func,Rng,false>;
			using range_adaptor = typename base_::range_adaptor; // using not accepted by MSVC

			friend struct no_adl::range_adaptor_access;
		public:
			using typename base_::index;

			// ctor from range and functor
			template< typename RngOther, typename FuncOther >
			explicit transform_adaptor( RngOther&& rng, FuncOther&& func ) noexcept
				: base_(std::forward<RngOther>(rng),std::forward<FuncOther>(func))
			{}

			template<typename Func2=Func/*enable SFINAE*/>
			auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(index const& idx) & MAYTHROW -> tc::transform_return_t<
				Func2,
				decltype(std::declval<Func2 const&>()(std::declval<range_adaptor &>().STATIC_VIRTUAL_METHOD_NAME(dereference_index)(std::declval<index const&>()))),
				decltype(std::declval<range_adaptor &>().STATIC_VIRTUAL_METHOD_NAME(dereference_index)(std::declval<index const&>()))
			> {
				// always call operator() const, which is assumed to be thread-safe
				return tc::as_const(this->m_func)(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx));
			}

			template<typename Func2=Func/*enable SFINAE*/>
			auto STATIC_VIRTUAL_METHOD_NAME(dereference_index)(index const& idx) const& MAYTHROW -> tc::transform_return_t<
				Func2,
				decltype(std::declval<Func2 const&>()(std::declval<range_adaptor const&>().STATIC_VIRTUAL_METHOD_NAME(dereference_index)(std::declval<index const&>()))),
				decltype(std::declval<range_adaptor const&>().STATIC_VIRTUAL_METHOD_NAME(dereference_index)(std::declval<index const&>()))
			> {
				// always call operator() const, which is assumed to be thread-safe
				return tc::as_const(this->m_func)(base_::STATIC_VIRTUAL_METHOD_NAME(dereference_index)(idx));
			}

			auto border_base_index(index const& idx) const& noexcept {
				return idx;
			}

			auto element_base_index(index const& idx) const& noexcept {
				return idx;
			}
		};
	}

	namespace no_adl {
		template<typename Func, typename Rng, bool bConst>
		struct constexpr_size<tc::transform_adaptor<Func,Rng,bConst>> : tc::constexpr_size<tc::remove_cvref_t<Rng>> {};

		template< typename Func, typename Rng, bool bConst >
		struct range_reference_transform_adaptor {
			using type = decltype(
				std::declval<Func&>()(
					std::declval<
						tc::range_reference_t<
							tc::apply_if_t<
								bConst,
								std::add_const,
								range_adaptor<transform_adaptor<Func,Rng>, Rng >
							>
						>
					>()
				)
			);
		};


		template< typename Func, typename Rng >
		struct range_reference<transform_adaptor<Func, Rng, false> > : range_reference_transform_adaptor<Func, Rng, false> {};

		template< typename Func, typename Rng >
		struct range_reference<transform_adaptor<Func, Rng, false> const> : range_reference_transform_adaptor<Func, Rng, true> {};
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
			auto operator()(S&& s) const& MAYTHROW ->decltype(auto) {
				return CONDITIONAL(m_func(s),m_t,std::forward<S>(s));
			}
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
		for_each(rng, [&](decltype(*tc::begin(rng)) v) noexcept {
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
	decltype(auto) untransform(Rng&& rng) noexcept {
		return std::forward<Rng>(rng).base_range();
	}

	template<typename Rng, std::enable_if_t<tc::is_instance<sub_range,std::remove_reference_t<Rng>>::value>* =nullptr >
	decltype(auto) untransform(Rng&& rng) noexcept {
		return tc::slice(untransform(std::forward<Rng>(rng).base_range()), rng.begin_index(), rng.end_index());
	}
}

