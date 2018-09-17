
// think-cell public library
//
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// Distributed under the Boost Software License, Version 1.0.
// See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt

#pragma once

#include "for_each.h"

namespace tc {
	/////////////////////////////////////////////////////
	// accumulate

	namespace no_adl {
		template< typename T, typename AccuOp >
		struct accumulate_fn /*final*/ {
			T * m_pt;
			AccuOp * m_paccuop;
			accumulate_fn( T & t, AccuOp & accuop ) noexcept
			:  m_pt(std::addressof(t)), m_paccuop(std::addressof(accuop))
			{}

			template<typename S>
			auto operator()(S&& s) const& MAYTHROW {
				return tc::continue_if_not_break(*m_paccuop, *m_pt, std::forward<S>(s));
			}
		};
	}

	template< typename Rng, typename T, typename AccuOp >
	T accumulate(Rng&& rng, T t, AccuOp accuop) MAYTHROW {
		tc::for_each(std::forward<Rng>(rng), no_adl::accumulate_fn<T,AccuOp>(t,accuop));
		return t;
	}

	namespace no_adl {
		template< typename T, typename AccuOp >
		struct accumulator_with_front /*final*/ {
			boost::optional<T> & m_t;

			AccuOp m_accuop;

			accumulator_with_front( boost::optional<T>& t, AccuOp&& accuop ) noexcept
				: m_t(t)
				, m_accuop(std::forward<AccuOp>(accuop))
			{}

			template< typename S >
			auto operator()( S&& s ) /* no & */ MAYTHROW {
				return CONDITIONAL(
					m_t,
					tc::continue_if_not_break( m_accuop, *m_t, std::forward<S>(s) ),
					m_t=std::forward<S>(s) BOOST_PP_COMMA() INTEGRAL_CONSTANT(tc::continue_)()
				);
			}
		};
	}

	template<typename Value, typename AccuOp>
	auto make_accumulator_with_front(boost::optional<Value>& value, AccuOp&& accumulate) noexcept
		return_ctor(no_adl::accumulator_with_front<Value BOOST_PP_COMMA() AccuOp>, (value, std::forward<AccuOp>(accumulate)))

	template< typename T, typename Rng, typename AccuOp >
	boost::optional<T> accumulate_with_front2(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		static_assert(tc::is_decayed< T >::value);
		boost::optional<T> t;
		tc::for_each(std::forward<Rng>(rng), tc::make_accumulator_with_front(t,std::forward<AccuOp>(accuop)));
		return t;
	}

	template< typename Rng, typename AccuOp >
	boost::optional< tc::range_value_t< std::remove_reference_t<Rng> > > accumulate_with_front(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		return accumulate_with_front2< tc::range_value_t< std::remove_reference_t<Rng> > >(std::forward<Rng>(rng),std::forward<AccuOp>(accuop));
	}

	/////////////////////////////////////////////////////
	// partial_sum

	namespace no_adl {
		template <typename Rng, typename T, typename AccuOp>
		struct partial_sum_adaptor {
		private:
			reference_or_value< Rng > m_baserng;
			reference_or_value< T > m_init;
			tc::decay_t<AccuOp> m_accuop;
		public:
			template <typename RngRef, typename TRef, typename AccuOpRef>
			explicit partial_sum_adaptor(RngRef&& rng, TRef&& init, AccuOpRef&& accuop) noexcept
				: m_baserng(aggregate_tag(), std::forward<RngRef>(rng))
				, m_init(aggregate_tag(), std::forward<TRef>(init))
				, m_accuop(std::forward<AccuOpRef>(accuop))
			{}

			template <typename Func>
			auto operator()(Func func) const& MAYTHROW {
				tc::decay_t<T> accu = *m_init;
				return tc::for_each( *m_baserng, [&] (auto&& arg) MAYTHROW {
					m_accuop(accu, std::forward<decltype(arg)>(arg));
					return func(accu);
				});
			}

			constexpr auto size() const& noexcept return_decltype(
				tc::size_raw(*m_baserng)
			)
		};
	}
	using no_adl::partial_sum_adaptor;

	namespace no_adl {
		template <typename Rng, typename T, typename AccuOp>
		struct range_reference<partial_sum_adaptor<Rng,T,AccuOp>> {
			using type = std::add_lvalue_reference_t<tc::decay_t<T>>;
		};

		template <typename Rng, typename T, typename AccuOp>
		struct range_reference<partial_sum_adaptor<Rng,T,AccuOp> const> :  range_reference<partial_sum_adaptor<Rng,T,AccuOp>> {};
	}


	template <typename Rng, typename T, typename AccuOp>
	auto partial_sum(Rng&& rng, T&& init, AccuOp&& accuop) noexcept return_ctor(
		partial_sum_adaptor<Rng BOOST_PP_COMMA() T BOOST_PP_COMMA() AccuOp >, (std::forward<Rng>(rng), std::forward<T>(init), std::forward<AccuOp>(accuop))
	)
}
