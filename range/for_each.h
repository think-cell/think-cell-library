//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016-2018 think-cell Software GmbH
//
// This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as 
// published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
// of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
//
// You should have received a copy of the GNU General Public License along with this program. 
// If not, see <http://www.gnu.org/licenses/>. 
//-----------------------------------------------------------------------------------------------------------------------------

#pragma once

#include "container.h" // tc::vector
#include "range_defines.h"

#include "break_or_continue.h"
#include "index_range.h"
#include "meta.h"
#include "empty.h"
#include "scope.h"
#include "size.h"
#include "utility.h"
#include "conditional.h"

#include <boost/optional.hpp>
#include <boost/range/begin.hpp>
#include <boost/range/end.hpp>

#include <boost/variant.hpp> // needed for parameter_storage

namespace tc {

	//-------------------------------------------------------------------------------------------------------------------------
	// for_each	

	// Primary for_each dispatcher
	template<typename Rng, typename Func>
	auto for_each(Rng&& rng, Func&& func) MAYTHROW {
		return for_each_impl(std::forward<Rng>(rng), std::forward<Func>(func), tc::type_list<std::conditional_t<std::is_array<std::remove_reference_t<Rng>>::value, tc::remove_cvref_t<Rng>, tc::decay_t<Rng>>>());
	}

	// Non-index iterator-based ranges
	template<typename Rng, typename Func, typename RngDecayed, 
		std::enable_if_t<
			tc::is_range_with_iterators< RngDecayed >::value && !tc::has_index< RngDecayed >::value
		>* = nullptr
	>
	auto for_each_impl(Rng&& rng, Func func, tc::type_list<RngDecayed>) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *boost::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
		for(auto it = boost::begin(rng); it!= boost::end(rng); ++it) {
			RETURN_IF_BREAK( tc::continue_if_not_break(func, *it) );
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	// Non-breakable iteration over index ranges
	template<typename Rng, typename Func, typename RngDecayed, 
		std::enable_if_t<
			!(tc::is_range_with_iterators< RngDecayed >::value && !tc::has_index< RngDecayed >::value) && 
			(std::is_void<decltype(std::declval<Rng>()(std::declval<Func>())) >::value ||
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), INTEGRAL_CONSTANT(tc::continue_)>::value)
		>* = nullptr
	>
	INTEGRAL_CONSTANT(tc::continue_) for_each_impl(Rng&& rng, Func&& func, tc::type_list<RngDecayed>) MAYTHROW {
		std::forward<Rng>(rng)( void_generator_type_check_impl::ensure_non_breaking_functor<Func>(std::forward<Func>(func)) );
		return {};
	}

	// Always-breaking iteration over index ranges
	template<typename Rng, typename Func, typename RngDecayed, 
		std::enable_if_t<
			!(tc::is_range_with_iterators< RngDecayed >::value && !tc::has_index< RngDecayed >::value) && 
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), INTEGRAL_CONSTANT(tc::break_)>::value
		>* = nullptr
	>
	INTEGRAL_CONSTANT(tc::break_) for_each_impl(Rng&& rng, Func&& func, tc::type_list<RngDecayed>) MAYTHROW {
		std::forward<Rng>(rng)( void_generator_type_check_impl::ensure_always_breaking_functor<Func>(std::forward<Func>(func)) );
		return {};
	}

	// Breakable iteration over index ranges
	template<typename Rng, typename Func, typename RngDecayed,
		std::enable_if_t<
			!(tc::is_range_with_iterators< RngDecayed >::value && !tc::has_index< RngDecayed >::value) && 
			std::is_same<decltype(std::declval<Rng>()(std::declval<Func>())), tc::break_or_continue>::value
		>* = nullptr
	>
	auto for_each_impl(Rng&& rng, Func&& func, tc::type_list<RngDecayed>) MAYTHROW {
		return std::forward<Rng>(rng)( std::forward<Func>(func) );
	}

	// Enumset
	template<typename Enumset, typename Func, typename Enum>
	auto for_each_impl(Enumset&& enumset, Func func, tc::type_list<tc::enumset<Enum>>) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, std::declval<Enum&>())), INTEGRAL_CONSTANT(tc::continue_)> {
		for (Enum e = tc::contiguous_enum<Enum>::begin(); e != tc::contiguous_enum<Enum>::end(); ++e) {
			if ((enumset&e)) {
				RETURN_IF_BREAK(tc::continue_if_not_break(func, e));
			}
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	// Integer sequences
	template<typename IntSequence, typename Func, typename TIndex, TIndex... Is>
	auto for_each_impl(IntSequence&&, Func func, tc::type_list<std::integer_sequence<TIndex, Is...>>) MAYTHROW {
		tc::common_type_t<
			INTEGRAL_CONSTANT(tc::continue_),
			decltype(tc::continue_if_not_break(func, std::integral_constant<TIndex, Is>()))...
		> breakorcontinue = INTEGRAL_CONSTANT(tc::continue_)();

		using swallow = std::initializer_list<bool>;
		swallow{(tc::continue_ == breakorcontinue && ((breakorcontinue = tc::continue_if_not_break(func, std::integral_constant<TIndex, Is>())), true))...};

		return breakorcontinue;
	}

	// Tuples
	template<typename Tuple, typename Func, typename... Ts>
	auto for_each_impl(Tuple&& tuple, Func func, tc::type_list<std::tuple<Ts...>>) MAYTHROW {
		return for_each(
			std::make_index_sequence<std::tuple_size<std::remove_reference_t<Tuple>>::value>(),
			[&](auto nconstIndex) MAYTHROW {
				return func(std::get<nconstIndex()>(std::forward<Tuple>(tuple)));
			}
		);
	}

	DEFINE_FN(for_each);

	/////////////////////////////////////////////////////
	// for_each_may_remove_current

	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng, typename Func >
	auto for_each_may_remove_current(Rng&& rng, Func func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *boost::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
		static_assert( !tc::range_filter_by_move_element< std::remove_reference_t<Rng> >::value );
		auto it=boost::begin(rng);
		auto const itEnd=boost::end(rng);
		while( it!=itEnd ) {
			RETURN_IF_BREAK(tc::continue_if_not_break(func, *it++));
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	DEFINE_FN(for_each_may_remove_current);

	/////////////////////////////////////////////////////
	// for_each_ordered_pair

	template< typename Rng, typename Func >
	auto for_each_ordered_pair(Rng const& rng, Func func) MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(func, *boost::begin(rng), *boost::begin(rng))), INTEGRAL_CONSTANT(tc::continue_)> {
		auto const itEndRng = boost::end(rng);
		for(auto itEnd = boost::begin(rng); itEnd != itEndRng; ++itEnd) {
			tc::reference_or_value<tc::range_reference_t<Rng const>> ref(aggregate_tag(), *itEnd);
			
			RETURN_IF_BREAK(
				tc::for_each(
					tc::take(rng, itEnd),
					[&](auto const& _) MAYTHROW { return func(_, *ref); }
				)
			);
		}
		return INTEGRAL_CONSTANT(tc::continue_)();
	}

	template<typename T>
	struct parameter_storage final {
		parameter_storage& operator=(T const& t) & noexcept {
			m_variant = std::addressof(t);
			return *this;
		}
		parameter_storage& operator=(T&& t) & noexcept {
			m_variant = tc_move(t);
			return *this;
		}
		explicit operator bool() const& noexcept {
			return m_variant.which();
		}
		T const& operator*() & noexcept {
			return boost::apply_visitor( FnDerefence(), m_variant );
		}
	private:
		struct empty {};
		struct FnDerefence final : boost::static_visitor<T const&> {
			T const& operator()(empty) const& noexcept {
				_ASSERTFALSE;
				return *boost::implicit_cast<T const*>(nullptr);
			}
			T const& operator()(T const* p) const& noexcept {
				return *p;
			}
			T const& operator()(T const& t) const& noexcept {
				return t;
			}
		};
		boost::variant<empty, T const*, T> m_variant;
		static_assert( tc::is_decayed< T >::value );
	};

	namespace for_each_adjacent_pair_adl_barrier {
		template<typename T, typename Func>
		struct Fn final {
		private:
			Func& m_func;
			parameter_storage<T> m_param;
		public:
			Fn(Func& func) noexcept
				: m_func(func)
			{}

			template<typename U>
			auto operator()(U&& u) & MAYTHROW -> tc::common_type_t<decltype(tc::continue_if_not_break(m_func, *m_param, u)), INTEGRAL_CONSTANT(tc::continue_)> {
				if (m_param) {
					RETURN_IF_BREAK(tc::continue_if_not_break(m_func, *m_param, u));
				}
				m_param = std::forward<U>(u);
				return INTEGRAL_CONSTANT(tc::continue_)();
			}
		};
	}

	template<typename T, typename Rng, typename Func, std::enable_if_t<!is_range_with_iterators<Rng>::value>* = nullptr>
	auto for_each_adjacent_pair(Rng const& rng, Func func) MAYTHROW {
		return tc::for_each( rng, for_each_adjacent_pair_adl_barrier::Fn<T, Func>(func) );
	}

	/////////////////////////////////////////////////////
	// accumulate

	namespace accumulate_adl_barrier {
		template< typename T, typename AccuOp >
		struct Fn final {
			T * m_pt;
			AccuOp * m_paccuop;
			Fn( T & t, AccuOp & accuop ) noexcept
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
		tc::for_each(std::forward<Rng>(rng), accumulate_adl_barrier::Fn<T,AccuOp>(t,accuop));
		return t;
	}

	namespace accumulator_with_front_adl_barrier {
		template< typename T, typename AccuOp >
		struct accumulator_with_front final {
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
	using accumulator_with_front_adl_barrier::accumulator_with_front;

	template<typename Value, typename AccuOp>
	auto make_accumulator_with_front(boost::optional<Value>& value, AccuOp&& accumulate) noexcept
		return_ctor(accumulator_with_front<Value BOOST_PP_COMMA() AccuOp>, (value, std::forward<AccuOp>(accumulate)))

	template< typename T, typename Rng, typename AccuOp >
	boost::optional<T> accumulate_with_front2(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		static_assert(tc::is_decayed< T >::value);
		boost::optional<T> t;
		tc::for_each(std::forward<Rng>(rng), tc::make_accumulator_with_front(t,std::forward<AccuOp>(accuop)));
		return t;
	}

	template< typename Rng, typename AccuOp >
	boost::optional< typename tc::range_value< std::remove_reference_t<Rng> >::type > accumulate_with_front(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		return accumulate_with_front2< typename tc::range_value< std::remove_reference_t<Rng> >::type >(std::forward<Rng>(rng),std::forward<AccuOp>(accuop));
	}

	/////////////////////////////////////////////////////
	// partial_sum

	namespace partial_sum_adl_barrier {
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

			auto size() const& noexcept return_decltype(
				tc::size(*m_baserng)
			)
		};
	}
	using partial_sum_adl_barrier::partial_sum_adaptor;

	namespace range_reference_adl_barrier {
		template <typename Rng, typename T, typename AccuOp>
		struct range_reference<partial_sum_adaptor<Rng,T,AccuOp>> {
			using type = std::add_lvalue_reference_t<tc::decay_t<T>>;
		};

		template <typename Rng, typename T, typename AccuOp>
		struct range_reference<partial_sum_adaptor<Rng,T,AccuOp> const> :  range_reference<partial_sum_adaptor<Rng,T,AccuOp>> {};
	}


	template <typename Rng, typename T, typename AccuOp>
	auto partial_sum(Rng&& rng, T&& init, AccuOp&& accuop) noexcept return_ctor(
		partial_sum_adaptor<view_by_value_t<Rng> BOOST_PP_COMMA() T BOOST_PP_COMMA() AccuOp >, (std::forward<Rng>(rng), std::forward<T>(init), std::forward<AccuOp>(accuop))
	)
}
