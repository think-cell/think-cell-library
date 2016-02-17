//-----------------------------------------------------------------------------------------------------------------------------
// think-cell public library
// Copyright (C) 2016 think-cell Software GmbH
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
#include "reverse_adaptor.h"
#include "array.h"

#include <boost/optional.hpp>
#include <boost/range/begin.hpp>
#include <boost/range/end.hpp>

#include <boost/variant.hpp> // needed for parameter_storage

namespace tc {

	//-------------------------------------------------------------------------------------------------------------------------
	// for_each	
	template<typename Rng, typename Func>
	std::enable_if_t<
		is_range_with_iterators< Rng >::value &&
		!has_index< std::remove_reference_t<Rng> >::value,
	break_or_continue > for_each(Rng&& rng, Func func) MAYTHROW {
		for(auto it = boost::begin(rng); it!= boost::end(rng); ++it) {
			RETURN_IF_BREAK( continue_if_not_break(func, *it) );
		}
		return continue_;
	}

	template<typename Rng, typename Func>
	std::enable_if_t<
		!(is_range_with_iterators< Rng >::value &&
		!has_index< std::remove_reference_t<Rng> >::value) &&
		std::is_void< decltype( std::declval<Rng>()( std::declval<Func>())) >::value,
	break_or_continue > for_each(Rng&& rng, Func func) MAYTHROW {
		std::forward<Rng>(rng)( void_generator_type_check_impl::ensure_non_break_or_continue_functor<Func>(func));
		return continue_;
	}

	template<typename Rng, typename Func>
	std::enable_if_t<
		!(is_range_with_iterators< Rng >::value &&
		!has_index< std::remove_reference_t<Rng> >::value) &&
		std::is_same< break_or_continue, decltype( std::declval<Rng>()( std::declval<Func>())) >::value,
	break_or_continue > for_each(Rng&& rng, Func&& func) MAYTHROW {
		return std::forward<Rng>(rng)( std::forward<Func>(func));
	}

	/////////////////////////////////////////////////////
	// for_each_may_remove_current

	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng, typename Func >
	break_or_continue for_each_may_remove_current(Rng&& rng, Func func) MAYTHROW {
		static_assert( !tc::is_vector< std::remove_reference_t<Rng> >::value, "" ); // tc::vector is among the types that cannot be used with for_each_may_remove_current
		auto it=boost::begin(rng);
		auto const itEnd=boost::end(rng);
		while( it!=itEnd ) {
			if( break_==continue_if_not_break(func, *it++) ) return break_;
		}
		return continue_;
	}

	/////////////////////////////////////////////////////
	// for_each_adjacent_tuple<2>

	namespace iterator_cache_adl_barrier {
		template<typename It>
		struct iterator_cache final : tc::nonmovable /*m_ref may contain pointer into m_it*/ {
		private:
			It m_it;
			tc::reference_or_value< typename std::iterator_traits<It>::reference > m_ref;

		public:
			iterator_cache(It it) noexcept
				: m_it(tc_move(it))
				, m_ref(*m_it,tc::aggregate_tag())
			{}

			iterator_cache& operator=(It it) & noexcept {
				m_it=tc_move(it);
				m_ref.aggregate(*m_it);
				return *this;
			}

			auto operator*() const & noexcept return_decltype( *m_ref )
			auto operator*() && noexcept return_decltype_rvalue_by_ref( *tc_move(m_ref) )
			auto operator*() const && noexcept = delete;
		};
	}
	using iterator_cache_adl_barrier::iterator_cache;


	
	template< typename Rng, typename Func, int... i >
	tc::break_or_continue for_each_adjacent_tuple_impl(Rng&& rng, Func func, std::integer_sequence<int, i...>) MAYTHROW {
		constexpr int N= sizeof...(i)+1;
		if (tc::size_bounded(rng, N)<N) {
			return continue_;
		} else {
			auto const itEnd = boost::end(rng);
			auto it = boost::begin(rng);
			tc::array<tc::iterator_cache< typename boost::range_iterator<std::remove_reference_t<Rng>>::type >, N> ait(tc::func_tag(), [&](std::size_t) { return it++; });

			for (;;) {
				for (int n = 0; n<N; ++n) {
					if (it == itEnd) {
						return continue_if_not_break(func, *tc_move_always(ait[n]), *tc_move_always(ait[(n + i + 1) % N])...);
					}
					RETURN_IF_BREAK(continue_if_not_break(func, *tc_move_always(ait[n]), *ait[(n + i + 1) % N]...));
					ait[n] = it;
					++it;
				}
			}
		}
	}

	template< int N, typename Rng, typename Func, std::enable_if_t< is_range_with_iterators<Rng>::value >* =nullptr >
	tc::break_or_continue for_each_adjacent_tuple(Rng&& rng, Func func) MAYTHROW {
		return for_each_adjacent_tuple_impl(std::forward<Rng>(rng), std::forward<Func>(func), std::make_integer_sequence<int,N-1>());
	}

	template< typename Rng, typename Func >
	tc::break_or_continue for_each_ordered_pair(Rng const& rng, Func func) MAYTHROW {
		auto const itEndRng = boost::end(rng);
		for(auto itEnd = boost::begin(rng); itEnd != itEndRng; ++itEnd) {
			tc::reference_or_value<typename boost::range_reference<Rng const>::type> ref(*itEnd, tc::aggregate_tag());
			
			RETURN_IF_BREAK(
				tc::for_each(
					tc::take(rng, itEnd),
					std::bind(func, std::placeholders::_1, std::ref(*ref))
				)
			);
		}
		return tc::continue_;
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
		explicit operator bool() const noexcept {
			return m_variant.which();
		}
		T const& operator*() noexcept {
			return boost::apply_visitor( FnDerefence(), m_variant );
		}
	private:
		struct empty {};
		struct FnDerefence final : boost::static_visitor<T const&> {
			T const& operator()(empty) const noexcept {
				_ASSERTFALSE;
				return *boost::implicit_cast<T const*>(nullptr);
			}
			T const& operator()(T const* p) const noexcept {
				return *p;
			}
			T const& operator()(T const& t) const noexcept {
				return t;
			}
		};
		boost::variant<empty, T const*, T> m_variant;
		static_assert( tc::is_decayed< T >::value, "" );
	};

	namespace for_each_adjacent_pair_adl_barrier {
		template<typename T, typename Func>
		struct Fn final {
			Fn(Func& func) noexcept
				: m_func(func)
			{}
			break_or_continue operator()(T const& t) MAYTHROW {
				if( m_param && break_==continue_if_not_break(m_func, *m_param, t) ) {
					return break_;
				}
				m_param = t;
				return continue_;
			}
			break_or_continue operator()(T&& t) MAYTHROW {
				if( m_param && break_==continue_if_not_break(m_func, *m_param, t) ) {
					return break_;
				}
				m_param = tc_move(t);
				return continue_;
			}
		private:
			Func& m_func;
			parameter_storage<T> m_param;
		};
	}

	template<typename T, typename Rng, typename Func>
	std::enable_if_t< !is_range_with_iterators<Rng>::value,
	break_or_continue > for_each_adjacent_pair(Rng const& rng, Func func) MAYTHROW {
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

			template< typename S >
			std::enable_if_t<
				std::is_same<
					std::result_of_t<AccuOp(T&, S &&)>,
					break_or_continue
				>::value,
				break_or_continue
			>
			operator()(S&& s) const MAYTHROW {
				return (*m_paccuop)( *m_pt, std::forward<S>(s) );
			}

			template< typename S >
			std::enable_if_t<
				!std::is_same<
					std::result_of_t<AccuOp(T&, S &&)>,
					break_or_continue
				>::value
			>
			operator()(S&& s) const MAYTHROW {
				(*m_paccuop)( *m_pt, std::forward<S>(s) );
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
			:  m_t(t), m_accuop(accuop)
			{}
			template< typename S >
			break_or_continue operator()( S&& s ) MAYTHROW {
				if( m_t ) {
					return continue_if_not_break( m_accuop, *m_t, std::forward<S>(s) );
				} else {
					m_t=std::forward<S>(s);
					return continue_;
				}
			}
		};
	}
	using accumulator_with_front_adl_barrier::accumulator_with_front;

	template< typename Rng, typename AccuOp >
	boost::optional< typename tc::range_value< std::remove_reference_t<Rng> >::type > accumulate_with_front(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		return accumulate_with_front2< typename tc::range_value< std::remove_reference_t<Rng> >::type >(std::forward<Rng>(rng),std::forward<AccuOp>(accuop));
	}


	template<typename Value, typename AccuOp>
	auto make_accumulator_with_front(boost::optional<Value>& value, AccuOp&& accumulate) noexcept
		return_ctor(accumulator_with_front<Value BOOST_PP_COMMA() AccuOp>, (value, std::forward<AccuOp>(accumulate)))

	template< typename T, typename Rng, typename AccuOp >
	boost::optional<T> accumulate_with_front2(Rng&& rng, AccuOp&& accuop) MAYTHROW {
		static_assert(tc::is_decayed< T >::value,"");
		boost::optional<T> t;
		tc::for_each(std::forward<Rng>(rng), tc::make_accumulator_with_front(t,std::forward<AccuOp>(accuop)));
		return t;
	}
}