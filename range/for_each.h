#pragma once

#include "range_defines.h"

#include "break_or_continue.h"
#include "index_range.h"
#include "meta.h"
#include "empty.h"

#include <vector>
#include <boost/optional.hpp>
#include <boost/range/begin.hpp>
#include <boost/range/end.hpp>

#include <boost/variant.hpp> // needed for parameter_storage

namespace RANGE_PROPOSAL_NAMESPACE {

	//-------------------------------------------------------------------------------------------------------------------------
	// for_each	
	template<typename Rng, typename Func>
	typename std::enable_if<
		is_range_with_iterators< Rng >::value &&
		!has_index< std::decay_t<Rng> >::value,
	break_or_continue >::type for_each(Rng&& rng, Func func) {
		for(auto it = boost::begin(rng); it!= boost::end(rng); ++it) {
			RETURN_IF_BREAK( continue_if_not_break(func, *it) );
		}
		return continue_;
	}

	template<typename Rng, typename Func>
	typename std::enable_if<
		!(is_range_with_iterators< Rng >::value &&
		!has_index< std::decay_t<Rng> >::value) &&
		std::is_void< decltype( std::declval<Rng>()( std::declval<Func>())) >::value,
	break_or_continue >::type for_each(Rng&& rng, Func func) {
		std::forward<Rng>(rng)( void_generator_type_check_impl::ensure_non_break_or_continue_functor<Func>(func));
		return continue_;
	}

	template<typename Rng, typename Func>
	typename std::enable_if<
		!(is_range_with_iterators< Rng >::value &&
		!has_index< std::decay_t<Rng> >::value) &&
		std::is_same< break_or_continue, decltype( std::declval<Rng>()( std::declval<Func>())) >::value,
	break_or_continue >::type for_each(Rng&& rng, Func&& func) {
		return std::forward<Rng>(rng)( std::forward<Func>(func));
	}

	template< typename Rng, typename Func >
	break_or_continue reverse_for_each(Rng&& rng, Func func) {
		auto const itBegin=boost::begin(rng);
		auto itEnd=boost::end(rng);
		while( itEnd!=itBegin ) {
			--itEnd;
			if( break_==continue_if_not_break(func, *itEnd) ) return break_;
		}
		return continue_;
	}

	/////////////////////////////////////////////////////
	// for_each_may_remove_current

	template< typename T >
	struct is_vector : std::false_type {};

	template< typename Ty, typename Alloc >
	struct is_vector< std::vector<Ty, Alloc> > : std::true_type {};

	// enable_if to ensure that removal preserves iterators would be nice, but is difficult for adapted ranges.
	template< typename Rng, typename Func >
	break_or_continue for_each_may_remove_current(Rng&& rng, Func func) {
		static_assert( !is_vector< std::remove_reference_t<Rng> >::value, "" ); // std::vector is among the types that cannot be used with for_each_may_remove_current
		auto it=boost::begin(rng);
		auto const itEnd=boost::end(rng);
		while( it!=itEnd ) {
			if( break_==continue_if_not_break(func, *it++) ) return break_;
		}
		return continue_;
	}

	template< typename Rng, typename Func >
	break_or_continue reverse_for_each_may_remove_current(Rng&& rng, Func func) {
		auto const itBegin=boost::begin(rng);
		auto it=boost::end(rng);
		if( it!=itBegin ) {
			--it;
			while( it!=itBegin ) {
				if( break_==continue_if_not_break(func, *it--) ) return break_;
			}
			return continue_if_not_break(func, *it);
		}
		return continue_;
	}

	/////////////////////////////////////////////////////
	// for_each_adjacent_pair

	template< typename Rng, typename Func, std::enable_if_t< is_range_with_iterators<Rng>::value >* =nullptr >
	tc::break_or_continue for_each_adjacent_pair(Rng&& rng, Func func) {
		if( 2 <= tc::distance_bounded(rng,2) ) {
			auto const itEnd = boost::end(rng);
			auto it = boost::begin(rng);
			typename boost::range_iterator<std::remove_reference_t<Rng>>::type ait[2] = {it, ++it}; // note: evaluation order for tc::array<,3>(...) arguments is wrong in VC12
			using RefType = tc::reference_or_value<typename boost::range_reference<std::remove_reference_t<Rng>>::type>;
			RefType aref[2] = {RefType{*ait[0], aggregate_tag{}}, RefType{*ait[1], aggregate_tag{}}}; // use transform_tag{} ctor as soon as ait is a tc::array

			int n = 0;
			for (;;) {
				++it;
				if (itEnd == it) {
					return continue_if_not_break(func, aref[n].last_access(), aref[(n+1)%2].last_access());
				} else {
					RETURN_IF_BREAK(continue_if_not_break(func, aref[n].last_access(), *tc::make_const(aref[(n+1)%2])) );
				}

				ait[n] = it;
				tc::renew(aref[n],*it, aggregate_tag{});
				n = (n+1)%2;
			}
		}
		return continue_;
	}

	template< typename Rng, typename Func >
	tc::break_or_continue for_each_adjacent_triple(Rng&& rng, Func func) {
		if( 3 <= tc::distance_bounded(rng,3) ) {
			auto const itEnd = boost::end(rng);
			auto it = boost::begin(rng);
			typename boost::range_iterator<std::remove_reference_t<Rng>>::type ait[3] = {it, ++it, ++it}; // note: evaluation order for tc::array<,3>(...) arguments is wrong in VC12
			using RefType = tc::reference_or_value<typename boost::range_reference<std::remove_reference_t<Rng>>::type>;
			RefType aref[3] = {RefType{*ait[0], aggregate_tag{}}, RefType{*ait[1], aggregate_tag{}}, RefType{*ait[2], aggregate_tag{}}}; // use transform_tag{} ctor as soon as ait is a tc::array

			int n = 0;
			for (;;) {
				++it;
				if (itEnd == it) {
					return continue_if_not_break(func, aref[n].last_access(), aref[(n+1)%3].last_access(), aref[(n+2)%3].last_access());
				} else {
					RETURN_IF_BREAK(continue_if_not_break(func, aref[n].last_access(), *tc::make_const(aref[(n+1)%3]), *tc::make_const(aref[(n+2)%3])) );
				}

				ait[n] = it;
				tc::renew(aref[n],*it, aggregate_tag{});
				n = (n+1)%3;
			}
		}
		return continue_;
	}

	template< typename Rng, typename Func >
	tc::break_or_continue for_each_ordered_pair(Rng const& rng, Func func) {
		auto const itEndRng = boost::end(rng);
		for(auto itEnd = boost::begin(rng); itEnd != itEndRng; ++itEnd) {
			using RefType = tc::reference_or_value<typename boost::range_reference<Rng const>::type>;
			RefType ref(*itEnd, tc::aggregate_tag());
			
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
	struct parameter_storage {
		parameter_storage& operator=(T const& t) {
			m_variant = std::addressof(t);
			return *this;
		}
		parameter_storage& operator=(T&& t) {
			m_variant = tc_move(t);
			return *this;
		}
		explicit operator bool() const {
			return m_variant.which();
		}
		T const& operator*() {
			return boost::apply_visitor( FnDerefence(), m_variant );
		}
	private:
		struct empty {};
		struct FnDerefence : boost::static_visitor<T const&> {
			T const& operator()(empty) const {
				_ASSERTFALSE;
				return *boost::implicit_cast<T const*>(nullptr);
			}
			T const& operator()(T const* p) const {
				return *p;
			}
			T const& operator()(T const& t) const {
				return t;
			}
		};
		boost::variant<empty, T const*, T> m_variant;
		static_assert( tc::is_decayed< T >::value, "" );
	};

	namespace for_each_adjacent_pair_adl_barrier {
		template<typename T, typename Func>
		struct Fn {
			Fn(Func& func)
				: m_func(func)
			{}
			break_or_continue operator()(T const& t) {
				if( m_param && break_==continue_if_not_break(m_func, *m_param, t) ) {
					return break_;
				}
				m_param = t;
				return continue_;
			}
			break_or_continue operator()(T&& t) {
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
	typename std::enable_if< !is_range_with_iterators<Rng>::value,
	break_or_continue >::type for_each_adjacent_pair(Rng const& rng, Func func) {
		return tc::for_each( rng, for_each_adjacent_pair_adl_barrier::Fn<T, Func>(func) );
	}

	/////////////////////////////////////////////////////
	// accumulate

	namespace accumulate_adl_barrier {
		template< typename T, typename AccuOp >
		struct Fn {
			T * m_pt;
			AccuOp * m_paccuop;
			Fn( T & t, AccuOp & accuop )
			:  m_pt(std::addressof(t)), m_paccuop(std::addressof(accuop))
			{}

			template< typename S >
			typename std::enable_if<
				std::is_same<
					tc::result_of_t<AccuOp(T&, S &&)>,
					break_or_continue
				>::value,
				break_or_continue
			>::type
			operator()(S&& s) const {
				return (*m_paccuop)( *m_pt, std::forward<S>(s) );
			}

			template< typename S >
			typename std::enable_if<
				!std::is_same<
					tc::result_of_t<AccuOp(T&, S &&)>,
					break_or_continue
				>::value
			>::type
			operator()(S&& s) const {
				(*m_paccuop)( *m_pt, std::forward<S>(s) );
			}
		};
	}

	template< typename Rng, typename T, typename AccuOp >
	T accumulate(Rng&& rng, T t, AccuOp accuop) {
		tc::for_each(std::forward<Rng>(rng), accumulate_adl_barrier::Fn<T,AccuOp>(t,accuop));
		return t;
	}

	namespace accumulate_with_front_adl_barrier {
		template< typename T, typename AccuOp >
		struct Fn {
			boost::optional<T> & m_t;
			AccuOp & m_accuop;
			Fn( boost::optional<T> & t, AccuOp & accuop )
			:  m_t(t), m_accuop(accuop)
			{}
			template< typename S >
			break_or_continue operator()( S&& s ) {
				if( m_t ) {
					return continue_if_not_break( m_accuop, *m_t, std::forward<S>(s) );
				} else {
					m_t=std::forward<S>(s);
					return continue_;
				}
			}
		};
	}

	template< typename Rng, typename AccuOp >
	boost::optional< typename tc::range_value< std::remove_reference_t<Rng> >::type > accumulate_with_front(Rng&& rng, AccuOp&& accuop) {
		return accumulate_with_front2< typename tc::range_value< std::remove_reference_t<Rng> >::type >(std::forward<Rng>(rng),std::forward<AccuOp>(accuop));
	}

	template< typename T, typename Rng, typename AccuOp >
	boost::optional<T> accumulate_with_front2(Rng&& rng, AccuOp accuop) {
		static_assert(tc::is_decayed< T >::value,"");
		boost::optional<T> t;
		tc::for_each(std::forward<Rng>(rng), accumulate_with_front_adl_barrier::Fn<T,AccuOp>(t,accuop));
		return t;
	}
}
