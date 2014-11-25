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
	template< typename Rng, typename Func >
	break_or_continue for_each(Rng && rng, Func && func) {
		return continue_if_void( ensure_index_range(std::forward<Rng>(rng)), std::forward<Func>(func));
	}

	template< typename Rng, typename Func >
	break_or_continue reverse_for_each(Rng && rng, Func && func) {
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type const itBegin=boost::begin(rng);
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type itEnd=boost::end(rng);
		while( itEnd!=itBegin ) {
			--itEnd;
			if( break_==continue_if_void(func, *itEnd) ) return break_;
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
	break_or_continue for_each_may_remove_current(Rng && rng, Func func) {
		static_assert( !is_vector< typename std::remove_reference<Rng>::type >::value, "" ); // std::vector is among the types that cannot be used with for_each_may_remove_current
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it=boost::begin(rng);
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type const itEnd=boost::end(rng);
		while( it!=itEnd ) {
			if( break_==continue_if_void(func, *it++) ) return break_;
		}
		return continue_;
	}

	template< typename Rng, typename Func >
	break_or_continue reverse_for_each_may_remove_current(Rng && rng, Func func) {
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type const itBegin=boost::begin(rng);
		typename boost::range_iterator< typename std::remove_reference<Rng>::type >::type it=boost::end(rng);
		if( it!=itBegin ) {
			--it;
			while( it!=itBegin ) {
				if( break_==continue_if_void(func, *it--) ) return break_;
			}
			return continue_if_void(func, *it);
		}
		return continue_;
	}

	/////////////////////////////////////////////////////
	// for_each_pair

	template< typename Rng, typename Func >
	typename std::enable_if< is_range_with_iterators<Rng>::value,
	break_or_continue >::type for_each_pair(Rng const& rng, Func func) {
		if( !RANGE_PROPOSAL_NAMESPACE::empty(rng) ) {
			for(typename boost::range_iterator<const Rng>::type it = boost::begin(rng);;) {
				typename boost::range_iterator<const Rng>::type itNext = boost::next(it);
				if( boost::end(rng)==itNext ) break;
				if( break_==continue_if_void(func, *it, *itNext) ) return break_;
				it = tc_move(itNext);
			}
		}
		return continue_;
	}

	template<typename T>
	struct parameter_storage {
		parameter_storage& operator=(T const& t) {
			m_variant = std::addressof(t);
			return *this;
		}
		parameter_storage& operator=(T && t) {
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
				return *static_cast<T const*>(nullptr);
			}
			T const& operator()(T const* p) const {
				return *p;
			}
			T const& operator()(T const& t) const {
				return t;
			}
		};
		boost::variant<empty, T const*, T> m_variant;
		static_assert( std::is_same<T, typename std::decay<T>::type>::value, "" );
	};

	namespace for_each_pair_detail {
		template<typename T, typename Func>
		struct Fn {
		public:
			Fn(Func& func)
				: m_func(func)
			{}
			break_or_continue operator()(T const& t) {
				if( m_param && break_==continue_if_void(m_func, *m_param, t) ) {
					return break_;
				}
				m_param = t;
				return continue_;
			}
			break_or_continue operator()(T && t) {
				if( m_param && break_==continue_if_void(m_func, *m_param, t) ) {
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
	break_or_continue >::type for_each_pair(Rng const& rng, Func func) {
		return tc::for_each( rng, for_each_pair_detail::Fn<T, Func>(func) );
	}

	/////////////////////////////////////////////////////
	// accumulate

	namespace accumulate_detail {
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
					typename tc::result_of<AccuOp(T&, S &&)>::type,
					break_or_continue
				>::value,
				break_or_continue
			>::type
			operator()(S && s) const {
				return (*m_paccuop)( *m_pt, std::forward<S>(s) );
			}

			template< typename S >
			typename std::enable_if<
				!std::is_same<
					typename tc::result_of<AccuOp(T&, S &&)>::type,
					break_or_continue
				>::value
			>::type
			operator()(S && s) const {
				(*m_paccuop)( *m_pt, std::forward<S>(s) );
			}
		};
	}

	template< typename Rng, typename T, typename AccuOp >
	T accumulate( Rng && rng, T t, AccuOp accuop ) {
		ensure_index_range(std::forward<Rng>(rng))(accumulate_detail::Fn<T,AccuOp>(t,accuop));
		return t;
	}

	namespace accumulate_with_front_detail {
		template< typename T, typename AccuOp >
		struct Fn {
			boost::optional<T> & m_t;
			AccuOp & m_accuop;
			Fn( boost::optional<T> & t, AccuOp & accuop )
			:  m_t(t), m_accuop(accuop)
			{}
			template< typename S >
			break_or_continue operator()( S && s ) {
				if( m_t ) {
					return continue_if_void( m_accuop, *m_t, std::forward<S>(s) );
				} else {
					m_t=std::forward<S>(s);
					return continue_;
				}
			}
		};
	}

	template< typename Rng, typename AccuOp >
	boost::optional< typename boost::range_value< typename std::remove_reference<Rng>::type >::type > accumulate_with_front( Rng && rng, AccuOp accuop ) {
		typedef typename boost::range_value< typename std::remove_reference<Rng>::type >::type T;
		boost::optional<T> t;
		accumulate_with_front_detail::Fn<T,AccuOp> fn(t,accuop);
		ensure_index_range(std::forward<Rng>(rng))(fn);
		return t;
	}
}
